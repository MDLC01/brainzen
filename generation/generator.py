from enum import IntEnum

from data_types import *
from exceptions import *
from generation.brainfuck_code import *
from generation.name_manager import *
from reference import Reference
from type_checking import *
from type_checking.operations import *


class CommentLevel(IntEnum):
    ONLY_REQUIRED = 0
    BZ_CODE = 1
    BZ_CODE_EXTENDED = 2
    DETAILS = 3


class Context:
    @classmethod
    def empty(cls) -> 'Context':
        return Context({}, {})

    @classmethod
    def with_global(cls, context: 'Context') -> 'Context':
        return cls(context.namespaces.copy(), context.subroutines.copy())

    def __init__(self, namespaces: dict[str, 'Context'], subroutines: dict[str, 'SubroutineCompiler']) -> None:
        self.namespaces = namespaces
        self.subroutines = subroutines

    def get_namespace(self, reference: Reference) -> 'Context':
        if reference is None:
            return self
        parent = self.get_namespace(reference.namespace)
        if reference.identifier in parent.namespaces:
            return parent.namespaces[reference.identifier]
        raise CompilerException(f'Unable to find namespace {reference}')

    def get_subroutine(self, reference: Reference) -> 'SubroutineCompiler':
        namespace = self.get_namespace(reference.namespace)
        if reference.identifier in namespace.subroutines:
            return namespace.subroutines[reference.identifier]
        raise CompilerException(f'Unable to find subroutine {reference}')

    def with_namespace(self, identifier: str, namespace: 'Context') -> 'Context':
        namespaces = self.namespaces.copy()
        namespaces[identifier] = namespace
        subroutines = self.subroutines.copy()
        return type(self)(namespaces, subroutines)

    def with_subroutine(self, identifier: str, subroutine: 'SubroutineCompiler') -> 'Context':
        namespaces = self.namespaces.copy()
        subroutines = self.subroutines.copy()
        subroutines[identifier] = subroutine
        return type(self)(namespaces, subroutines)


class SubroutineCompiler(NameManager):
    """
    A subroutine is only allowed to change cells to the right of where it started, and it
    must set all values back to 0 after being called. It can expect cells to contain 0 by default.
    The return value of a function is defined by the position of the pointer when it ends.
    """

    def __init__(self, subroutine: TypeCheckedSubroutine, context: Context, *,
                 comment_level: int = CommentLevel.BZ_CODE) -> None:
        super().__init__(subroutine.arguments, subroutine.return_type)
        self.context = context
        self.subroutine = subroutine
        self.comment_level = comment_level
        self.brainfuck_code = BrainfuckCode()
        self.generated = False
        self.index = 0
        self._loop_start_indices: list[int] = []

    def identifier(self) -> str:
        """Return the identifier of the subroutine."""
        return self.subroutine.identifier

    def arguments(self) -> list[DataType]:
        """Return the list of the types of the arguments this subroutine accepts."""
        return [argument.type for argument in self.subroutine.arguments]

    def returns(self) -> bool:
        """Return a boolean indicating whether this subroutine returns a value (is a function)."""
        return self.subroutine.is_function()

    def return_type(self) -> DataType:
        """Return the return type of this subroutine (may be None if the subroutine is a procedure)."""
        return self.subroutine.return_type

    def arity(self) -> int:
        """Return the number of arguments this subroutine expects."""
        return len(self.subroutine.arguments)

    def _comment(self, comment: str, level: int = CommentLevel.DETAILS, *, prefix: str = '\t', end: str = '\n') -> None:
        if level <= self.comment_level:
            self.brainfuck_code.comment(comment, prefix=prefix, end=end)

    def _decrement(self, index: int | None, *, count: int = 1) -> None:
        self._goto(index)
        count = self.brainfuck_code.remove_up_to(count, '+')
        self.brainfuck_code.append('-', count)

    def _increment(self, index: int | None, *, count: int = 1) -> None:
        self._goto(index)
        count = self.brainfuck_code.remove_up_to(count, '-')
        self.brainfuck_code.append('+', count)

    def _loop_start(self, index: int | None) -> None:
        self._goto(index)
        self._loop_start_indices.append(self.index)
        self.brainfuck_code.append('[')

    def _loop_end(self) -> None:
        if len(self._loop_start_indices) == 0:
            raise CompilerException('Unmatched closing bracket in generated code')
        # Automatically balance loops
        self._goto(self._loop_start_indices.pop())
        self.brainfuck_code.append(']')

    def _output(self, index: int | None = None) -> None:
        self._goto(index)
        self.brainfuck_code.append('.')

    def _input(self) -> None:
        self.brainfuck_code.append(',')

    def _left(self, count: int = 1) -> None:
        self.index -= count
        count = self.brainfuck_code.remove_up_to(count, '>')
        self.brainfuck_code.append('<', count)
        if self.index < 0:
            raise CompilerException(f'Subroutine {self.identifier()!r} uses negative indices')

    def _right(self, count: int = 1) -> None:
        self.index += count
        count = self.brainfuck_code.remove_up_to(count, '<')
        self.brainfuck_code.append('>', count)

    def _reset(self, index: int | None = None, *, block_size: int = 1) -> None:
        """Reset the value of the block starting at `index` (current cell by default) to 0"""
        if index is None:
            index = self.index
        for i in range(block_size):
            self._loop_start(index + i)
            self._decrement(index + i)
            self._loop_end()
        self._goto(index)

    def _set(self, value: int, *, index: int | None = None) -> None:
        self._reset(index)
        if value < 0:
            self._decrement(index, count=-value)
        else:
            self._increment(index, count=value)

    def _goto(self, index: int | None) -> None:
        """Move the pointer to the passed index (relative to the starting index of the subroutine)."""
        if index is None:
            return
        delta = index - self.index
        if delta < 0:
            self._left(-delta)
        else:
            self._right(delta)

    def _move(self, destinations: set[int], source: int | None = None, *, block_size: int = 1) -> None:
        """Destructively add the content of the current cell to the destination cells."""
        if block_size <= 0:
            raise CompilerException('Unable to move block with negative size')
        if source is None:
            source = self.index
        if block_size > 1:
            for i in range(block_size):
                self._move({destination + i for destination in destinations}, source + i)
        else:
            self._loop_start(source)
            self._decrement(source)
            # The destinations are sorted to limit the number of moves
            for destination in sorted(destinations):
                if destination == source:
                    break
                self._increment(destination)
            self._loop_end()

    def value(self, value: int, variable_type: DataType = Types.CHAR, identifier: str = None) -> Variable:
        variable = self.variable(variable_type, identifier)
        if value is not None:
            initial_index = self.index
            self._set(value, index=variable.index)
            self._goto(initial_index)
        return variable

    def clear_variable_memory(self, variable: Variable) -> None:
        self.reset_variable(variable)

    def get_name(self, location: Location, identifier: str) -> Name:
        if identifier not in self:
            raise CompilationException(location, f'Unknown variable: {identifier!r}')
        return self[identifier]

    def reset_variable(self, variable: Name) -> None:
        """Reset the passed variable to 0 and move the pointer to its position."""
        self._reset(variable.index, block_size=variable.size())

    def copy_variable(self, source: Name, destination: int | None = None) -> None:
        """Copy the value of the passed variable to the current location."""
        if destination is None:
            destination = self.index
        if source == destination:
            return
        size = source.size()
        # Reset destination
        self._reset(destination, block_size=size)
        # Copy variable cell by cell
        for i in range(size):
            self._reset(self.tmp.index)
            self._move({destination + i, self.tmp.index}, source.index + i)
            self._move({source.index + i}, self.tmp.index)
        # Return to the destination cell
        self._goto(destination)

    def compile_negation_operation(self, index: int, operand: int) -> None:
        # >, <+>[<->[-]]
        self._increment(index)
        self._loop_start(operand)
        self._decrement(index)
        self._reset(operand)
        self._loop_end()

    def compile_bool_normalization_operation(self, index: int, operand: int) -> None:
        # >,[<+>[-]]
        self._loop_start(operand)
        self._increment(index)
        self._reset(operand)
        self._loop_end()

    def compile_opposition_operation(self, index: int, operand: int) -> None:
        # >, [-<->]
        self._loop_start(operand)
        self._decrement(operand)
        self._decrement(index)
        self._loop_end()

    def evaluate_unary_operation(self, operation: UnaryOperation, operand_expression: TypedExpression) -> None:
        index = self.index
        # Array opposition
        if isinstance(operation, ArrayOppositionOperation):
            with self.evaluate_in_new_variable(operand_expression) as operand:
                for i in range(operation.array_count):
                    self.compile_opposition_operation(index + i, operand.index + i)
        # Other operations don't have fast path
        else:
            with self.evaluate_in_new_variable(operand_expression) as operand:
                # Negation
                if isinstance(operation, NegationOperation):
                    self.compile_negation_operation(index, operand.index)
                # Bool normalization
                elif isinstance(operation, BoolNormalizationOperation):
                    self.compile_bool_normalization_operation(index, operand.index)
                # Opposition
                elif isinstance(operation, OppositionOperation):
                    self.compile_opposition_operation(index, operand.index)
                # Unknown
                else:
                    raise ImpossibleException(f'Unknown unary operation: {operation!r}')
        self._goto(index)

    def compile_char_equality_test(self, index: int, left: int, right: int) -> None:
        # >,>, [-<->] <<+>[<->[-]]
        # Subtract right from left
        self._loop_start(right)
        self._decrement(right)
        self._decrement(left)
        self._loop_end()
        # Test if the result is zero
        self._increment(index)
        self._loop_start(left)
        self._decrement(index)
        self._reset(left)
        self._loop_end()

    def compile_equality_test(self, index: int, left: int, right: int, block_size: int) -> None:
        if block_size == 1:
            return self.compile_char_equality_test(index, left, right)
        self._set(1, index=index)
        with self.variable() as tmp:
            # For each pair of cell, test if they are different
            for i in range(block_size):
                # Subtraction is used instead of difference test because the is no need for the result to be normalized
                self.compile_subtraction_operation(tmp.index, left + i, right + i)
                # This loop is entered if the current cells are not equal
                self._loop_start(tmp.index)
                self._reset(index)
                self._reset(tmp.index)
                self._loop_end()

    def compile_char_difference_test(self, index: int, left: int, right: int) -> None:
        # >,>, [-<->] <[<+>[-]]
        # Subtract right from left
        self._loop_start(right)
        self._decrement(right)
        self._decrement(left)
        self._loop_end()
        # Test if the result is non-zero
        self._loop_start(left)
        self._increment(index)
        self._reset(left)
        self._loop_end()

    def compile_difference_test(self, index: int, left: int, right: int, block_size: int) -> None:
        if block_size == 1:
            return self.compile_char_difference_test(index, left, right)
        self._reset(index)
        with self.variable() as tmp:
            # For each pair of cell, test if they are equal
            for i in range(block_size):
                # Subtraction is used instead of difference test because the is no need for the result to be normalized
                self.compile_subtraction_operation(tmp.index, left + i, right + i)
                # This loop is entered if the current cells are not equal
                self._loop_start(tmp.index)
                self._set(1, index=index)
                self._reset(tmp.index)
                self._loop_end()

    def compile_strict_inequality_test(self, index: int, left: int, right: int) -> None:
        with self.variable() as tmp1, self.variable() as tmp2:
            self._loop_start(right)
            # Duplicate right operand
            self._move({tmp1.index, tmp2.index}, left)
            self._move({left}, tmp2.index)
            self._increment(tmp2.index)
            # Do stuff
            self._loop_start(tmp1.index)
            self._decrement(right)
            self._decrement(left)
            self._decrement(tmp2.index)
            self._reset(tmp1.index)
            self._loop_end()
            # Do other stuff
            self._loop_start(tmp2.index)
            self._reset(right)
            self._increment(index)
            self._decrement(tmp2.index)
            self._loop_end()
            self._loop_end()

    def compile_large_inequality_test(self, index: int, left: int, right: int) -> None:
        # Compute !(l > r)
        with self.variable() as tmp1, self.variable() as tmp2:
            self._loop_start(left)
            # Duplicate right operand
            self._move({tmp1.index, tmp2.index}, right)
            self._move({right}, tmp2.index)
            self._increment(tmp2.index)
            # Do stuff
            self._loop_start(tmp1.index)
            self._decrement(left)
            self._decrement(right)
            self._decrement(tmp2.index)
            self._reset(tmp1.index)
            self._loop_end()
            # Do other stuff
            self._loop_start(tmp2.index)
            self._reset(left)
            self._increment(tmp1.index)
            self._decrement(tmp2.index)
            self._loop_end()
            self._loop_end()
            # Negate result
            self._increment(index)
            self._loop_start(tmp1.index)
            self._decrement(tmp1.index)
            self._decrement(index)
            self._loop_end()

    def compile_inverse_strict_inequality_test(self, index: int, left: int, right: int) -> None:
        with self.variable() as tmp1, self.variable() as tmp2:
            self._loop_start(left)
            # Duplicate right operand
            self._move({tmp1.index, tmp2.index}, right)
            self._move({right}, tmp2.index)
            self._increment(tmp2.index)
            # Do stuff
            self._loop_start(tmp1.index)
            self._decrement(left)
            self._decrement(right)
            self._decrement(tmp2.index)
            self._reset(tmp1.index)
            self._loop_end()
            # Do other stuff
            self._loop_start(tmp2.index)
            self._reset(left)
            self._increment(index)
            self._decrement(tmp2.index)
            self._loop_end()
            self._loop_end()

    def compile_inverse_large_inequality_test(self, index: int, left: int, right: int) -> None:
        # Compute !(l < r)
        with self.variable() as tmp1, self.variable() as tmp2:
            self._loop_start(right)
            # Duplicate right operand
            self._move({tmp1.index, tmp2.index}, left)
            self._move({left}, tmp2.index)
            self._increment(tmp2.index)
            # Do stuff
            self._loop_start(tmp1.index)
            self._decrement(right)
            self._decrement(left)
            self._decrement(tmp2.index)
            self._reset(tmp1.index)
            self._loop_end()
            # Do other stuff
            self._loop_start(tmp2.index)
            self._reset(right)
            self._increment(tmp1.index)
            self._decrement(tmp2.index)
            self._loop_end()
            self._loop_end()
            # Negate result
            self._increment(index)
            self._loop_start(tmp1.index)
            self._decrement(tmp1.index)
            self._decrement(index)
            self._loop_end()

    def compile_conjunction_operation(self, index: int, left: int, right: int) -> None:
        # There might be a better way
        # >,>, >++(tmp1) >+(tmp2) <<<[>>-<<[-]] >[>-<[-]] >[>-<[-]] >[<<<<+>>>>[-]]
        with self.value(2) as tmp1, self.value(1) as tmp2:
            # Test if the left operand is true
            self._loop_start(left)
            self._decrement(tmp1.index)
            self._reset(left)
            self._loop_end()
            # Test if the right operand is true
            self._loop_start(right)
            self._decrement(tmp1.index)
            self._reset(right)
            self._loop_end()
            # Test if one of the operand is true
            self._loop_start(tmp1.index)
            self._decrement(tmp2.index)
            self._reset(tmp1.index)
            self._loop_end()
            # Return result
            self._loop_start(tmp2.index)
            self._increment(index)
            self._reset(tmp2.index)
            self._loop_end()

    def compile_disjunction_operation(self, index: int, left: int, right: int) -> None:
        # There might be a better way
        # >,>, >+(tmp) <<[>>[-]<<-<+>] >>[<[-<<+>>]>[-]]
        with self.value(1) as tmp:
            # If the left operand is truthy, its value is copied
            self._loop_start(left)
            self._reset(tmp.index)
            self._decrement(left)
            self._increment(index)
            self._loop_end()
            # Else, the value of the right operand is copied
            self._loop_start(tmp.index)
            self._move({index}, right)
            self._reset(tmp.index)
            self._loop_end()

    def compile_addition_operation(self, index: int, left: int, right: int) -> None:
        # >,>, <[-<+>] >[-<<+>>]
        self._move({index}, left)
        self._loop_start(right)
        self._decrement(right)
        self._increment(index)
        self._loop_end()

    def compile_subtraction_operation(self, index: int, left: int, right: int) -> None:
        # >,>, <[-<+>] >[-<<->>]
        self._move({index}, left)
        self._loop_start(right)
        self._decrement(right)
        self._decrement(index)
        self._loop_end()

    def compile_multiplication_operation(self, index: int, left: int, right: int) -> None:
        # >,>, >(tmp) <<[- >[-<<+>>>+<] >[-<+>]<<]
        with self.variable() as tmp:
            self._loop_start(left)
            self._decrement(left)
            self._move({index, tmp.index}, right)
            self._move({right}, tmp.index)
            self._loop_end()

    def compile_division_operation(self, index: int, left: int, right: int) -> None:
        # >,>, >(tmp1) >(tmp2) >(tmp3) <<<<<[->->+ <[->>+>+<<<]>>>[-<<<+>>>] + <[>-<[-]] >[-<<<<<+>>>[-<+>]>>]<<<<]
        with self.variable() as tmp1, self.variable() as tmp2, self.variable() as tmp3:
            self._loop_start(left)
            self._decrement(left)
            self._decrement(right)
            self._increment(tmp1.index)
            self._move({tmp2.index, tmp3.index}, right)
            self._move({right}, tmp3.index)
            self._increment(tmp3.index)
            self._loop_start(tmp2.index)
            self._decrement(tmp3.index)
            self._reset(tmp2.index)
            self._loop_end()
            self._loop_start(tmp3.index)
            self._decrement(tmp3.index)
            self._increment(index)
            self._move({right}, tmp1.index)
            self._loop_end()
            self._loop_end()

    def compile_modulo_operation(self, index: int, left: int, right: int) -> None:
        # Same as division, but result is in tmp1
        # >,>, >(tmp1) >(tmp2) >(tmp3) <<<<<[->->+ <[->>+>+<<<]>>>[-<<<+>>>] + <[>-<[-]] >[-<<<<<+>>>[-<+>]>>]<<<<]
        with self.variable() as tmp1, self.variable() as tmp2, self.variable() as tmp3:
            self._loop_start(left)
            self._decrement(left)
            self._decrement(right)
            self._increment(index)
            self._move({tmp1.index, tmp2.index}, right)
            self._move({right}, tmp2.index)
            self._increment(tmp2.index)
            self._loop_start(tmp1.index)
            self._decrement(tmp2.index)
            self._reset(tmp1.index)
            self._loop_end()
            self._loop_start(tmp2.index)
            self._decrement(tmp2.index)
            self._increment(tmp3.index)
            self._move({right}, index)
            self._loop_end()
            self._loop_end()

    def evaluate_binary_operation(self, operation: BinaryOperation, left_expression: TypedExpression,
                                  right_expression: TypedExpression) -> None:
        index = self.index
        # Array concatenation
        if isinstance(operation, ConcatenationOperation):
            self.evaluate(left_expression, index)
            left_array_size = operation.left_array_count * operation.base_type.size()
            self.evaluate(right_expression, index + left_array_size)
        # Array scaling
        elif isinstance(operation, ArrayScalingOperation):
            with (self.evaluate_in_new_variable(left_expression) as left_backup,
                  self.variable(left_expression.type()) as left,
                  self.evaluate_in_new_variable(right_expression) as right):
                for i in range(operation.array_count):
                    self.copy_variable(left_backup, left.index)
                    self.compile_multiplication_operation(index + i, left.index, right.index + i)
        # Other operations don't have fast path
        else:
            with (self.evaluate_in_new_variable(left_expression) as left,
                  self.evaluate_in_new_variable(right_expression) as right):
                # Equality test
                if isinstance(operation, EqualityTestOperation):
                    self.compile_equality_test(index, left.index, right.index, operation.left_type().size())
                # Difference test
                elif isinstance(operation, DifferenceTestOperation):
                    self.compile_difference_test(index, left.index, right.index, operation.left_type().size())
                # Strict inequality test
                elif isinstance(operation, StrictInequalityTestOperation):
                    self.compile_strict_inequality_test(index, left.index, right.index)
                # Large inequality test
                elif isinstance(operation, LargeInequalityTestOperation):
                    self.compile_large_inequality_test(index, left.index, right.index)
                # Inverse strict inequality test
                elif isinstance(operation, InverseStrictInequalityTestOperation):
                    self.compile_inverse_strict_inequality_test(index, left.index, right.index)
                # Inverse large inequality test
                elif isinstance(operation, InverseLargeInequalityTestOperation):
                    self.compile_inverse_large_inequality_test(index, left.index, right.index)
                # Conjunction
                elif isinstance(operation, ConjunctionOperation):
                    self.compile_conjunction_operation(index, left.index, right.index)
                # Disjunction
                elif isinstance(operation, DisjunctionOperation):
                    self.compile_disjunction_operation(index, left.index, right.index)
                # Addition
                elif isinstance(operation, AdditionOperation):
                    self.compile_addition_operation(index, left.index, right.index)
                # Subtraction
                elif isinstance(operation, SubtractionOperation):
                    self.compile_subtraction_operation(index, left.index, right.index)
                # Multiplication
                elif isinstance(operation, MultiplicationOperation):
                    self.compile_multiplication_operation(index, left.index, right.index)
                # Division
                elif isinstance(operation, DivisionOperation):
                    self.compile_division_operation(index, left.index, right.index)
                # Modulo
                elif isinstance(operation, ModuloOperation):
                    self.compile_modulo_operation(index, left.index, right.index)
                # Unknown
                else:
                    raise ImpossibleException(f'Unknown binary operation: {operation!r}')
        self._goto(index)

    def evaluate_array_comprehension(self, element_format: TypedExpression, iterators: list[TypedForLoopIterator],
                                     index: int | None = None) -> None:
        if index is None:
            index = self.index
        self._goto(index)
        count = iterators[0].count
        with self.scope():
            # Evaluate arrays and declare loop variables
            variables = []
            for iterator in iterators:
                # There is no need to copy the entire array if it already exists
                if isinstance(iterator.array, TypedIdentifier):
                    array = self.get_name(iterator.array.location, iterator.array.name)
                else:
                    array = self.scoped_variable(iterator.array.type())
                    self.evaluate(iterator.array, array.index)
                variables.append(self.scoped_pointer(array.index, iterator.type, iterator.variable))
            # Run loop
            for i in range(count):
                self.evaluate(element_format, index + i * element_format.type().size())
                # Update loop variable indices
                for variable in variables:
                    variable.update_index(variable.index + variable.type.size())

    def evaluate(self, expression: TypedExpression, index: int | None = None) -> None:
        """Evaluate the passed expression at the current location (or `index` if specified)."""
        if index is None:
            index = self.index
        self._goto(index)
        match expression:
            case LiteralChar(value=value):
                self._set(value)
            case LiteralArray(value=value):
                for element in value:
                    self.evaluate(element)
                    self._right(element.type().size())
            case TypedArrayComprehension(element_format=element_format, iterators=iterators):
                self.evaluate_array_comprehension(element_format, iterators)
            case LiteralTuple(elements=elements):
                for element in elements:
                    self.evaluate(element)
                    self._right(element.type().size())
            case TypedIdentifier(location=location, name=name):
                variable = self.get_name(location, name)
                self.copy_variable(variable)
                self._comment(f'Copied {name}', CommentLevel.BZ_CODE_EXTENDED)
            case TypedArraySubscriptExpression(array=array, index=array_index):
                element_size = array.type().base_type.size()
                with self.evaluate_in_new_variable(array) as tmp:
                    self._move({index}, tmp.index + array_index * element_size, block_size=element_size)
            case TypedArraySlicingExpression(array=array, start=start, stop=stop):
                element_size = array.type().base_type.size()
                with self.evaluate_in_new_variable(array) as tmp:
                    for i in range(start, stop):
                        self._move({index + (i - start) * element_size}, tmp.index + i * element_size,
                                   block_size=element_size)
            case InputCall():
                self._input()
            case TypedFunctionCall(location=location, reference=reference, arguments=arguments):
                self.call(location, reference, arguments, True)
            case TypedUnaryArithmeticExpression(operation=operation, operand=operand_expression):
                self.evaluate_unary_operation(operation, operand_expression)
            case TypedBinaryArithmeticExpression(operation=operation, left=left_expression, right=right_expression):
                self.evaluate_binary_operation(operation, left_expression, right_expression)
            case _:
                raise ImpossibleException(f'Unknown expression type: {expression.__class__.__name__}')
        self._goto(index)

    def get_pointer(self, expression: TypedExpression) -> Pointer | None:
        """Return a pointer to the passed expression if its value is already stored somewhere (None otherwise)."""
        match expression:
            case TypedIdentifier(location=location, name=name):
                variable = self.get_name(location, name)
                return self.pointer(variable.index, expression.type())
            case TypedArraySubscriptExpression(array=array, index=index):
                array_pointer = self.get_pointer(array)
                if array_pointer is None:
                    return None
                base_type_size = expression.type().size()
                return self.pointer(array_pointer.index + index * base_type_size, expression.type())
            case TypedArraySlicingExpression(array=array, start=start):
                array_pointer = self.get_pointer(array)
                if array_pointer is None:
                    return None
                base_type_size = expression.type().size()
                return self.pointer(array_pointer.index + start * base_type_size, expression.type())
        return None

    def evaluate_in_new_variable(self, expression: TypedExpression, *, read_only: bool = False) -> Name:
        """
        Evaluate the passed expression in a new variable.
        If `read_only` is True, it might be a pointer to a cell that already contains this value.
        """
        if read_only:
            pointer = self.get_pointer(expression)
            if pointer is not None:
                return pointer
        variable = self.variable(expression.type())
        self.evaluate(expression, variable.index)
        return variable

    def _evaluate_arguments(self, arguments: list[TypedExpression]) -> int:
        """Evaluate passed arguments for subroutine call and return subroutine origin."""
        with self.scope():
            # Start by evaluating complex arguments (arguments that are not just literals and identifiers)
            evaluated_arguments: list[int | Name] = []
            for argument in arguments:
                match argument:
                    case LiteralChar(value=value):
                        evaluated_arguments.append(value)
                    case TypedIdentifier(location=location, name=name):
                        variable = self.get_name(location, name)
                        evaluated_arguments.append(variable)
                    case _:
                        arg = self.scoped_variable(argument.type())
                        self.evaluate(argument, arg.index)
                        evaluated_arguments.append(arg)
            # Then copy everything at the right place
            subroutine_origin = self.memory_size()
            self._goto(subroutine_origin)
            for i, evaluated_argument in enumerate(evaluated_arguments):
                if isinstance(evaluated_argument, int):
                    self._set(evaluated_argument)
                    self._right()
                else:
                    self.copy_variable(evaluated_argument)
                    self._right(evaluated_argument.size())
        return subroutine_origin

    def call(self, location: Location, reference: Reference, arguments: list[TypedExpression],
             expect_return: bool) -> None:
        """Call a subroutine and, if specified, get a return value"""
        # Get call context and subroutine
        call_index = self.index
        subroutine = self.context.get_subroutine(reference)
        if expect_return and not subroutine.returns():
            raise CompilerException(f'Using procedure {reference} as a function should have been caught earlier')
        # Test if the number of arguments is right
        arity = subroutine.arity()
        if arity != len(arguments):
            message = f'Subroutine {reference} expects {arity} arguments, found {len(arguments)}'
            raise CompilationException(location, message)
        # First, evaluates the arguments
        self._comment(f'Preparing call to subroutine {reference} (evaluating arguments)', prefix='\n')
        subroutine_origin = self._evaluate_arguments(arguments)
        self._goto(subroutine_origin)
        # Then, call the subroutine
        self._comment(f'Calling subroutine {reference}', prefix='\n')
        self.brainfuck_code.extend(subroutine.generate(comment_level=self.comment_level))
        self.index += subroutine.index
        # Finally, get the return value (if required) and reset the cells to 0
        self._comment(f'Finalizing call to subroutine {reference}', prefix='\n')
        if expect_return:
            return_index = self.index
            block_size = subroutine.return_type().size()
            self._reset(call_index, block_size=block_size)
            self._move({call_index}, return_index, block_size=block_size)
            self._comment('Get return value')
        # Clear memory
        self._reset(subroutine_origin, block_size=subroutine.memory_size())
        self._comment('Reset memory')
        # Finalize
        self._goto(call_index)
        self._comment('Go to call index')

    def assign(self, target: TypedAssignmentTarget, index: int) -> None:
        match target:
            case TypedPrimitiveAssignmentTarget(location=location, identifier=identifier, offset=offset):
                variable = self.get_name(location, identifier)
                destination = variable.index + offset
                self._reset(destination, block_size=target.type().size())
                self._move({destination}, index, block_size=target.type().size())
            case TypedTupleAssignmentTarget(elements=elements) as tuple_target:
                product_type = tuple_target.type()
                for i, element in enumerate(elements):
                    self.assign(element, index + product_type.offset_of(i))
            case _:
                raise ImpossibleException(f'Unknown assignment target type: {target.__class__.__name__}')

    def print(self, values: list[TypedExpression], *, new_line: bool = False) -> None:
        for expression in values:
            if not expression.type().is_string():
                raise CompilationException(expression.location, f'Expected string but found {expression.type()}')
            with self.evaluate_in_new_variable(expression, read_only=True) as tmp:
                for i in range(expression.type().size()):
                    self._output(tmp.index + i)
        if new_line:
            with self.value(ord('\n')) as tmp:
                self._output(tmp.index)

    def loop(self, count: TypedExpression, body: TypeCheckedInstructionBlock) -> None:
        self._comment('Initializing loop counter', prefix='\n')
        with self.evaluate_in_new_variable(count) as counter:
            self._comment('Starting loop', prefix='\n')
            self._loop_start(counter.index)
            self.compile_instruction(body)
            self._decrement(counter.index)
            self._loop_end()
            self._comment(f'Ending loop', prefix='\n')

    def while_loop(self, test: TypedExpression, body: TypeCheckedInstructionBlock) -> None:
        with self.evaluate_in_new_variable(test) as condition:
            self._loop_start(condition.index)
            self.compile_instruction(body)
            self.evaluate(test, condition.index)
            self._loop_end()

    def do_while_loop(self, body: TypeCheckedInstructionBlock, test: TypedExpression) -> None:
        with self.value(1) as condition:
            self._loop_start(condition.index)
            self.compile_instruction(body)
            self.evaluate(test, condition.index)
            self._loop_end()

    def for_loop(self, iterators: list[TypedForLoopIterator], body: TypeCheckedInstructionBlock) -> None:
        count = iterators[0].count
        with self.scope():
            # Evaluate arrays and declare loop variables
            variables = []
            for iterator in iterators:
                # Mutating the loop variable should mutate the corresponding element in the array.
                if isinstance(iterator.array, TypedIdentifier):
                    array = self.get_name(iterator.array.location, iterator.array.name)
                else:
                    array = self.scoped_variable(iterator.array.type())
                    self.evaluate(iterator.array, array.index)
                variables.append(self.scoped_pointer(array.index, iterator.type, iterator.variable))
            # Run loop
            for i in range(count):
                self.compile_instruction(body)
                # Update loop variable indices
                for variable in variables:
                    variable.update_index(variable.index + variable.type.size())

    def condition(self, test: TypedExpression, if_body: TypeCheckedInstructionBlock,
                  else_body: TypeCheckedInstructionBlock) -> None:
        self._comment('Initializing conditional statement and evaluating condition', prefix='\n')
        with self.evaluate_in_new_variable(test) as condition:
            if else_body is None:
                self._comment('Starting if', prefix='\n')
                self._loop_start(condition.index)
                self.compile_instruction(if_body)
                self._reset(condition.index)
                self._loop_end()
            else:
                with self.value(1) as has_entered_if:
                    self._comment('Starting if', prefix='\n')
                    self._loop_start(condition.index)
                    self.compile_instruction(if_body)
                    self._decrement(has_entered_if.index)  # Reset
                    self._reset(condition.index)
                    self._loop_end()
                    self._comment('Starting else', prefix='\n')
                    self._loop_start(has_entered_if.index)
                    self.compile_instruction(else_body)
                    self._decrement(has_entered_if.index)  # Reset
                    self._loop_end()

    def create_context_snapshot(self, location: Location = Location.unknown(), identifier: str | None = None) -> None:
        current_index = self.index

        self._comment('=== CONTEXT SNAPSHOT ===', CommentLevel.ONLY_REQUIRED, prefix='\n')

        if identifier is not None:
            variable = self.get_name(location, identifier)
            self._goto(variable.index)
        self.brainfuck_code.append('#')

        context = f'Currently at index {self.index} in subroutine {self.identifier()!r}'
        self._comment(context, CommentLevel.ONLY_REQUIRED, prefix='\n', end='')

        scopes = 'Active names: ' + str(self.names)
        self._comment(scopes, CommentLevel.ONLY_REQUIRED, prefix='\n', end='')

        local_memory_overview = f'Local memory size: {self.memory_size()}'
        self._comment(local_memory_overview, CommentLevel.ONLY_REQUIRED, prefix='\n')

        self._comment('=== CONTEXT SNAPSHOT END ===', CommentLevel.ONLY_REQUIRED, prefix='')

        self._goto(current_index)

    def compile_instruction(self, instruction: TypeCheckedInstruction) -> None:
        comment_line = True
        match instruction:
            case TypeCheckedInstructionBlock(instructions=instructions):
                with self.scope():
                    for instruction in instructions:
                        self.compile_instruction(instruction)
            case TypeCheckedVariableDeclaration(identifier=identifier, type=variable_type, value=expression):
                variable = self.scoped_variable(variable_type, identifier)
                if expression is not None:
                    self.evaluate(expression, variable.index)
            case TypeCheckedIncrementation(location=location, identifier=identifier):
                variable = self.get_name(location, identifier)
                self._increment(variable.index)
            case TypeCheckedDecrementation(location=location, identifier=identifier):
                variable = self.get_name(location, identifier)
                self._decrement(variable.index)
            case TypeCheckedAssignment(target=target, value=value):
                with self.evaluate_in_new_variable(value) as tmp:
                    self.assign(target, tmp.index)
            case PrintCall(arguments=arguments, new_line=new_line):
                self.print(arguments, new_line=new_line)
            case TypeCheckedProcedureCall(location=location, reference=reference, arguments=arguments):
                self.call(location, reference, arguments, False)
                comment_line = False
            case TypedExpression() as expression:
                self.evaluate(expression)
            case TypeCheckedLoopStatement(count=count, body=body):
                self.loop(count, body)
            case TypeCheckedWhileLoopStatement(test=test, body=body):
                self.while_loop(test, body)
            case TypeCheckedDoWhileLoopStatement(body=body, test=test):
                self.do_while_loop(body, test)
            case TypeCheckedForLoopStatement(iterators=iterators, body=body):
                self.for_loop(iterators, body)
            case TypeCheckedConditionalStatement(test=test, if_body=if_body, else_body=else_body):
                self.condition(test, if_body, else_body)
            case TypeCheckedReturnInstruction(value=expression):
                self.evaluate(expression, self.return_index)
            case TypeCheckedContextSnapshot(location=location, identifier=identifier):
                self.create_context_snapshot(location, identifier)
                CompilationWarning.add(location, '`?` is a debug feature and should not be used in production code')
                comment_line = False
            case _:
                raise ImpossibleException(f'Unknown instruction type: {instruction.__class__.__name__}')
        if comment_line:
            self._comment(f'{instruction};', CommentLevel.BZ_CODE)

    def generate(self, *, comment_level: int | None = None) -> BrainfuckCode:
        if comment_level is not None:
            self.comment_level = comment_level
        if not self.generated:
            if isinstance(self.subroutine, TypeCheckedNativeSubroutine):
                self.brainfuck_code.append_raw(self.subroutine.bf_code)
                self.index = self.subroutine.offset
            elif isinstance(self.subroutine, TypeCheckedProcedure):
                self.compile_instruction(self.subroutine.body)
                self._goto(self.return_index)
            else:
                raise ImpossibleException(f'Unknown subroutine type: {self.subroutine.__class__.__name__}')
            self.generated = True
        return self.brainfuck_code


def generate_namespace_context(namespace: TypeCheckedNamespace, global_context: Context = Context.empty()) -> 'Context':
    context = Context.with_global(global_context)
    for element in namespace:
        if isinstance(element, TypeCheckedConstant):
            pass
        elif isinstance(element, TypeCheckedNamespace):
            namespace_context = generate_namespace_context(element, context)
            context = context.with_namespace(element.identifier, namespace_context)
        elif isinstance(element, TypeCheckedSubroutine):
            context = context.with_subroutine(element.identifier, SubroutineCompiler(element, context))
        else:
            raise CompilerException(f'Unknown namespace element type: {element.__class__.__name__}')
    return context


def generate_program(namespace: TypeCheckedNamespace, main_procedure_reference: Reference, *,
                     comment_level: int = CommentLevel.BZ_CODE) -> str:
    context = generate_namespace_context(namespace)
    try:
        main = context.get_subroutine(main_procedure_reference)
    except CompilerException:
        raise CompilationException(Location.unknown(), 'Unable to locate main procedure')
    if main.arity() > 0:
        raise CompilationException(main.subroutine.location, 'Main procedure should not accept arguments')
    if main.returns():
        raise CompilationException(main.subroutine.location, 'Main subroutine should be a procedure')
    return str(main.generate(comment_level=comment_level)).strip() + '\n'


__all__ = ['CommentLevel', 'generate_program']
