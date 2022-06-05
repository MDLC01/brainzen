from enum import IntEnum

from data_types import *
from exceptions import *
from generation.name_manager import *
from tokenization.operators import *
from type_checking import *


class CommentLevel(IntEnum):
    ONLY_REQUIRED = 0
    BZ_CODE = 1
    BZ_CODE_EXTENDED = 2
    DETAILS = 3


class SubroutineCompiler(NameManager):
    """
    A subroutine is only allowed to change cells to the right of where it started, and it
    must set all values back to 0 after being called. It can expect cells to contain 0 by default.
    The return value of a function is defined by the position of the pointer when it ends.
    """

    def __init__(self, subroutine: TypeCheckedSubroutine, context: dict[str, 'SubroutineCompiler'], *,
                 comment_level: int = CommentLevel.BZ_CODE) -> None:
        super().__init__(subroutine.arguments, subroutine.return_type)
        self.context: dict[str, SubroutineCompiler] = context
        self.subroutine: TypeCheckedSubroutine = subroutine
        self.comment_level: int = comment_level
        self.bf_code: str = ''
        self.index: int = 0
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

    def _remove_up_to(self, count: int, char: str) -> int:
        """
        Remove up to `count` times the passed character at the end of the Brainfuck code
        and return the number of characters that where removed, minus `count`.
        """
        i = 0
        while i < count and len(self.bf_code) > 0 and self.bf_code[-1] == char:
            self.bf_code = self.bf_code[:-1]
            i += 1
        return count - i

    def _comment(self, comment: str, level: int = CommentLevel.DETAILS, *, prefix: str = '\t', end: str = '\n') -> None:
        if level <= self.comment_level:
            serialized_comment = (comment
                                  .replace('<', '(less than)')
                                  .replace('>', '(greater than)')
                                  .replace('+', '(plus)')
                                  .replace('-', '(minus)')
                                  .replace('[', '(')
                                  .replace(']', ')')
                                  .replace('.', '(period)')
                                  .replace(',', '(comma)')
                                  .replace('#', '(hash)'))
            self.bf_code += f'{prefix}{serialized_comment}{end}'

    def _decrement(self, index: int | None = None, *, count: int = 1) -> None:
        self._goto(index)
        count = self._remove_up_to(count, '+')
        self.bf_code += '-' * count

    def _increment(self, index: int | None = None, *, count: int = 1) -> None:
        self._goto(index)
        count = self._remove_up_to(count, '-')
        self.bf_code += '+' * count

    def _loop_start(self, index: int | None = None) -> None:
        self._goto(index)
        self._loop_start_indices.append(self.index)
        self.bf_code += '['

    def _loop_end(self) -> None:
        if len(self._loop_start_indices) == 0:
            raise CompilerException('Unmatched closing bracket in generated code')
        # Automatically balance loops
        self._goto(self._loop_start_indices.pop())
        self.bf_code += ']'

    def _output(self, index: int | None = None) -> None:
        self._goto(index)
        self.bf_code += '.'

    def _input(self) -> None:
        self.bf_code += ','

    def _left(self, count: int = 1) -> None:
        self.index -= count
        count = self._remove_up_to(count, '>')
        self.bf_code += '<' * count
        if self.index < 0:
            raise CompilerException(f'Subroutine {self.identifier()!r} uses negative indices')

    def _right(self, count: int = 1) -> None:
        self.index += count
        count = self._remove_up_to(count, '<')
        self.bf_code += '>' * count

    def _reset(self, index: int | None = None, *, block_size: int = 1) -> None:
        """Reset the value of the block starting at `index` (current cell by default) to 0"""
        self._goto(index)
        for _ in range(block_size):
            self._loop_start()
            self._decrement()
            self._loop_end()
            self._right()
        self._left(block_size)

    def _set(self, value: int) -> None:
        self._reset()
        if value < 0:
            self._decrement(count=-value)
        else:
            self._increment(count=value)

    def _goto(self, index: int | None) -> None:
        """Move the pointer to the passed index (relative to the starting index of the subroutine)."""
        if index is None:
            return
        delta = index - self.index
        if delta < 0:
            self._left(-delta)
        else:
            self._right(delta)

    def _move(self, destinations: set[int], *, block_size: int = 1) -> None:
        """Destructively add the content of the current cell to the destination cells."""
        if block_size <= 0:
            raise CompilerException('Unable to move block with negative size')
        source = self.index
        if block_size > 1:
            for i in range(block_size):
                self._goto(source + i)
                self._move({destination + i for destination in destinations})
        else:
            self._loop_start()
            self._decrement()
            # The destinations are sorted to limit the number of moves
            for destination in sorted(destinations):
                if destination == source:
                    break
                self._goto(destination)
                self._increment()
            self._goto(source)
            self._loop_end()

    def value(self, value: int, variable_type: DataType = Types.CHAR, identifier: str = None) -> Variable:
        variable = self.variable(variable_type, identifier)
        if value is not None:
            initial_index = self.index
            self._goto(variable.index)
            self._set(value)
            self._goto(initial_index)
        return variable

    def clear_variable_memory(self, variable: Variable) -> None:
        self._reset(variable.index, block_size=variable.size())

    def get_name(self, location: Location, identifier: str) -> Name:
        if identifier not in self:
            raise CompilationException(location, f'Unknown variable: {identifier!r}')
        return self[identifier]

    def reset_variable(self, variable: Name) -> None:
        """Reset the passed variable to 0 and move the pointer to its position."""
        self._reset(variable.index, block_size=variable.size())

    def copy_variable(self, source: Name) -> None:
        """Copy the value of the passed variable to the current location."""
        destination = self.index
        backup = self.tmp
        if source == destination:
            return
        size = source.size()
        # Reset destination
        self._reset(destination, block_size=size)
        # Copy variable cell by cell
        for i in range(size):
            self._reset(backup.index)
            self._goto(source.index + i)
            self._move({destination + i, backup.index})
            self._goto(backup.index)
            self._move({source.index + i})
        # Return to the destination cell
        self._goto(destination)

    def get_subroutine(self, location: Location, identifier: str) -> 'SubroutineCompiler':
        if identifier in self.context:
            return self.context[identifier]
        raise CompilationException(location, f'Unknown subroutine: {identifier!r}')

    def evaluate_unary_arithmetic_expression(self, expression: TypedUnaryArithmeticExpression) -> None:
        evaluation_index = self.index
        with self.evaluate_in_new_variable(expression.operand) as operand:
            self._reset(evaluation_index)
            # Comments show corresponding Brainfuck code, where the evaluation index is the starting index, and the
            # operand is stored at the index of the pointer when the comma is reached
            if expression.operation is UnaryOperator.DOUBLE_BANG:
                # >,[<+>[-]]
                self._loop_start(operand.index)
                self._goto(evaluation_index)
                self._increment()
                self._reset(operand.index)
                self._loop_end()
            elif expression.operation is UnaryOperator.BANG:
                # >, <+>[<->[-]]
                self._goto(evaluation_index)
                self._increment()
                self._loop_start(operand.index)
                self._goto(evaluation_index)
                self._decrement()
                self._reset(operand.index)
                self._loop_end()
            elif expression.operation is UnaryOperator.MINUS:
                # >, [-<->]
                self._loop_start(operand.index)
                self._decrement()
                self._goto(evaluation_index)
                self._decrement()
                self._goto(operand.index)
                self._loop_end()
            else:
                raise ImpossibleException(f'Unknown unary operation: {expression.operation!r}')
        self._goto(evaluation_index)

    def evaluate_binary_arithmetic_expression(self, expression: TypedBinaryArithmeticExpression) -> None:
        """Evaluate the passed binary arithmetic expression in the current cell."""
        evaluation_index = self.index
        with (self.evaluate_in_new_variable(expression.left) as left,
              self.evaluate_in_new_variable(expression.right) as right):
            self._reset(evaluation_index)
            # Comments show corresponding Brainfuck code, where the evaluation index is the starting index, the
            # left operand is stored at the index of the pointer when the first comma is reached, and the right
            # operand is stored at the index of the pointer when the second comma is reached
            if expression.operation is BinaryOperator.DOUBLE_EQUAL:
                # >,>, [-<->] <<+>[<->[-]]
                # Subtract right from left
                self._loop_start(right.index)
                self._decrement()
                self._decrement(left.index)
                self._goto(right.index)
                self._loop_end()
                # Test if the result is zero
                self._goto(evaluation_index)
                self._increment()
                self._loop_start(left.index)
                self._goto(evaluation_index)
                self._decrement()
                self._reset(left.index)
                self._loop_end()
            elif expression.operation is BinaryOperator.BANG_EQUAL:
                # >,>, [-<->] <[<+>[-]]
                # Subtract right from left
                self._loop_start(right.index)
                self._decrement()
                self._decrement(left.index)
                self._goto(right.index)
                self._loop_end()
                # Test if the result is non-zero
                self._loop_start(left.index)
                self._goto(evaluation_index)
                self._increment()
                self._reset(left.index)
                self._loop_end()
            elif expression.operation is BinaryOperator.LESS_THAN:
                with self.variable() as tmp1, self.variable() as tmp2:
                    self._loop_start(right.index)
                    # Duplicate right operand
                    self._goto(left.index)
                    self._move({tmp1.index, tmp2.index})
                    self._goto(tmp2.index)
                    self._move({left.index})
                    self._increment(tmp2.index)
                    # Do stuff
                    self._loop_start(tmp1.index)
                    self._decrement(right.index)
                    self._decrement(left.index)
                    self._decrement(tmp2.index)
                    self._reset(tmp1.index)
                    self._loop_end()
                    # Do other stuff
                    self._loop_start(tmp2.index)
                    self._reset(right.index)
                    self._goto(evaluation_index)
                    self._increment()
                    self._decrement(tmp2.index)
                    self._loop_end()
                    self._goto(right.index)
                    self._loop_end()
            elif expression.operation is BinaryOperator.LESS_THAN_EQUAL:
                # Compute !(l > r)
                with self.variable() as tmp1, self.variable() as tmp2:
                    self._loop_start(left.index)
                    # Duplicate right operand
                    self._goto(right.index)
                    self._move({tmp1.index, tmp2.index})
                    self._goto(tmp2.index)
                    self._move({right.index})
                    self._increment(tmp2.index)
                    # Do stuff
                    self._loop_start(tmp1.index)
                    self._decrement(left.index)
                    self._decrement(right.index)
                    self._decrement(tmp2.index)
                    self._reset(tmp1.index)
                    self._loop_end()
                    # Do other stuff
                    self._loop_start(tmp2.index)
                    self._reset(left.index)
                    self._increment(tmp1.index)
                    self._decrement(tmp2.index)
                    self._loop_end()
                    self._goto(left.index)
                    self._loop_end()
                    # Negate result
                    self._goto(evaluation_index)
                    self._increment()
                    self._loop_start(tmp1.index)
                    self._decrement(tmp1.index)
                    self._goto(evaluation_index)
                    self._decrement()
                    self._goto(tmp1.index)
                    self._loop_end()
            elif expression.operation is BinaryOperator.GREATER_THAN:
                with self.variable() as tmp1, self.variable() as tmp2:
                    self._loop_start(left.index)
                    # Duplicate right operand
                    self._goto(right.index)
                    self._move({tmp1.index, tmp2.index})
                    self._goto(tmp2.index)
                    self._move({right.index})
                    self._increment(tmp2.index)
                    # Do stuff
                    self._loop_start(tmp1.index)
                    self._decrement(left.index)
                    self._decrement(right.index)
                    self._decrement(tmp2.index)
                    self._reset(tmp1.index)
                    self._loop_end()
                    # Do other stuff
                    self._loop_start(tmp2.index)
                    self._reset(left.index)
                    self._goto(evaluation_index)
                    self._increment()
                    self._decrement(tmp2.index)
                    self._loop_end()
                    self._goto(left.index)
                    self._loop_end()
            elif expression.operation is BinaryOperator.GREATER_THAN_EQUAL:
                # Compute !(l < r)
                with self.variable() as tmp1, self.variable() as tmp2:
                    self._loop_start(right.index)
                    # Duplicate right operand
                    self._goto(left.index)
                    self._move({tmp1.index, tmp2.index})
                    self._goto(tmp2.index)
                    self._move({left.index})
                    self._increment(tmp2.index)
                    # Do stuff
                    self._loop_start(tmp1.index)
                    self._decrement(right.index)
                    self._decrement(left.index)
                    self._decrement(tmp2.index)
                    self._reset(tmp1.index)
                    self._loop_end()
                    # Do other stuff
                    self._loop_start(tmp2.index)
                    self._reset(right.index)
                    self._increment(tmp1.index)
                    self._decrement(tmp2.index)
                    self._loop_end()
                    self._goto(right.index)
                    self._loop_end()
                    # Negate result
                    self._goto(evaluation_index)
                    self._increment()
                    self._loop_start(tmp1.index)
                    self._decrement(tmp1.index)
                    self._goto(evaluation_index)
                    self._decrement()
                    self._goto(tmp1.index)
                    self._loop_end()
            elif expression.operation is BinaryOperator.DOUBLE_AMPERSAND:
                # There might be a better way
                # >,>, >++(tmp1) >+(tmp2) <<<[>>-<<[-]] >[>-<[-]] >[>-<[-]] >[<<<<+>>>>[-]]
                with self.value(2) as tmp1, self.value(1) as tmp2:
                    # Test if the left operand is true
                    self._loop_start(left.index)
                    self._decrement(tmp1.index)
                    self._reset(left.index)
                    self._loop_end()
                    # Test if the right operand is true
                    self._loop_start(right.index)
                    self._decrement(tmp1.index)
                    self._reset(right.index)
                    self._loop_end()
                    # Test if one of the operand is true
                    self._loop_start(tmp1.index)
                    self._decrement(tmp2.index)
                    self._reset(tmp1.index)
                    self._loop_end()
                    # Return result
                    self._loop_start(tmp2.index)
                    self._goto(evaluation_index)
                    self._increment()
                    self._reset(tmp2.index)
                    self._loop_end()
            elif expression.operation is BinaryOperator.DOUBLE_PIPE:
                # There might be a better way
                # >,>, >+(tmp) <<[>>[-]<<-<+>] >>[<[-<<+>>]>[-]]
                with self.value(1) as tmp:
                    # If the left operand is truthy, its value is copied
                    self._loop_start(left.index)
                    self._reset(tmp.index)
                    self._decrement(left.index)
                    self._goto(evaluation_index)
                    self._increment()
                    self._goto(left.index)
                    self._loop_end()
                    # Else, the value of the right operand is copied
                    self._loop_start(tmp.index)
                    self._goto(right.index)
                    self._move({evaluation_index})
                    self._reset(tmp.index)
                    self._loop_end()
            elif expression.operation is BinaryOperator.PLUS:
                # >,>, <[-<+>] >[-<<+>>]
                self._goto(left.index)
                self._move({evaluation_index})
                self._loop_start(right.index)
                self._decrement()
                self._goto(evaluation_index)
                self._increment()
                self._goto(right.index)
                self._loop_end()
            elif expression.operation is BinaryOperator.MINUS:
                # >,>, <[-<+>] >[-<<->>]
                self._goto(left.index)
                self._move({evaluation_index})
                self._loop_start(right.index)
                self._decrement()
                self._goto(evaluation_index)
                self._decrement()
                self._goto(right.index)
                self._loop_end()
            elif expression.operation is BinaryOperator.STAR:
                # >,>, >(tmp) <<[- >[-<<+>>>+<] >[-<+>]<<]
                with self.variable() as tmp:
                    self._loop_start(left.index)
                    self._decrement()
                    self._goto(right.index)
                    self._move({evaluation_index, tmp.index})
                    self._goto(tmp.index)
                    self._move({right.index})
                    self._goto(left.index)
                    self._loop_end()
            elif expression.operation is BinaryOperator.SLASH:
                # >,>, >(tmp1) >(tmp2) >(tmp3) <<<<<[->->+ <[->>+>+<<<]>>>[-<<<+>>>] + <[>-<[-]] >[-<<<<<+>>>[-<+>]>>]<<<<]
                with self.variable() as tmp1, self.variable() as tmp2, self.variable() as tmp3:
                    self._loop_start(left.index)
                    self._decrement()
                    self._decrement(right.index)
                    self._increment(tmp1.index)
                    self._goto(right.index)
                    self._move({tmp2.index, tmp3.index})
                    self._goto(tmp3.index)
                    self._move({right.index})
                    self._increment()
                    self._loop_start(tmp2.index)
                    self._decrement(tmp3.index)
                    self._reset(tmp2.index)
                    self._loop_end()
                    self._loop_start(tmp3.index)
                    self._decrement()
                    self._goto(evaluation_index)
                    self._increment()
                    self._goto(tmp1.index)
                    self._move({right.index})
                    self._goto(tmp3.index)
                    self._loop_end()
                    self._goto(left.index)
                    self._loop_end()
            elif expression.operation is BinaryOperator.PERCENT:
                # Same as division, but result is in tmp1
                # >,>, >(tmp1) >(tmp2) >(tmp3) <<<<<[->->+ <[->>+>+<<<]>>>[-<<<+>>>] + <[>-<[-]] >[-<<<<<+>>>[-<+>]>>]<<<<]
                with self.variable() as tmp1, self.variable() as tmp2, self.variable() as tmp3:
                    self._loop_start(left.index)
                    self._decrement()
                    self._decrement(right.index)
                    self._goto(evaluation_index)
                    self._increment()
                    self._goto(right.index)
                    self._move({tmp1.index, tmp2.index})
                    self._goto(tmp2.index)
                    self._move({right.index})
                    self._increment()
                    self._loop_start(tmp1.index)
                    self._decrement(tmp2.index)
                    self._reset(tmp1.index)
                    self._loop_end()
                    self._loop_start(tmp2.index)
                    self._decrement()
                    self._increment(tmp3.index)
                    self._goto(evaluation_index)
                    self._move({right.index})
                    self._goto(tmp2.index)
                    self._loop_end()
                    self._goto(left.index)
                    self._loop_end()
            elif expression.operation is BinaryOperator.DOUBLE_DOT:
                # TODO: Evaluate operands here instead of copying them
                self._goto(evaluation_index)
                self.copy_variable(left)
                self._right(left.size())
                self.copy_variable(right)
            else:
                raise ImpossibleException(f'Unknown binary operation: {expression.operation!r}')
        self._goto(evaluation_index)

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
            case LiteralTuple(elements=elements):
                for element in elements:
                    self.evaluate(element)
                    self._right(element.type().size())
            case TypedIdentifier(location=location, name=name):
                variable = self.get_name(location, name)
                self.copy_variable(variable)
                self._comment(f'Copied {name}', CommentLevel.BZ_CODE_EXTENDED)
            case TypedArrayAccessExpression(array=array, index=array_index):
                evaluation_index = self.index
                with self.evaluate_in_new_variable(array) as tmp:
                    self._goto(tmp.index + array_index)
                    self._move({evaluation_index})
                    self._goto(evaluation_index)
            case InputCall():
                self.bf_code += ','
            case TypedFunctionCall(location=location, identifier=identifier, arguments=arguments):
                self.call(location, identifier, arguments, True)
            case TypedUnaryArithmeticExpression() as unary_arithmetic_expression:
                self.evaluate_unary_arithmetic_expression(unary_arithmetic_expression)
            case TypedBinaryArithmeticExpression() as binary_arithmetic_expression:
                self.evaluate_binary_arithmetic_expression(binary_arithmetic_expression)
            case _:
                raise ImpossibleException(f'Unknown expression type: {expression.__class__.__name__}')
        self._goto(index)

    def evaluate_in_new_variable(self, expression: TypedExpression) -> Name:
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
                        arg = self.scoped_variable(generate_unique_identifier(), argument.type())
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

    def call(self, location: Location, identifier: str, arguments: list[TypedExpression], expect_return: bool) -> None:
        """Call a subroutine and, if specified, get a return value"""
        # Get call context and subroutine
        call_index = self.index
        subroutine = self.get_subroutine(location, identifier)
        if expect_return and not subroutine.returns():
            raise CompilerException(f'Using procedure {identifier!r} as a function should have been caught earlier')
        # Test if the number of arguments is right
        arity = subroutine.arity()
        if arity != len(arguments):
            message = f'Subroutine {identifier!r} expects {arity} arguments, found {len(arguments)}'
            raise CompilationException(location, message)
        # First, evaluates the arguments
        self._comment(f'Preparing call to subroutine {identifier!r} (evaluating arguments)', prefix='\n')
        subroutine_origin = self._evaluate_arguments(arguments)
        self._goto(subroutine_origin)
        # Then, call the subroutine
        self._comment(f'Calling subroutine {identifier!r}', prefix='\n')
        self.bf_code += subroutine.generate()
        self.index += subroutine.index
        # Finally, get the return value (if required) and reset the cells to 0
        self._comment(f'Finalizing call to subroutine {identifier!r}', prefix='\n')
        if expect_return:
            return_index = self.index
            block_size = subroutine.return_type().size()
            self._reset(call_index, block_size=block_size)
            self._goto(return_index)
            self._move({call_index}, block_size=block_size)
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
                self._goto(index)
                self._move({destination}, block_size=target.type().size())
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
            with self.evaluate_in_new_variable(expression) as tmp:
                self._goto(tmp.index)
                for i in range(expression.type().size()):
                    self._output()
                    self._right()
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
            self._loop_start()
            self.compile_instruction(body)
            self.evaluate(test, condition.index)
            self._loop_end()

    def do_while_loop(self, body: TypeCheckedInstructionBlock, test: TypedExpression) -> None:
        with self.value(1) as condition:
            self._loop_start(condition.index)
            self.compile_instruction(body)
            self.evaluate(test, condition.index)
            self._loop_end()

    def for_loop(self, loop_type: DataType, loop_variable_identifier: str, loop_array: TypedExpression,
                 body: TypeCheckedInstructionBlock) -> None:
        array_type = loop_array.type()
        if not isinstance(array_type, ArrayType):
            raise ImpossibleException('For loop iterator is not an array (should have been caught earlier)')

        with (self.evaluate_in_new_variable(loop_array) as array,
              self.pointer(0, loop_type, loop_variable_identifier) as loop_variable):
            for i in range(array_type.count):
                loop_variable.update_index(array.index + i * array_type.base_type.size())
                self.compile_instruction(body)

    def condition(self, test: TypedExpression, if_body: TypeCheckedInstructionBlock,
                  else_body: TypeCheckedInstructionBlock) -> None:
        self._comment('Initializing conditional statement and evaluating condition', prefix='\n')
        with self.value(1) as has_entered_if, self.evaluate_in_new_variable(test) as condition:
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
        self.bf_code += '#'

        context = f'Currently at index {self.index} in subroutine {self.identifier()!r}'
        self._comment(context, CommentLevel.ONLY_REQUIRED, prefix='\n', end='')

        scopes = 'Active names: ' + str(self.names).replace(',', ';')
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
                variable = self.scoped_variable(identifier, variable_type)
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
            case TypeCheckedProcedureCall(location=location, identifier=identifier, arguments=arguments):
                self.call(location, identifier, arguments, False)
                comment_line = False
            case TypedExpression() as expression:
                self.evaluate(expression)
            case TypeCheckedLoopStatement(count=count, body=body):
                self.loop(count, body)
            case TypeCheckedWhileLoopStatement(test=test, body=body):
                self.while_loop(test, body)
            case TypeCheckedDoWhileLoopStatement(body=body, test=test):
                self.do_while_loop(body, test)
            case TypeCheckedForLoopStatement(loop_type=loop_type, loop_variable=loop_variable, loop_array=loop_array,
                                             body=body):
                self.for_loop(loop_type, loop_variable, loop_array, body)
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

    def generate(self) -> str:
        if not self.bf_code:
            if isinstance(self.subroutine, TypeCheckedNativeSubroutine):
                self.bf_code = self.subroutine.bf_code
                self.index = self.subroutine.offset
            elif isinstance(self.subroutine, TypeCheckedProcedure):
                self.compile_instruction(self.subroutine.body)
                self._goto(self.return_index)
            else:
                raise ImpossibleException(f'Unknown subroutine type: {self.subroutine.__class__.__name__}')
        return self.bf_code


def generate_program(namespace: TypeCheckedNamespace, main_procedure_identifier: str, *, verbose_level: int = 1) -> str:
    # Built in procedures
    procedures: dict[str, SubroutineCompiler] = {}
    # Get all elements
    for subroutine in namespace:
        if isinstance(subroutine, TypeCheckedSubroutine):
            procedures[subroutine.identifier] = SubroutineCompiler(subroutine, procedures.copy(), comment_level=verbose_level)
    # Generate code for main procedure
    if main_procedure_identifier not in procedures:
        raise CompilationException(namespace.location, f'{main_procedure_identifier!r} procedure not found')
    main = procedures[main_procedure_identifier]
    if main.arity() > 0:
        raise CompilationException(main.subroutine.location, 'Main procedure should not accept arguments')
    if main.returns():
        raise CompilationException(main.subroutine.location, 'Main subroutine should be a procedure')
    return main.generate().strip() + '\n'


__all__ = ['CommentLevel', 'generate_program']
