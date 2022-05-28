from enum import IntEnum

from data_types import *
from exceptions import *
from generation.memory_manager import *
from operations import *
from type_checking import *


class CommentLevel(IntEnum):
    ONLY_REQUIRED = 0
    BZ_CODE = 1
    BZ_CODE_EXTENDED = 2
    DETAILS = 3


class SubroutineCompiler(ScopeManager):
    """
    A subroutine is only allowed to change cells to the right of where it started, and it
    must set all values back to 0 after being called. It can expect cells to contain 0 by default.
    The return value of a function is defined by the position of the pointer when it ends.
    """

    def __init__(self, subroutine: TypeCheckedSubroutine, context: dict[str, 'SubroutineCompiler'], *,
                 comment_level: int = CommentLevel.BZ_CODE) -> None:
        super().__init__()
        self.context: dict[str, SubroutineCompiler] = context
        self.subroutine: TypeCheckedSubroutine = subroutine
        self.comment_level: int = comment_level
        self.bf_code: str = ''
        self.index: int = 0
        self._loop_start_indices: list[int] = []
        for argument in subroutine.arguments:
            self.register_argument(argument.identifier, argument.type)
        self.register_variable(Types.CHAR, '$tmp')
        self.tmp = self['$tmp']

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

    def register_variable(self, variable_type: DataType = Types.CHAR, identifier: str = None,
                          value: int | None = None) -> str:
        # Register identifier
        if identifier is None:
            identifier = generate_unique_identifier()
        self.register_identifier(identifier, variable_type)
        # Set the value of the variable if provided
        if value is not None:
            initial_index = self.index
            self._goto(self[identifier])
            self._set(value)
            self._goto(initial_index)
        # Return identifier
        return identifier

    def __delitem__(self, identifier: str) -> None:
        self._reset(self[identifier], block_size=self.size_of(identifier))
        super().__delitem__(identifier)

    def goto_variable(self, location: Location, identifier: str) -> None:
        """Move the pointer to the position of the passed variable."""
        if identifier not in self:
            raise CompilationException(location, f'Unknown variable: {identifier!r}')
        self._goto(self[identifier])

    def increment_variable(self, location: Location, identifier: str) -> None:
        """Increment the passed variable and move the pointer to its position"""
        self.goto_variable(location, identifier)
        self._increment()

    def decrement_variable(self, location: Location, identifier: str) -> None:
        """Decrement the passed variable and move the pointer to its position"""
        self.goto_variable(location, identifier)
        self._decrement()

    def reset_variable(self, location: Location, identifier: str) -> None:
        """Reset the passed variable to 0 and move the pointer to its position"""
        self.goto_variable(location, identifier)
        self._reset(block_size=self.size_of(identifier))

    def copy_variable(self, identifier: str) -> None:
        """Copy the value of the passed variable to the current location."""
        source = self[identifier]
        destination = self.index
        backup = self.tmp
        if source == destination:
            return
        size = self.size_of(identifier)
        # Reset destination
        self._reset(destination, block_size=size)
        # Copy variable cell by cell
        for i in range(size):
            self._reset(backup)
            self._goto(source + i)
            self._move({destination + i, backup})
            self._goto(backup)
            self._move({source + i})
        # Return to the destination cell
        self._goto(destination)

    def get_subroutine(self, location: Location, identifier: str) -> 'SubroutineCompiler':
        if identifier in self.context:
            return self.context[identifier]
        raise CompilationException(location, f'Unknown subroutine: {identifier!r}')

    def evaluate_unary_arithmetic_expression(self, expression: TypedUnaryArithmeticExpression) -> None:
        with self:
            evaluation_index = self.index
            # Evaluate operand
            operand = self.evaluate_in_new_variable(expression.operand)
            # Reset destination to 0
            self._reset(evaluation_index)
            # Comments show corresponding Brainfuck code, where the evaluation index is the starting index, and the
            # operand is stored at the index of the pointer when the comma is reached
            if expression.operation is UnaryOperation.BOOL_NORMALIZATION:
                # >,[<+>[-]]
                self._loop_start(self[operand])
                self._goto(evaluation_index)
                self._increment()
                self._reset(self[operand])
                self._loop_end()
            elif expression.operation is UnaryOperation.NEGATION:
                # >, <+>[<->[-]]
                self._goto(evaluation_index)
                self._increment()
                self._loop_start(self[operand])
                self._goto(evaluation_index)
                self._decrement()
                self._reset(self[operand])
                self._loop_end()
            elif expression.operation is UnaryOperation.OPPOSITION:
                # >, [-<->]
                self._loop_start(self[operand])
                self._decrement()
                self._goto(evaluation_index)
                self._decrement()
                self._goto(self[operand])
                self._loop_end()
            else:
                raise ImpossibleException(f'Unknown operation: {expression.operation!r}')
            # Finalize
            self._goto(evaluation_index)

    def evaluate_binary_arithmetic_expression(self, expression: TypedBinaryArithmeticExpression) -> None:
        """Evaluate the passed binary arithmetic expression in the current cell."""
        with self:
            evaluation_index = self.index
            # Evaluate operands
            left = self.evaluate_in_new_variable(expression.left)
            right = self.evaluate_in_new_variable(expression.right)
            # Reset destination to 0
            self._reset(evaluation_index)
            # Comments show corresponding Brainfuck code, where the evaluation index is the starting index, the
            # left operand is stored at the index of the pointer when the first comma is reached, and the right
            # operand is stored at the index of the pointer when the second comma is reached
            if expression.operation is BinaryOperation.EQUALITY_TEST:
                # >,>, [-<->] <<+>[<->[-]]
                # Subtract right from left
                self._loop_start(self[right])
                self._decrement()
                self._decrement(self[left])
                self._goto(self[right])
                self._loop_end()
                # Test if the result is zero
                self._goto(evaluation_index)
                self._increment()
                self._loop_start(self[left])
                self._goto(evaluation_index)
                self._decrement()
                self._reset(self[left])
                self._loop_end()
            elif expression.operation is BinaryOperation.DIFFERENCE_TEST:
                # >,>, [-<->] <[<+>[-]]
                # Subtract right from left
                self._loop_start(self[right])
                self._decrement()
                self._decrement(self[left])
                self._goto(self[right])
                self._loop_end()
                # Test if the result is non-zero
                self._loop_start(self[left])
                self._goto(evaluation_index)
                self._increment()
                self._reset(self[left])
                self._loop_end()
            elif expression.operation is BinaryOperation.STRICT_INEQUALITY_TEST:
                tmp1, tmp2 = self.require_memory(count=2)
                self._loop_start(self[right])
                # Duplicate right operand
                self._goto(self[left])
                self._move({self[tmp1], self[tmp2]})
                self._goto(self[tmp2])
                self._move({self[left]})
                self._increment(self[tmp2])
                # Do stuff
                self._loop_start(self[tmp1])
                self._decrement(self[right])
                self._decrement(self[left])
                self._decrement(self[tmp2])
                self._reset(self[tmp1])
                self._loop_end()
                # Do other stuff
                self._loop_start(self[tmp2])
                self._reset(self[right])
                self._goto(evaluation_index)
                self._increment()
                self._decrement(self[tmp2])
                self._loop_end()
                self._goto(self[right])
                self._loop_end()
            elif expression.operation is BinaryOperation.LARGE_INEQUALITY_TEST:
                # Compute !(l > r)
                tmp1, tmp2 = self.require_memory(count=2)
                self._loop_start(self[left])
                # Duplicate right operand
                self._goto(self[right])
                self._move({self[tmp1], self[tmp2]})
                self._goto(self[tmp2])
                self._move({self[right]})
                self._increment(self[tmp2])
                # Do stuff
                self._loop_start(self[tmp1])
                self._decrement(self[left])
                self._decrement(self[right])
                self._decrement(self[tmp2])
                self._reset(self[tmp1])
                self._loop_end()
                # Do other stuff
                self._loop_start(self[tmp2])
                self._reset(self[left])
                self._increment(self[tmp1])
                self._decrement(self[tmp2])
                self._loop_end()
                self._goto(self[left])
                self._loop_end()
                # Negate result
                self._goto(evaluation_index)
                self._increment()
                self._loop_start(self[tmp1])
                self._decrement(self[tmp1])
                self._goto(evaluation_index)
                self._decrement()
                self._goto(self[tmp1])
                self._loop_end()
            elif expression.operation is BinaryOperation.INVERSE_STRICT_INEQUALITY_TEST:
                tmp1, tmp2 = self.require_memory(count=2)
                self._loop_start(self[left])
                # Duplicate right operand
                self._goto(self[right])
                self._move({self[tmp1], self[tmp2]})
                self._goto(self[tmp2])
                self._move({self[right]})
                self._increment(self[tmp2])
                # Do stuff
                self._loop_start(self[tmp1])
                self._decrement(self[left])
                self._decrement(self[right])
                self._decrement(self[tmp2])
                self._reset(self[tmp1])
                self._loop_end()
                # Do other stuff
                self._loop_start(self[tmp2])
                self._reset(self[left])
                self._goto(evaluation_index)
                self._increment()
                self._decrement(self[tmp2])
                self._loop_end()
                self._goto(self[left])
                self._loop_end()
            elif expression.operation is BinaryOperation.INVERSE_LARGE_INEQUALITY_TEST:
                # Compute !(l < r)
                tmp1, tmp2 = self.require_memory(count=2)
                self._loop_start(self[right])
                # Duplicate right operand
                self._goto(self[left])
                self._move({self[tmp1], self[tmp2]})
                self._goto(self[tmp2])
                self._move({self[left]})
                self._increment(self[tmp2])
                # Do stuff
                self._loop_start(self[tmp1])
                self._decrement(self[right])
                self._decrement(self[left])
                self._decrement(self[tmp2])
                self._reset(self[tmp1])
                self._loop_end()
                # Do other stuff
                self._loop_start(self[tmp2])
                self._reset(self[right])
                self._increment(self[tmp1])
                self._decrement(self[tmp2])
                self._loop_end()
                self._goto(self[right])
                self._loop_end()
                # Negate result
                self._goto(evaluation_index)
                self._increment()
                self._loop_start(self[tmp1])
                self._decrement(self[tmp1])
                self._goto(evaluation_index)
                self._decrement()
                self._goto(self[tmp1])
                self._loop_end()
            elif expression.operation is BinaryOperation.CONJUNCTION:
                # There might be a better way
                # >,>, >++(tmp1) >+(tmp2) <<<[>>-<<[-]] >[>-<[-]] >[>-<[-]] >[<<<<+>>>>[-]]
                tmp1 = self.register_variable(Types.CHAR, value=2)
                tmp2 = self.register_variable(Types.CHAR, value=1)
                # Test if the left operand is true
                self._loop_start(self[left])
                self._decrement(self[tmp1])
                self._reset(self[left])
                self._loop_end()
                # Test if the right operand is true
                self._loop_start(self[right])
                self._decrement(self[tmp1])
                self._reset(self[right])
                self._loop_end()
                # Test if one of the operand is true
                self._loop_start(self[tmp1])
                self._decrement(self[tmp2])
                self._reset(self[tmp1])
                self._loop_end()
                # Return result
                self._loop_start(self[tmp2])
                self._goto(evaluation_index)
                self._increment()
                self._reset(self[tmp2])
                self._loop_end()
            elif expression.operation is BinaryOperation.DISJUNCTION:
                # There might be a better way
                # >,>, >+(tmp) <<[>>[-]<<-<+>] >>[<[-<<+>>]>[-]]
                tmp = self.register_variable(Types.CHAR, value=1)
                # If the left operand is truthy, its value is copied
                self._loop_start(self[left])
                self._reset(self[tmp])
                self._decrement(self[left])
                self._goto(evaluation_index)
                self._increment()
                self._goto(self[left])
                self._loop_end()
                # Else, the value of the right operand is copied
                self._loop_start(self[tmp])
                self._goto(self[right])
                self._move({evaluation_index})
                self._reset(self[tmp])
                self._loop_end()
            elif expression.operation is BinaryOperation.ADDITION:
                # >,>, <[-<+>] >[-<<+>>]
                self._goto(self[left])
                self._move({evaluation_index})
                self._loop_start(self[right])
                self._decrement()
                self._goto(evaluation_index)
                self._increment()
                self._goto(self[right])
                self._loop_end()
            elif expression.operation is BinaryOperation.SUBTRACTION:
                # >,>, <[-<+>] >[-<<->>]
                self._goto(self[left])
                self._move({evaluation_index})
                self._loop_start(self[right])
                self._decrement()
                self._goto(evaluation_index)
                self._decrement()
                self._goto(self[right])
                self._loop_end()
            elif expression.operation is BinaryOperation.MULTIPLICATION:
                # >,>, >(tmp) <<[- >[-<<+>>>+<] >[-<+>]<<]
                tmp = self.register_variable()
                self._loop_start(self[left])
                self._decrement()
                self._goto(self[right])
                self._move({evaluation_index, self[tmp]})
                self._goto(self[tmp])
                self._move({self[right]})
                self._goto(self[left])
                self._loop_end()
            elif expression.operation is BinaryOperation.DIVISION:
                # >,>, >(tmp1) >(tmp2) >(tmp3) <<<<<[->->+ <[->>+>+<<<]>>>[-<<<+>>>] + <[>-<[-]] >[-<<<<<+>>>[-<+>]>>]<<<<]
                tmp1, tmp2, tmp3 = self.require_memory(count=3)
                self._loop_start(self[left])
                self._decrement()
                self._decrement(self[right])
                self._increment(self[tmp1])
                self._goto(self[right])
                self._move({self[tmp2], self[tmp3]})
                self._goto(self[tmp3])
                self._move({self[right]})
                self._increment()
                self._loop_start(self[tmp2])
                self._decrement(self[tmp3])
                self._reset(self[tmp2])
                self._loop_end()
                self._loop_start(self[tmp3])
                self._decrement()
                self._goto(evaluation_index)
                self._increment()
                self._goto(self[tmp1])
                self._move({self[right]})
                self._goto(self[tmp3])
                self._loop_end()
                self._goto(self[left])
                self._loop_end()
            elif expression.operation is BinaryOperation.MODULO_OPERATION:
                # Same as division, but result is in tmp1
                # >,>, >(tmp1) >(tmp2) >(tmp3) <<<<<[->->+ <[->>+>+<<<]>>>[-<<<+>>>] + <[>-<[-]] >[-<<<<<+>>>[-<+>]>>]<<<<]
                tmp1, tmp2, tmp3 = self.require_memory(count=3)
                self._loop_start(self[left])
                self._decrement()
                self._decrement(self[right])
                self._goto(evaluation_index)
                self._increment()
                self._goto(self[right])
                self._move({self[tmp1], self[tmp2]})
                self._goto(self[tmp2])
                self._move({self[right]})
                self._increment()
                self._loop_start(self[tmp1])
                self._decrement(self[tmp2])
                self._reset(self[tmp1])
                self._loop_end()
                self._loop_start(self[tmp2])
                self._decrement()
                self._increment(self[tmp3])
                self._goto(evaluation_index)
                self._move({self[right]})
                self._goto(self[tmp2])
                self._loop_end()
                self._goto(self[left])
                self._loop_end()
            elif expression.operation is BinaryOperation.CONCATENATION:
                # TODO: Evaluate operands here instead of copying them
                self._goto(evaluation_index)
                self.copy_variable(left)
                self._right(self.size_of(left))
                self.copy_variable(right)
            else:
                raise ImpossibleException(f'Unknown operation: {expression.operation!r}')
            # Finalize
            self._goto(evaluation_index)

    def evaluate_arithmetic_expression(self, expression: TypedArithmeticExpression) -> None:
        """Evaluate the passed arithmetic expression at the current location."""
        match expression:
            case TypedUnaryArithmeticExpression() as unary_arithmetic_expression:
                self.evaluate_unary_arithmetic_expression(unary_arithmetic_expression)
            case TypedBinaryArithmeticExpression() as binary_arithmetic_expression:
                self.evaluate_binary_arithmetic_expression(binary_arithmetic_expression)
            case _:
                raise ImpossibleException(f'Unknown arithmetic expression type: {expression.__class__}')

    def evaluate(self, expression: TypedExpression) -> None:
        """Evaluate the passed expression at the current location. Does not move the cursor."""
        index = self.index
        match expression:
            case LiteralChar(value=value):
                self._set(value)
            case LiteralArray(value=value):
                for element in value:
                    self.evaluate(element)
                    self._right(element.type().size())
            case TypedIdentifier(name=name):
                self.copy_variable(name)
                self._comment(f'Copied {name}', CommentLevel.BZ_CODE_EXTENDED)
            case TypedArrayAccessExpression(array=array, index=index):
                evaluation_index = self.index
                with self:
                    tmp = self.evaluate_in_new_variable(array)
                    self._goto(self[tmp] + index)
                    self._move({evaluation_index})
                    self._goto(evaluation_index)
            case InputCall():
                self.bf_code += ','
            case TypedFunctionCall(location=location, identifier=identifier, arguments=arguments):
                self.call(location, identifier, arguments, True)
            case TypedArithmeticExpression() as arithmetic_expression:
                self.evaluate_arithmetic_expression(arithmetic_expression)
            case _:
                raise ImpossibleException(f'Unknown expression type: {expression.__class__.__name__}')
        self._goto(index)

    def evaluate_in_new_variable(self, expression: TypedExpression) -> str:
        variable = self.register_variable(expression.type())
        self._goto(self[variable])
        self.evaluate(expression)
        return variable

    def _evaluate_arguments(self, arguments: list[TypedExpression]) -> int:
        """Evaluate passed arguments and return subroutine origin."""
        with self:
            # Start by evaluating complex arguments (arguments that are not just literals and identifiers)
            evaluated_arguments: list[int | str] = []
            for argument in arguments:
                match argument:
                    case LiteralChar(value=value):
                        evaluated_arguments.append(value)
                    case TypedIdentifier(name=name):
                        evaluated_arguments.append(name)
                    case _:
                        tmp = self.evaluate_in_new_variable(argument)
                        evaluated_arguments.append(tmp)
            # Then copy everything at the right place
            subroutine_origin = self.memory_size()
            self._goto(subroutine_origin)
            for i, evaluated_argument in enumerate(evaluated_arguments):
                if isinstance(evaluated_argument, int):
                    self._set(evaluated_argument)
                    self._right()
                else:
                    self.copy_variable(evaluated_argument)
                    self._right(self.size_of(evaluated_argument))
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

    def print(self, values: list[TypedExpression], *, new_line: bool = False) -> None:
        for expression in values:
            if not expression.type().is_string():
                raise CompilationException(expression.location, f'Expected string but found {expression.type()}')
            with self:
                # Evaluate expression
                tmp = self.evaluate_in_new_variable(expression)
                # Print expression
                self._goto(self[tmp])
                for i in range(expression.type().size()):
                    self._output()
                    self._right()
        if new_line:
            with self:
                tmp = self.register_variable(Types.CHAR)
                self._goto(self[tmp])
                self._set(ord('\n'))
                self._output()

    def loop(self, count: TypedExpression, body: TypeCheckedInstructionBlock) -> None:
        with self:
            # Initialize loop counter
            self._comment('Initializing loop counter', prefix='\n')
            counter = self.evaluate_in_new_variable(count)
            # Loop body
            self._comment('Starting loop', prefix='\n')
            self._loop_start(self[counter])
            self.compile_instruction(body)
            self._decrement(self[counter])
            self._loop_end()
            # End loop
            self._comment(f'Ending loop', prefix='\n')

    def while_loop(self, test: TypedExpression, body: TypeCheckedInstructionBlock) -> None:
        with self:
            condition = self.evaluate_in_new_variable(test)
            self._loop_start()
            self.compile_instruction(body)
            self._goto(self[condition])
            self.evaluate(test)
            self._loop_end()

    def for_loop(self, loop_type: DataType, loop_variable: str, loop_array: TypedExpression,
                 body: TypeCheckedInstructionBlock) -> None:
        array_type = loop_array.type()
        if not isinstance(array_type, ArrayType):
            raise ImpossibleException('For loop iterator is not an array (should have been caught earlier)')

        with self:
            array = self.evaluate_in_new_variable(loop_array)
            self.register_pointer(loop_variable, loop_type, 0)
            for i in range(array_type.count):
                with self:
                    self.update_pointer(loop_variable, self[array] + i * array_type.base_type.size())
                    self.compile_instruction(body)

    def condition(self, test: TypedExpression, if_body: TypeCheckedInstructionBlock,
                  else_body: TypeCheckedInstructionBlock) -> None:
        self._comment('Initializing conditional statement', prefix='\n')
        with self:
            # This variable is used for the else statement
            has_entered_if = self.register_variable(Types.CHAR, value=1)
            # This is where the condition is evaluated
            self._comment('Evaluating condition', prefix='\n')
            condition = self.evaluate_in_new_variable(test)
            self._comment('Starting if', prefix='\n')
            # If body
            with self:
                self._loop_start()
                self.compile_instruction(if_body)
                self._decrement(self[has_entered_if])  # Reset
                self._reset(self[condition])
                self._loop_end()
            # Else body
            with self:
                self._loop_start(self[has_entered_if])
                self.compile_instruction(else_body)
                self._decrement(self[has_entered_if])  # Reset
                self._loop_end()

    def create_context_snapshot(self) -> None:
        self._comment('=== CONTEXT SNAPSHOT ===', CommentLevel.ONLY_REQUIRED, prefix='\n')

        self.bf_code += '#'

        context = f'Currently at index {self.index} in subroutine {self.identifier()!r}'
        self._comment(context, CommentLevel.ONLY_REQUIRED, prefix='\n', end='')

        scopes = 'Active scopes: ' + str(self.scopes).replace(',', ';')
        self._comment(scopes, CommentLevel.ONLY_REQUIRED, prefix='\n', end='')

        local_memory_overview = f'Local memory size: {self.memory_size()}'
        self._comment(local_memory_overview, CommentLevel.ONLY_REQUIRED, prefix='\n', end='')

        self._comment('=== CONTEXT SNAPSHOT END ===', CommentLevel.ONLY_REQUIRED, prefix='\n')

    def compile_instruction(self, instruction: TypeCheckedInstruction) -> None:
        comment_line = True
        match instruction:
            case TypeCheckedInstructionBlock(instructions=instructions):
                with self:
                    for instruction in instructions:
                        self.compile_instruction(instruction)
            case TypeCheckedVariableDeclaration(identifier=identifier, type=variable_type, value=expression):
                self.register_variable(variable_type, identifier)
                if expression is not None:
                    self._goto(self[identifier])
                    self.evaluate(expression)
            case TypeCheckedIncrementation(location=location, identifier=identifier):
                self.increment_variable(location, identifier)
            case TypeCheckedDecrementation(location=location, identifier=identifier):
                self.decrement_variable(location, identifier)
            case TypeCheckedAssignment(location=location, identifier=identifier, value=expression):
                self.goto_variable(location, identifier)
                self.evaluate(expression)
            case TypeCheckedArrayAssignment(location=location, identifier=identifier, offset=offset, value=expression):
                self.goto_variable(location, identifier)
                self._right(offset)
                self.evaluate(expression)
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
            case TypeCheckedForLoopStatement(loop_type=loop_type, loop_variable=loop_variable, loop_array=loop_array,
                                             body=body):
                self.for_loop(loop_type, loop_variable, loop_array, body)
            case TypeCheckedConditionalStatement(test=test, if_body=if_body, else_body=else_body):
                self.condition(test, if_body, else_body)
            case TypeCheckedReturnInstruction(value=expression):
                self.evaluate(expression)
            case TypeCheckedContextSnapshot(location=location):
                self.create_context_snapshot()
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
                for instruction in self.subroutine.body:
                    self.compile_instruction(instruction)
            else:
                raise ImpossibleException(f'Unknown subroutine type: {self.subroutine.__class__.__name__}')
        return self.bf_code


def generate_program(namespace: TypeCheckedNamespace, main_procedure_identifier: str, *, verbose_level: int = 1) -> str:
    # Built in procedures
    procedures: dict[str, SubroutineCompiler] = {}
    # Get all elements
    for element in namespace:
        if isinstance(element, TypeCheckedSubroutine):
            procedures[element.identifier] = SubroutineCompiler(element, procedures.copy(), comment_level=verbose_level)
        else:
            raise ImpossibleException(f'Unknown element type: {element.__class__.__name__}')
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
