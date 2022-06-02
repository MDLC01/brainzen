from abc import ABC, abstractmethod
from typing import Generator

from data_types import *
from exceptions import *
from intermediate_representation.instructions import *
from operations import BinaryOperation, UnaryOperation
from type_checking.typed_assignment_targets import TypedAssignmentTarget
from type_checking.typing_context import SubroutineTypingContext


class TypeCheckedInstruction(ABC):
    @staticmethod
    def from_instruction(context: SubroutineTypingContext, instruction: Instruction) -> 'TypeCheckedInstruction':
        match instruction:
            case InstructionBlock() as instruction_block:
                return TypeCheckedInstructionBlock(context, instruction_block)
            case Expression() as expression:
                return TypedExpression.from_expression(context, expression)
            case ProcedureCall(identifier='print') as procedure_call:
                return PrintCall(context, procedure_call)
            case ProcedureCall(identifier='println') as procedure_call:
                return PrintCall(context, procedure_call, True)
            case FunctionCall(identifier='input') as function_call:
                return InputCall(function_call)
            case ProcedureCall() as procedure_call:
                return TypeCheckedProcedureCall(context, procedure_call)
            case Incrementation() as incrementation:
                return TypeCheckedIncrementation(context, incrementation)
            case Decrementation() as decrementation:
                return TypeCheckedDecrementation(context, decrementation)
            case VariableDeclaration() as variable_declaration:
                return TypeCheckedVariableDeclaration(context, variable_declaration)
            case Assignment() as assignment:
                return TypeCheckedAssignment(context, assignment)
            case LoopStatement() as loop_statement:
                return TypeCheckedLoopStatement(context, loop_statement)
            case WhileLoopStatement() as while_loop_statement:
                return TypeCheckedWhileLoopStatement(context, while_loop_statement)
            case ForLoopStatement() as for_loop_statement:
                return TypeCheckedForLoopStatement(context, for_loop_statement)
            case ConditionalStatement() as conditional_statement:
                return TypeCheckedConditionalStatement(context, conditional_statement)
            case ReturnInstruction() as return_instruction:
                return TypeCheckedReturnInstruction(context, return_instruction)
            case ContextSnapshot() as context_snapshot:
                return TypeCheckedContextSnapshot(context_snapshot)
        raise ImpossibleException(f'Unknown instruction type: {instruction.__class__!r}')

    def __init__(self, location: Location) -> None:
        self.location = location

    def may_return(self) -> bool:
        return False

    def may_break(self) -> bool:
        return False

    def early_exists(self) -> bool:
        return self.may_return() or self.early_exists()

    def __repr__(self) -> str:
        return self.__class__.__name__


class TypeCheckedInstructionBlock(TypeCheckedInstruction):
    def __init__(self, context: SubroutineTypingContext, instruction_block: InstructionBlock) -> None:
        super().__init__(instruction_block.location)
        self._may_return: bool = False
        self._may_break: bool = False
        # Type checking
        context.open_scope()
        self.instructions: list[TypeCheckedInstruction] = self._type_check(context, instruction_block.instructions)
        context.close_scope()

    def _type_check(self, context: SubroutineTypingContext,
                    instructions: list[Instruction]) -> list[TypeCheckedInstruction]:
        type_checked_instructions: list[TypeCheckedInstruction] = []
        for instruction in instructions:
            type_checked_instruction = TypeCheckedInstruction.from_instruction(context, instruction)
            if type_checked_instruction.may_return():
                self._may_return = True
            if type_checked_instruction.may_break():
                self._may_break = True
            type_checked_instructions.append(type_checked_instruction)
        return type_checked_instructions

    def may_return(self) -> bool:
        return self._may_return

    def may_break(self) -> bool:
        return self._may_break

    def is_empty(self) -> bool:
        return len(self.instructions) == 0

    def __len__(self) -> int:
        return len(self.instructions)

    def __iter__(self) -> Generator[TypeCheckedInstruction, None, None]:
        for instruction in self.instructions:
            yield instruction

    def __repr__(self, indent='') -> str:
        instructions = '{'
        if not self.is_empty():
            for instruction in self.instructions:
                instructions += f'\n{indent}    {instruction.__repr__()};'
            instructions += '\n' + indent + '}'
        else:
            instructions += '}'
        return instructions


class TypedExpression(TypeCheckedInstruction, ABC):
    """An expression whose type is known."""

    @staticmethod
    def from_expression(context: SubroutineTypingContext, expression: Expression) -> 'TypedExpression':
        match expression:
            case Char() as char:
                return LiteralChar.from_char(char)
            case Array() as array:
                return LiteralArray.from_array(context, array)
            case Tuple() as tuple_expression:
                return LiteralTuple.from_tuple_expression(context, tuple_expression)
            case Identifier() as identifier:
                return TypedIdentifier(context, identifier)
            case UnaryArithmeticExpression() as unary_arithmetic_expression:
                return TypedUnaryArithmeticExpression(context, unary_arithmetic_expression)
            case BinaryArithmeticExpression() as binary_arithmetic_expression:
                return TypedBinaryArithmeticExpression(context, binary_arithmetic_expression)
            case ArrayAccessExpression() as array_access_expression:
                return TypedArrayAccessExpression(context, array_access_expression)
            case FunctionCall(location=location, identifier='print'):
                raise CompilationException(location, f"Procedure 'print' does not return anything")
            case FunctionCall(location=location, identifier='println'):
                raise CompilationException(location, f"Procedure 'println' does not return anything")
            case FunctionCall(identifier='input') as function_call:
                return InputCall(function_call)
            case FunctionCall() as function_call:
                return TypedFunctionCall(context, function_call)
        raise ImpossibleException(f'Unknown expression type: {expression.__class__!r}')

    @abstractmethod
    def type(self) -> DataType:
        ...

    @abstractmethod
    def is_known_at_compile_time(self) -> bool:
        """Test if the value of this expression is known at compile time."""
        ...


class LiteralChar(TypedExpression):
    @classmethod
    def from_char(cls, char: Char) -> 'LiteralChar':
        return cls(char.location, char.value)

    def __init__(self, location: Location, value: int) -> None:
        super().__init__(location)
        self.value: int = value

    def type(self) -> DataType:
        return Types.CHAR

    def is_known_at_compile_time(self) -> bool:
        return True

    def __str__(self) -> str:
        return str(self.value)

    def __repr__(self) -> str:
        return f'{self.__class__.__name__}[{self.value!r}]'


class LiteralArray(TypedExpression):
    class EmptyType(DataType):
        def size(self) -> int:
            return 0

        def __eq__(self, other) -> bool:
            return isinstance(other, type(self))

        def __repr__(self) -> str:
            return repr(type(self))

    @classmethod
    def from_array(cls, context: SubroutineTypingContext, array: Array) -> 'LiteralArray':
        return cls(array.location, [TypedExpression.from_expression(context, expression) for expression in array.value])

    def __init__(self, location: Location, value: list[TypedExpression]) -> None:
        super().__init__(location)
        self.value: list[TypedExpression] = value
        self.content_type: DataType = LiteralArray.EmptyType()
        for element in self.value:
            if self.content_type == LiteralArray.EmptyType():
                self.content_type = element.type()
            elif element.type() != self.content_type:
                message = f'Expected element of type {self.content_type} but found {element.type()}'
                raise CompilationException(element.location, message)

    def __len__(self) -> int:
        return len(self.value)

    def type(self) -> DataType:
        return ArrayType(self.content_type, len(self.value))

    def is_known_at_compile_time(self) -> bool:
        return all(element.is_known_at_compile_time() for element in self.value)

    def __str__(self) -> str:
        # If the value is a constant string
        if self.type().is_string() and self.is_known_at_compile_time():
            s = '"'
            for element in self.value:
                if isinstance(element, LiteralChar):
                    s += chr(element.value)
                else:
                    s += str(element)
            return s + '"'
        # If the value is not a constant string
        elements = ', '.join(str(element) for element in self.value)
        return f'[{elements}]'

    def __repr__(self) -> str:
        return f'{self.__class__.__name__}[{self.value!r}]'


class LiteralTuple(TypedExpression):
    @classmethod
    def from_tuple_expression(cls, context: SubroutineTypingContext, tuple_expression: Tuple) -> 'LiteralTuple':
        return cls(tuple_expression.location,
                   [TypedExpression.from_expression(context, expression) for expression in tuple_expression.elements])

    def __init__(self, location: Location, elements: list[TypedExpression]) -> None:
        super().__init__(location)
        self.elements = elements
        self.types = [element.type() for element in elements]
        if len(self.elements) < 2:
            raise CompilerException('Tuple must be contain at least two elements')

    def type(self) -> DataType:
        return ProductType.from_operands(self.types)

    def is_known_at_compile_time(self) -> bool:
        return all(element.is_known_at_compile_time() for element in self.elements)

    def __str__(self) -> str:
        elements = ', '.join(str(element) for element in self.elements)
        return f'({elements})'

    def __repr__(self) -> str:
        return f'{self.__class__.__name__}[{self.elements!r}]'


class TypedIdentifier(TypedExpression):
    def __init__(self, context: SubroutineTypingContext, identifier: Identifier) -> None:
        super().__init__(identifier.location)
        self.name = identifier.name
        self._type: DataType = context.get_variable_type(self.location, self.name)

    def type(self) -> DataType:
        return self._type

    def is_known_at_compile_time(self) -> bool:
        return False

    def __str__(self) -> str:
        return self.name

    def __repr__(self) -> str:
        return f'{self.__class__.__name__}[{self.name}]'


class TypedArithmeticExpression(TypedExpression, ABC):
    def is_known_at_compile_time(self) -> bool:
        # Could be True in some situation, but it would require compile time evaluation of arithmetic
        return False


class TypedUnaryArithmeticExpression(TypedArithmeticExpression):
    def __init__(self, context: SubroutineTypingContext, expression: UnaryArithmeticExpression) -> None:
        super().__init__(expression.location)
        self.operation: UnaryOperation = expression.operation
        self.operand = TypedExpression.from_expression(context, expression.operand)
        # Type checking: all operations accept characters and return characters
        if self.operand.type() != Types.CHAR:
            message = f'Operand was expected to be a character, but found {self.operand.type()}'
            raise CompilationException(self.operand.location, message)
        self._type: DataType = Types.CHAR

    def type(self) -> DataType:
        return self._type

    def __str__(self) -> str:
        return f'({self.operation}{self.operand})'

    def __repr__(self) -> str:
        return f'{self.__class__.__name__}[{self.operation!r}, {self.operand!r}]'


class TypedBinaryArithmeticExpression(TypedArithmeticExpression):
    def __init__(self, context: SubroutineTypingContext, expression: BinaryArithmeticExpression) -> None:
        super().__init__(expression.location)
        self.operation: BinaryOperation = expression.operation
        self.left = TypedExpression.from_expression(context, expression.left)
        self.right = TypedExpression.from_expression(context, expression.right)
        # Type checking
        self._type: DataType = self._run_type_check()

    def _expect_left_operand(self, expected_type: DataType):
        if self.left.type() != expected_type:
            message = f'Left operand of {self.operation} was expected to be a character,' \
                      f' but found {self.left.type()}'
            raise CompilationException(self.left.location, message)

    def _expect_right_operand(self, expected_type: DataType):
        if self.right.type() != expected_type:
            message = f'Right operand of {self.operation} was expected to be a character,' \
                      f' but found {self.right.type()}'
            raise CompilationException(self.right.location, message)

    def _expect_both_operand(self, expected_type: DataType):
        self._expect_left_operand(expected_type)
        self._expect_right_operand(expected_type)

    def _run_type_check(self) -> DataType:
        if self.operation in (BinaryOperation.EQUALITY_TEST, BinaryOperation.DIFFERENCE_TEST):
            if self.right.type() != self.left.type():
                message = f'Expected right operand of {self.operation} to be of the same type as left operand' \
                          f' (expected {self.left.type()} but found {self.right.type()})'
                raise CompilationException(self.location, message)
            return Types.CHAR
        if self.operation in (BinaryOperation.STRICT_INEQUALITY_TEST,
                              BinaryOperation.LARGE_INEQUALITY_TEST,
                              BinaryOperation.INVERSE_STRICT_INEQUALITY_TEST,
                              BinaryOperation.INVERSE_LARGE_INEQUALITY_TEST,
                              BinaryOperation.CONJUNCTION,
                              BinaryOperation.DISJUNCTION,
                              BinaryOperation.ADDITION,
                              BinaryOperation.SUBTRACTION,
                              BinaryOperation.MULTIPLICATION,
                              BinaryOperation.DIVISION,
                              BinaryOperation.MODULO_OPERATION):
            self._expect_both_operand(Types.CHAR)
            return Types.CHAR
        if self.operation is BinaryOperation.CONCATENATION:
            left_type = self.left.type().get_actual_type()
            right_type = self.right.type().get_actual_type()
            if not isinstance(left_type, ArrayType):
                message = f'Expected left operand of {self.operation} to be an array, but found {left_type}'
                raise CompilationException(self.left.location, message)
            if not isinstance(right_type, ArrayType):
                message = f'Expected right operand of {self.operation} to be an array, but found {right_type}'
                raise CompilationException(self.right.location, message)
            if left_type.base_type != right_type.base_type:
                message = f'Expected left and right operands of {self.operation} to be arrays of the same type,' \
                          f' but found {left_type} and {right_type}'
                raise CompilationException(self.location, message)
            return ArrayType(left_type.base_type, left_type.count + right_type.count)
        raise ImpossibleException(f'Unknown operation: {self.operation}')

    def type(self) -> DataType:
        return self._type

    def __str__(self) -> str:
        return f'({self.left} {self.operation} {self.right})'

    def __repr__(self) -> str:
        return f'{self.__class__.__name__}[{self.operation!r}, {self.left!r}, {self.right!r}]'


class TypedArrayAccessExpression(TypedExpression):
    def __init__(self, context: SubroutineTypingContext, array_access_expression: ArrayAccessExpression) -> None:
        super().__init__(array_access_expression.location)
        self.array = TypedExpression.from_expression(context, array_access_expression.array)
        self.index = array_access_expression.index
        # Type checking
        array_type = self.array.type().get_actual_type()
        if not isinstance(array_type, ArrayType):
            raise CompilationException(self.location, f'Can not subscript {array_type}')
        # Negative indices start wrap around
        if self.index < 0:
            self.index = array_type.count - self.index
        if self.index < 0 or self.index >= array_type.count:
            raise CompilationException(self.location, f'Array index out of bounds')
        self._type = array_type.base_type

    def type(self) -> DataType:
        return self._type

    def is_known_at_compile_time(self) -> bool:
        return self.array.is_known_at_compile_time()

    def __str__(self) -> str:
        return f'{self.array}[{self.index}]'

    def __repr__(self) -> str:
        return f'{self.__class__.__name__}[{self.array!r}, {self.index!r}]'


class PrintCall(TypeCheckedInstruction):
    def __init__(self, context: SubroutineTypingContext, procedure_call: ProcedureCall, new_line: bool = False) -> None:
        super().__init__(procedure_call.location)
        self.new_line = new_line
        self.procedure = 'println' if new_line else 'print'
        # Type checking
        self.arguments: list[TypedExpression] = self._type_check_arguments(context, procedure_call.arguments)

    def _type_check_arguments(self, context: SubroutineTypingContext,
                              arguments: list[Expression]) -> list[TypedExpression]:
        typed_arguments: list[TypedExpression] = []
        for argument in arguments:
            typed_argument = TypedExpression.from_expression(context, argument)
            argument_type = typed_argument.type()
            if not argument_type.is_string():
                message = f'Procedure {self.procedure!r} only accepts string-like argument, but found {argument_type}'
                raise CompilationException(argument.location, message)
            typed_arguments.append(typed_argument)
        return typed_arguments

    def __str__(self) -> str:
        arguments = ', '.join(str(argument) for argument in self.arguments)
        return f'{self.procedure}({arguments})'

    def __repr__(self) -> str:
        arguments = ', '.join(repr(argument) for argument in self.arguments)
        return f'{self.__class__.__name__}[println={self.new_line}, {arguments}]'


class InputCall(TypedExpression):
    def __init__(self, procedure_call: ProcedureCall) -> None:
        super().__init__(procedure_call.location)
        # Type checking
        arity = len(procedure_call.arguments)
        if arity > 0:
            raise CompilationException(self.location, f"Function 'input' accepts 0 arguments, but found {arity}")

    def type(self) -> DataType:
        return Types.CHAR

    def is_known_at_compile_time(self) -> bool:
        return False

    def __str__(self) -> str:
        return f'input()'

    def __repr__(self) -> str:
        return f'{self.__class__.__name__}'


class TypeCheckedProcedureCall(TypeCheckedInstruction):
    def __init__(self, context: SubroutineTypingContext, procedure_call: ProcedureCall) -> None:
        super().__init__(procedure_call.location)
        self.identifier: str = procedure_call.identifier
        # Type checking
        self.arguments: list[TypedExpression] = self._type_check_arguments(context, procedure_call.arguments)

    def _type_check_arguments(self, context: SubroutineTypingContext,
                              arguments: list[Expression]) -> list[TypedExpression]:
        expected_types = context.get_subroutine_argument_types(self.location, self.identifier)
        if len(arguments) != len(expected_types):
            argument_count = f'{len(expected_types)} argument'
            if len(expected_types) > 1:
                argument_count += 's'
            message = f'Subroutine {self.identifier!r} accepts {argument_count}, but found {len(arguments)}'
            raise CompilationException(self.location, message)
        typed_arguments: list[TypedExpression] = []
        for i, argument in enumerate(arguments):
            typed_argument = TypedExpression.from_expression(context, argument)
            if typed_argument.type() != expected_types[i]:
                message = f'Argument {i} of subroutine {self.identifier!r} is expected to be of type' \
                          f' {expected_types[i]}, but found {typed_argument.type()}'
                raise CompilationException(typed_argument.location, message)
            typed_arguments.append(typed_argument)
        return typed_arguments

    def __str__(self) -> str:
        arguments = ', '.join(str(argument) for argument in self.arguments)
        return f'{self.identifier}({arguments})'

    def __repr__(self) -> str:
        if self.arguments:
            arguments = ', '.join(repr(argument) for argument in self.arguments)
            return f'{self.__class__.__name__}[{self.identifier}, {arguments}]'
        return f'{self.__class__.__name__}[{self.identifier}]'


class TypedFunctionCall(TypeCheckedProcedureCall, TypedExpression):
    def __init__(self, context: SubroutineTypingContext, function_call: FunctionCall) -> None:
        super().__init__(context, function_call)
        # Type checking
        self.return_type: DataType = context.get_function_return_type(function_call.location, function_call.identifier)

    def type(self) -> DataType:
        return self.return_type

    def is_known_at_compile_time(self) -> bool:
        return False


class TypeCheckedIncrementation(TypeCheckedInstruction):
    def __init__(self, context: SubroutineTypingContext, incrementation: Incrementation) -> None:
        super().__init__(incrementation.location)
        self.identifier = incrementation.identifier
        variable_type = context.get_variable_type(incrementation.location, self.identifier)
        # Type checking
        if variable_type != Types.CHAR:
            raise CompilationException(self.location, f'Can not increment {variable_type}')

    def __str__(self) -> str:
        return f'{self.identifier}++'

    def __repr__(self) -> str:
        return f'{self.__class__.__name__}[{self.identifier}]'


class TypeCheckedDecrementation(TypeCheckedInstruction):
    def __init__(self, context: SubroutineTypingContext, decrementation: Decrementation) -> None:
        super().__init__(decrementation.location)
        self.identifier = decrementation.identifier
        variable_type = context.get_variable_type(decrementation.location, self.identifier)
        # Type checking
        if variable_type != Types.CHAR:
            raise CompilationException(self.location, f'Can not decrement {variable_type}')

    def __str__(self) -> str:
        return f'{self.identifier}--'

    def __repr__(self) -> str:
        return f'{self.__class__.__name__}[{self.identifier}]'


class TypeCheckedVariableDeclaration(TypeCheckedInstruction):
    def __init__(self, context: SubroutineTypingContext, variable_declaration: VariableDeclaration) -> None:
        super().__init__(variable_declaration.location)
        self.identifier: str = variable_declaration.identifier
        self.type: DataType = variable_declaration.type
        # Type checking
        self.value: TypedExpression | None
        if variable_declaration.value is None:
            self.value = None
        else:
            self.value = TypedExpression.from_expression(context, variable_declaration.value)
            if not self.value.type() == self.type:
                raise CompilationException(self.value.location, f'Expected {self.type} but found {self.value.type()}')
        context.add_variable(self.location, self.identifier, self.type)

    def __str__(self) -> str:
        return f'let {self.type} {self.identifier} = {self.value}'

    def __repr__(self) -> str:
        return f'{self.__class__.__name__}[{self.identifier}, {self.type!r}, {self.value!r}]'


class TypeCheckedAssignment(TypeCheckedInstruction):
    def __init__(self, context: SubroutineTypingContext, assignment: Assignment) -> None:
        super().__init__(assignment.location)
        self.target = TypedAssignmentTarget.from_assignment_target(context, assignment.target)
        self.value = TypedExpression.from_expression(context, assignment.value)
        # Type checking
        if self.target.type() != self.value.type():
            raise CompilationException(self.location, f'Unable to assign {self.value.type()} to {self.target.type()}')

    def __str__(self) -> str:
        return f'{self.target} = {self.value}'

    def __repr__(self) -> str:
        return f'{self.__class__.__name__}[{self.target!r}, {self.value!r}]'


class TypeCheckedLoopStatement(TypeCheckedInstruction):
    def __init__(self, context: SubroutineTypingContext, loop_statement: LoopStatement) -> None:
        super().__init__(loop_statement.location)
        self.count: TypedExpression = TypedExpression.from_expression(context, loop_statement.count)
        self.body: TypeCheckedInstructionBlock = TypeCheckedInstructionBlock(context, loop_statement.body)
        # Type checking
        if not self.count.type().is_integer():
            raise CompilationException(self.count.location, f'Expected char but found {self.count.type()}')

    def may_return(self) -> bool:
        return self.body.may_return()

    def __str__(self) -> str:
        return f'loop ({self.count}) line {self.location.line}'

    def __repr__(self) -> str:
        return f'{self.__class__.__name__}[{self.count!r}, {self.body!r}]'


class TypeCheckedWhileLoopStatement(TypeCheckedInstruction):
    def __init__(self, context: SubroutineTypingContext, while_loop_statement: WhileLoopStatement) -> None:
        super().__init__(while_loop_statement.location)
        self.test: TypedExpression = TypedExpression.from_expression(context, while_loop_statement.test)
        self.body: TypeCheckedInstructionBlock = TypeCheckedInstructionBlock(context, while_loop_statement.body)
        # Type checking
        if self.test.type() != Types.CHAR:
            raise CompilationException(self.test.location, f'Expected char but found {self.test.type()}')

    def may_return(self) -> bool:
        return self.body.may_return()

    def __str__(self) -> str:
        return f'while ({self.test}) line {self.location.line}'

    def __repr__(self) -> str:
        return f'{self.__class__.__name__}[{self.test!r}, {self.body!r}]'


class TypeCheckedForLoopStatement(TypeCheckedInstruction):
    def __init__(self, context: SubroutineTypingContext, for_loop_statement: ForLoopStatement) -> None:
        super().__init__(for_loop_statement.location)
        self.loop_type = for_loop_statement.loop_type
        self.loop_variable = for_loop_statement.loop_variable
        # Type checking
        context.open_scope()
        # TODO: Specify a more precise location
        context.add_variable(self.location, self.loop_variable, self.loop_type)
        self.loop_array = TypedExpression.from_expression(context, for_loop_statement.loop_array)
        self.body: TypeCheckedInstructionBlock = TypeCheckedInstructionBlock(context, for_loop_statement.body)
        loop_array_type = self.loop_array.type()
        if not isinstance(loop_array_type, ArrayType) or loop_array_type.base_type != self.loop_type:
            message = f'Expected {self.loop_type} but found {self.loop_array.type()}'
            raise CompilationException(self.loop_array.location, message)
        context.close_scope()

    def may_return(self) -> bool:
        return self.body.may_return()

    def __str__(self) -> str:
        return f'for ({self.loop_type} {self.loop_variable} : {self.loop_array}) line {self.location.line}'

    def __repr__(self) -> str:
        return f'{self.__class__.__name__}[{self.loop_type!r}, {self.loop_variable!r}, {self.loop_array!r},' \
               f' {self.body!r}]'


class TypeCheckedConditionalStatement(TypeCheckedInstruction):
    def __init__(self, context: SubroutineTypingContext, statement: ConditionalStatement) -> None:
        super().__init__(statement.location)
        self.test: TypedExpression = TypedExpression.from_expression(context, statement.test)
        self.if_body: TypeCheckedInstructionBlock = TypeCheckedInstructionBlock(context, statement.if_body)
        self.else_body: TypeCheckedInstructionBlock = TypeCheckedInstructionBlock(context, statement.else_body)
        # Type checking
        if self.test.type() != Types.CHAR:
            raise CompilationException(self.test.location, f'Expected char but found {self.test.type()}')

    def may_return(self) -> bool:
        return self.if_body.may_return() or self.else_body.may_return()

    def may_break(self) -> bool:
        return self.if_body.may_break() or self.else_body.may_break()

    def has_else(self) -> bool:
        return not self.else_body.is_empty()

    def __str__(self) -> str:
        return f'if ({self.test}) line {self.location.line}'

    def __repr__(self) -> str:
        return f'{self.__class__.__name__}[{self.test!r}, {self.if_body!r}, {self.else_body!r}]'


class TypeCheckedReturnInstruction(TypeCheckedInstruction):
    def __init__(self, context: SubroutineTypingContext, instruction: ReturnInstruction) -> None:
        super().__init__(instruction.location)
        self.value: TypedExpression = TypedExpression.from_expression(context, instruction.value)
        # Type checking
        if context.expected_return_type is None:
            raise CompilationException(self.location, 'Unexpected return statement in procedure')
        if self.value.type() != context.expected_return_type:
            message = f'Expected {context.expected_return_type} but found {self.value.type()}'
            raise CompilationException(self.location, message)

    def may_return(self) -> bool:
        return True

    def __str__(self) -> str:
        return f'return {self.value}'

    def __repr__(self) -> str:
        return f'{self.__class__.__name__}[{self.value!r}]'


class TypeCheckedContextSnapshot(TypeCheckedInstruction):
    def __init__(self, context_snapshot: ContextSnapshot) -> None:
        super().__init__(context_snapshot.location)
        self.identifier = context_snapshot.identifier

    def __str__(self) -> str:
        if self.identifier is not None:
            return f'{self.identifier}?'
        return '?'

    def __repr__(self) -> str:
        return f'{self.__class__.__name__}'


__all__ = ['TypeCheckedInstruction', 'TypeCheckedInstructionBlock', 'TypedExpression', 'LiteralChar', 'LiteralArray',
           'LiteralTuple', 'TypedIdentifier', 'TypedArithmeticExpression', 'TypedUnaryArithmeticExpression',
           'TypedBinaryArithmeticExpression', 'TypedArrayAccessExpression', 'PrintCall', 'InputCall',
           'TypeCheckedProcedureCall', 'TypedFunctionCall', 'TypeCheckedIncrementation', 'TypeCheckedDecrementation',
           'TypeCheckedVariableDeclaration', 'TypeCheckedAssignment', 'TypeCheckedLoopStatement',
           'TypeCheckedWhileLoopStatement', 'TypeCheckedForLoopStatement', 'TypeCheckedConditionalStatement',
           'TypeCheckedReturnInstruction', 'TypeCheckedContextSnapshot']
