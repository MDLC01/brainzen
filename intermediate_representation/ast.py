from abc import ABC, abstractmethod
from typing import Generator, Type, TypeVar

from data_types import *
from exceptions import *
from intermediate_representation.instructions import *
from operations import *
from tokenization.tokens import *


class NamespaceElement(ABC):
    __slots__ = 'location', 'identifier'

    def __init__(self, location: Location, identifier: str) -> None:
        self.location = location
        self.identifier = identifier

    @abstractmethod
    def __str__(self) -> str:
        ...

    @abstractmethod
    def __repr__(self, indent: str = '') -> str:
        ...


class SubroutineArgument:
    __slots__ = 'location', 'identifier', 'type'

    def __init__(self, location: Location, identifier: str, variable_type: DataType) -> None:
        self.location = location
        self.identifier = identifier
        self.type = variable_type

    def __repr__(self) -> str:
        return f'{self.type} {self.identifier}'


class Procedure(NamespaceElement):
    __slots__ = 'arguments', 'body'

    def __init__(self, location: Location, identifier: str, arguments: list[SubroutineArgument],
                 body: InstructionBlock) -> None:
        super().__init__(location, identifier)
        self.arguments = arguments
        self.body = body

    def _argument_string(self) -> str:
        return ', '.join(repr(argument) for argument in self.arguments)

    def __str__(self) -> str:
        return f'proc {self.identifier}({self._argument_string()}) line {self.location.line}'

    def __repr__(self, indent: str = '') -> str:
        return f'({self._argument_string()}) -> {self.body.__repr__(indent)}'


class Function(Procedure):
    __slots__ = 'return_type'

    def __init__(self, location: Location, identifier: str, arguments: list[SubroutineArgument], body: InstructionBlock,
                 return_type: DataType) -> None:
        super().__init__(location, identifier, arguments, body)
        self.return_type = return_type

    def __str__(self) -> str:
        return f'func {self.identifier}({self._argument_string()}) -> {self.return_type} line {self.location.line}'


class Namespace:
    def __init__(self, location: Location, identifier: str | None = None) -> None:
        self.location = location
        if identifier is None:
            identifier = '<' + location.file + '>'
        self.identifier = identifier
        self.elements: list[NamespaceElement] = []
        self.subroutine_locations: dict[str, Location] = {}

    def get_base_type(self, location: Location, identifier: str) -> DataType:
        return PrimitiveType.of(location, identifier)

    def is_valid_base_type(self, identifier: str) -> bool:
        return PrimitiveType.is_valid_base_type(identifier)

    def add_subroutine(self, subroutine: Procedure) -> None:
        identifier = subroutine.identifier
        if identifier in self.subroutine_locations:
            message = f'Subroutine {identifier!r} is already defined at {self.subroutine_locations[identifier]}'
            raise CompilationException(subroutine.location, message)
        self.subroutine_locations[identifier] = subroutine.location
        self.elements.append(subroutine)

    def __iter__(self) -> Generator[NamespaceElement, None, None]:
        for element in self.elements:
            yield element

    def __repr__(self) -> str:
        indent = '    '
        s = f'{self.__class__.__name__}['
        if self.elements:
            s += '\n'
        for element in self.elements:
            s += f'{indent}{element.identifier} = {element.__repr__(indent)},\n'
        return s + ']'


class Constant:
    """
    Temporary hack to evaluate constants at compile time.
    This is NOT meant to be definitive.
    TODO: Rewrite this class.
    """

    @staticmethod
    def _evaluate_unary_operation(operation: UnaryOperation, operand: int | list) -> int | list:
        if operation is UnaryOperation.BOOL_NORMALIZATION:
            return int(bool(operand))
        if operation is UnaryOperation.NEGATION:
            return int(not operand)
        if operation is UnaryOperation.OPPOSITION:
            return -operand
        raise ImpossibleException(f'Unknown operation: {operation}')

    @staticmethod
    def _evaluate_binary_operation(operation: BinaryOperation, left: int | list, right: int | list) -> int | list:
        if operation is BinaryOperation.EQUALITY_TEST:
            return left == right
        if operation is BinaryOperation.DIFFERENCE_TEST:
            return left != right
        if operation is BinaryOperation.STRICT_INEQUALITY_TEST:
            return left < right
        if operation is BinaryOperation.LARGE_INEQUALITY_TEST:
            return left <= right
        if operation is BinaryOperation.INVERSE_STRICT_INEQUALITY_TEST:
            return left > right
        if operation is BinaryOperation.INVERSE_LARGE_INEQUALITY_TEST:
            return left >= right
        if operation is BinaryOperation.CONJUNCTION:
            return int(bool(left and right))
        if operation is BinaryOperation.DISJUNCTION:
            return int(bool(left or right))
        if operation is BinaryOperation.ADDITION:
            return left + right
        if operation is BinaryOperation.SUBTRACTION:
            return left - right
        if operation is BinaryOperation.MULTIPLICATION:
            return left * right
        if operation is BinaryOperation.DIVISION:
            return left // right
        if operation is BinaryOperation.MODULO_OPERATION:
            return left % right
        if operation is BinaryOperation.CONCATENATION:
            return left + right
        raise ImpossibleException(f'Unknown operation: {operation}')

    @classmethod
    def _evaluate(cls, expression: Expression) -> Char | Array:
        location = expression.location
        match expression:
            case Char() as value:
                return value
            case Array() as value:
                return value
            case UnaryArithmeticExpression(operation=operation, operand=operand):
                value = cls._evaluate(operand).value
                return Char(location, cls._evaluate_unary_operation(operation, value))
            case BinaryArithmeticExpression(operation=operation, left=left, right=right):
                left_value = cls._evaluate(left).value
                right_value = cls._evaluate(right).value
                result = cls._evaluate_binary_operation(operation, left_value, right_value)
                if isinstance(result, int):
                    return Char(location, result)
                if isinstance(result, list):
                    return Array(location, result)
                raise ImpossibleException(f'Compile time evaluation of {expression} failed')
            case _:
                message = 'Invalid constant: A constant must be evaluable at compile time'
                raise CompilationException(expression.location, message)

    def __init__(self, location: Location, identifier: str, expression: Expression) -> None:
        self.location = location
        self.identifier = identifier
        self.value = Constant._evaluate(expression)

    def type(self) -> DataType:
        return self.value.type()


class ASTGenerator:
    _T = TypeVar('_T')

    def __init__(self, file: Location, tokens: list[Token]) -> None:
        self.index = 0
        self.tokens = tokens
        self.constants: dict[str, Constant] = {}
        self.file = file
        self.namespace = Namespace(self.file)

    def _location(self) -> Location:
        return self.tokens[self.index].location

    def _previous_location(self) -> Location:
        return self.tokens[self.index - 1].location

    def _location_from(self, location: Location) -> Location:
        return location.extend_to(self._previous_location())

    def _has_next(self) -> bool:
        """Test if there is a token next or if the end of the file has been reached"""
        # return self.index < len(self.tokens)
        return not isinstance(self.tokens[self.index], EOFToken)

    def _peek(self) -> AnyToken:
        """Return the next token without advancing the cursor"""
        return self.tokens[self.index]

    def _is_next(self, token_type: Type[AnyToken]) -> bool:
        """Test if the next token if of the passed token type"""
        return isinstance(self._peek(), token_type)

    def _next(self) -> AnyToken:
        """Return the next token and advance the cursor"""
        token = self._peek()
        self.index += 1
        return token

    def _expect(self, token_type: Type[_T]) -> _T:
        """Return the next token and advance the cursor; raise an error if the next token is not of the passed type"""
        if not self._is_next(token_type):
            message = f'Expected {token_type.__doc__} but found {self._peek().__doc__}'
            raise CompilationException(self._location(), message)
        return self._next()

    def _eat(self, token_type: Type[AnyToken]) -> bool:
        """Advance the cursor if the next token is of the passed type"""
        if self._is_next(token_type):
            self._next()
            return True
        return False

    def parse_sequence(self) -> list[Expression]:
        elements = []
        # TODO: Rewrite that to not depend on a specific set of closing parenthesis
        if self._is_next(CloseParToken) or self._is_next(CloseBracketToken) or self._is_next(CloseBraceToken):
            return elements
        while True:
            if elements and not self._eat(CommaToken):
                break
            elements.append(self.parse_expression())
        return elements

    def parse_operand_start(self) -> Expression:
        match self._next():
            # Parenthesis
            case OpenParToken():
                expression = self.parse_expression()
                self._expect(CloseParToken)
                return expression
            # Bool normalization
            case DoubleBangToken(location=location):
                return UnaryArithmeticExpression(location, UnaryOperation.BOOL_NORMALIZATION, self.parse_operand())
            # Logical not
            case BangToken(location=location):
                return UnaryArithmeticExpression(location, UnaryOperation.NEGATION, self.parse_operand())
            # Minus sign
            case MinusToken(location=location):
                return UnaryArithmeticExpression(location, UnaryOperation.OPPOSITION, self.parse_operand())
            # Constant
            case HashToken(location=location):
                identifier = self._expect(IdentifierToken).name
                if identifier not in self.constants:
                    raise CompilationException(location, f'Unknown constant: {identifier!r}')
                return self.constants[identifier].value
            # Number literal
            case NumberLiteral(location=location, value=value):
                return Char(location, value)
            # String literal
            case StringLiteral(location=location, value=value):
                return Array.from_string(location, value)
            # Array literal
            case OpenBracketToken(location=location):
                array = Array(location, self.parse_sequence())
                self._expect(CloseBracketToken)
                return array
            # Identifier
            case IdentifierToken(location=location, name=name):
                # Function call
                if self._eat(OpenParToken):
                    arguments = self.parse_sequence()
                    self._expect(CloseParToken)
                    return FunctionCall(location, name, arguments)
                # Value
                return Identifier(location, name)
            case Token(location=location) as token:
                raise CompilationException(location, f'Unexpected token: {token.__doc__}')

    def parse_operand(self) -> Expression:
        operand = self.parse_operand_start()
        while self._is_next(OpenBracketToken):
            location = self._next().location
            index = self._expect(NumberLiteral)
            close_bracket_location = self._expect(CloseBracketToken).location
            location = location.extend_to(close_bracket_location)
            operand = ArrayAccessExpression(location, operand, index.value)
        return operand

    def parse_expression(self) -> Expression:
        left = self.parse_operand()
        while self._peek().is_binary_operation() or self._is_next(OpenBracketToken):
            location = self._location()
            operation = self._next().binary_operation
            right = self.parse_operand()
            if isinstance(left, BinaryArithmeticExpression) and left.operation.priority < operation.priority:
                left.right = BinaryArithmeticExpression(location, operation, left.right, right)
            else:
                left = BinaryArithmeticExpression(location, operation, left, right)
        return left

    def parse_instruction(self) -> Instruction:
        """Parse an instruction (does not expect a semicolon at the end)"""
        if self._is_next(IdentifierToken):
            identifier = self._expect(IdentifierToken)
            start_location = identifier.location
            # Variable declaration
            if self.namespace.is_valid_base_type(identifier.name):
                self.index -= 1
                variable_type = self.parse_type()
                variable_name = self._expect(IdentifierToken).name
                if self._eat(EqualToken):
                    value = self.parse_expression()
                    return VariableDeclaration(self._location_from(start_location), variable_name, variable_type, value)
                return VariableDeclaration(self._location_from(start_location), variable_name, variable_type)
            # Incrementation
            if self._eat(DoublePlusToken):
                return Incrementation(self._location_from(start_location), identifier.name)
            # Decrementation
            if self._eat(DoubleMinusToken):
                return Decrementation(self._location_from(start_location), identifier.name)
            # Assignment
            if self._eat(EqualToken):
                value = self.parse_expression()
                return Assignment(self._location_from(start_location), identifier.name, value)
            # Array assignment
            if self._is_next(OpenBracketToken):
                indices = []
                while self._eat(OpenBracketToken):
                    indices.append(self._expect(NumberLiteral).value)
                    self._expect(CloseBracketToken)
                self._expect(EqualToken)
                value = self.parse_expression()
                return ArrayAssignment(self._location_from(start_location), identifier.name, indices, value)
            # Procedure call
            if self._eat(OpenParToken):
                arguments = self.parse_sequence()
                self._expect(CloseParToken)
                return ProcedureCall(self._location_from(start_location), identifier.name, arguments)
            self.index -= 1
        # Native code block
        if self._is_next(NativeCodeBlock):
            native_code_block = self._expect(NativeCodeBlock)
            return NativeCode(native_code_block.location, native_code_block.bf_code)
        # Return instruction
        if self._eat(ReturnKeyword):
            start_location = self._previous_location()
            expression = self.parse_expression()
            return ReturnInstruction(self._location_from(start_location), expression)
        # Question mark
        if self._eat(QuestionMarkToken):
            return ContextSnapshot(self._previous_location())
        # Invalid instruction
        raise CompilationException(self._location(), f'Expected instruction but found {self._peek().__doc__}')

    def parse_instruction_or_statement(self) -> Instruction:
        location = self._location()
        # Loop
        if self._eat(LoopKeyword):
            self._expect(OpenParToken)
            count = self.parse_expression()
            self._expect(CloseParToken)
            instructions = self.parse_instruction_block()
            return LoopStatement(location, count, instructions)
        # While loop
        if self._eat(WhileKeyword):
            self._expect(OpenParToken)
            condition = self.parse_expression()
            self._expect(CloseParToken)
            instructions = self.parse_instruction_block()
            return WhileLoopStatement(location, condition, instructions)
        # For loop
        if self._eat(ForKeyword):
            self._expect(OpenParToken)
            loop_variable_type = self.parse_type()
            loop_variable_identifier = self._expect(IdentifierToken).name
            self._expect(ColonToken)
            loop_array = self.parse_expression()
            self._expect(CloseParToken)
            instructions = self.parse_instruction_block()
            return ForLoopStatement(location, loop_variable_type, loop_variable_identifier, loop_array, instructions)
        # Conditional statement
        if self._eat(IfKeyword):
            self._expect(OpenParToken)
            # Test
            test = self.parse_expression()
            self._expect(CloseParToken)
            # Instructions if true
            if_instructions = self.parse_instruction_block()
            # Optional else
            else_instructions = InstructionBlock.empty()
            if self._eat(ElseKeyword):
                # Instructions if false
                else_instructions = self.parse_instruction_block()
            # Build statement
            return ConditionalStatement(location, test, if_instructions, else_instructions)
        # Instruction
        instruction = self.parse_instruction()
        self._expect(SemicolonToken)
        return instruction

    def parse_instruction_block(self) -> InstructionBlock:
        """Parse an instruction block (or a single instruction or statement)"""
        # Instruction block
        location = self._location()
        if self._eat(OpenBraceToken):
            instructions = InstructionBlock(location)
            while not self._eat(CloseBraceToken):
                instruction = self.parse_instruction_or_statement()
                instructions.append(instruction)
            return instructions
        # Single instruction or statement
        instruction = self.parse_instruction_or_statement()
        return InstructionBlock(instruction.location, instruction)

    def parse_type(self) -> DataType:
        base_type_identifier = self._expect(IdentifierToken)
        base_type = self.namespace.get_base_type(base_type_identifier.location, base_type_identifier.name)
        # Array
        if self._eat(OpenBracketToken):
            size = self._expect(NumericLiteral).value
            self._expect(CloseBracketToken)
            return ArrayType(base_type, size)
        # Single
        return base_type

    def parse_and_define_constant(self) -> None:
        # Parse constant
        start_location = self._expect(HashToken).location
        identifier = self._expect(IdentifierToken).name
        self._expect(EqualToken)
        expression = self.parse_expression()
        self._expect(SemicolonToken)
        # Add to list of available constants
        location = self._location_from(start_location)
        if identifier in self.constants:
            message = f'Constant {identifier!r} is already defined at {self.constants[identifier].location!r}'
            raise CompilationException(location, message)
        self.constants[identifier] = Constant(location, identifier, expression)

    def parse_subroutine_argument_declaration(self) -> list[SubroutineArgument]:
        self._expect(OpenParToken)
        arguments = []
        if self._eat(CloseParToken):
            return arguments
        while True:
            start_location = self._location()
            argument_type = self.parse_type()
            identifier = self._expect(IdentifierToken)
            arguments.append(SubroutineArgument(self._location_from(start_location), identifier.name, argument_type))
            if not self._eat(CommaToken):
                self._expect(CloseParToken)
                break
        return arguments

    def parse_procedure_definition(self) -> Procedure:
        start_location = self._expect(ProcKeyword).location
        identifier = self._expect(IdentifierToken)
        arguments = self.parse_subroutine_argument_declaration()
        instructions = self.parse_instruction_block()
        location = self._location_from(start_location)
        return Procedure(location, identifier.name, arguments, instructions)

    def parse_function_definition(self) -> Function:
        start_location = self._expect(FuncKeyword).location
        identifier = self._expect(IdentifierToken)
        arguments = self.parse_subroutine_argument_declaration()
        self._expect(ArrowToken)
        return_type = self.parse_type()
        instructions = self.parse_instruction_block()
        location = self._location_from(start_location)
        return Function(location, identifier.name, arguments, instructions, return_type)

    def generate_ast(self) -> Namespace:
        while self._has_next():
            if self._is_next(HashToken):
                self.parse_and_define_constant()
            elif self._is_next(ProcKeyword):
                procedure = self.parse_procedure_definition()
                self.namespace.add_subroutine(procedure)
            elif self._is_next(FuncKeyword):
                function = self.parse_function_definition()
                self.namespace.add_subroutine(function)
            else:
                message = f'Expected constant or subroutine declaration but found {self._peek().__doc__}'
                raise CompilationException(self._location(), message)
        return self.namespace


__all__ = ['NamespaceElement', 'SubroutineArgument', 'Procedure', 'Function', 'Namespace', 'Constant', 'ASTGenerator']
