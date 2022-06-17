from abc import ABC, abstractmethod
from typing import Generator, Type, TypeVar

from data_types import *
from exceptions import *
from intermediate_representation.assignment_targets import *
from intermediate_representation.instructions import *
from reference import *
from tokenization.operators import *
from tokenization.tokens import *


class NamespaceElement(ABC):
    __slots__ = 'location', 'identifier', 'is_private'

    def __init__(self, location: Location, identifier: str, is_private: bool) -> None:
        self.location = location
        self.identifier = identifier
        self.is_private = is_private

    def _modifier_prefix(self) -> str:
        s = ''
        if self.is_private:
            s += 'private '
        return s

    @abstractmethod
    def __str__(self) -> str:
        ...

    @abstractmethod
    def __repr__(self, indent: str = '') -> str:
        ...


class Constant(NamespaceElement):
    __slots__ = 'expression'

    def __init__(self, location: Location, identifier: str, is_private: bool, expression: Expression) -> None:
        super().__init__(location, identifier, is_private)
        self.expression = expression

    def __str__(self) -> str:
        return f'#{self.identifier} = {self.expression}'

    def __repr__(self, indent: str = '') -> str:
        return f'{self.__class__.__name__}[{self.identifier}, {self.expression!r}]'


class SubroutineArgument:
    __slots__ = 'location', 'identifier', 'type'

    def __init__(self, location: Location, identifier: str, variable_type: DataType) -> None:
        self.location = location
        self.identifier = identifier
        self.type = variable_type

    def __repr__(self) -> str:
        return f'{self.type} {self.identifier}'


class Subroutine(NamespaceElement, ABC):
    __slots__ = 'arguments', 'return_type'

    def __init__(self, location: Location, identifier: str, is_private: bool, arguments: list[SubroutineArgument],
                 return_type: DataType | None) -> None:
        super().__init__(location, identifier, is_private)
        self.arguments = arguments
        self.return_type = return_type

    def is_function(self) -> bool:
        return self.return_type is not None

    def _keyword(self) -> str:
        if self.is_function():
            return 'func'
        return 'proc'

    def _argument_string(self) -> str:
        return ', '.join(repr(argument) for argument in self.arguments)

    def __str__(self) -> str:
        prefix = f'{self._modifier_prefix()}{self._keyword()}'
        suffix = ''
        if self.is_function():
            suffix = f' -> {self.return_type}'
        return f'{prefix} {self.identifier}({self._argument_string()}){suffix} line {self.location.line}'


class NativeSubroutine(Subroutine):
    __slots__ = 'offset', 'bf_code'

    def __init__(self, location: Location, identifier: str, is_private: bool, arguments: list[SubroutineArgument],
                 return_type: DataType | None, offset: int, bf_code: str) -> None:
        super().__init__(location, identifier, is_private, arguments, return_type)
        self.offset = offset
        self.bf_code = bf_code

    def _modifier_prefix(self) -> str:
        return super()._modifier_prefix() + 'native '

    def __repr__(self, indent: str = '') -> str:
        return f'```{self.bf_code}```'


class Procedure(Subroutine):
    __slots__ = 'body'

    def __init__(self, location: Location, identifier: str, is_private: bool, arguments: list[SubroutineArgument],
                 return_type: DataType | None, body: InstructionBlock) -> None:
        super().__init__(location, identifier, is_private, arguments, return_type)
        self.body = body

    def _argument_string(self) -> str:
        return ', '.join(repr(argument) for argument in self.arguments)

    def __repr__(self, indent: str = '') -> str:
        return f'({self._argument_string()}) -> {self.body.__repr__(indent)}'


class AlreadyDefinedException(CompilationException):
    def __init__(self, element_type: str, element: NamespaceElement, redefine_location: Location) -> None:
        message = f'{element_type} {element.identifier!r} is already defined at {element.location!r}'
        super().__init__(redefine_location, message)


class Namespace(NamespaceElement):
    def __init__(self, location: Location, identifier: str, is_private: bool) -> None:
        super().__init__(location, identifier, is_private)
        self.elements: list[NamespaceElement] = []
        self.constants: dict[str, Constant] = {}
        self.namespaces: dict[str, Namespace] = {}
        self.subroutines: dict[str, Subroutine] = {}

    def register_constant(self, constant: Constant) -> None:
        identifier = constant.identifier
        if identifier in self.constants:
            raise AlreadyDefinedException('Constant', self.constants[identifier], constant.location)
        self.constants[identifier] = constant
        self.elements.append(constant)

    def register_namespace(self, namespace: 'Namespace') -> None:
        identifier = namespace.identifier
        if identifier in self.namespaces:
            raise AlreadyDefinedException('Namespace', self.namespaces[identifier], namespace.location)
        self.namespaces[identifier] = namespace
        self.elements.append(namespace)

    def register_subroutine(self, subroutine: Subroutine) -> None:
        identifier = subroutine.identifier
        if identifier in self.subroutines:
            raise AlreadyDefinedException('Subroutine', self.subroutines[identifier], subroutine.location)
        self.subroutines[identifier] = subroutine
        self.elements.append(subroutine)

    def __iter__(self) -> Generator[NamespaceElement, None, None]:
        for element in self.elements:
            yield element

    def __str__(self) -> str:
        return f'{self._modifier_prefix()}namespace {self.identifier} line {self.location.line}'

    def __repr__(self, indent: str = '') -> str:
        inner_indent = indent + '    '
        s = f'{indent}{self.__class__.__name__}['
        if self.elements:
            s += '\n'
            for element in self.elements:
                s += f'{inner_indent}{element.identifier} = {element.__repr__(inner_indent)},\n'
            s += indent
        return s + ']'


class File(Namespace):
    def __init__(self, location: Location) -> None:
        super().__init__(location, '<global>', False)


class ASTGenerator:
    _T = TypeVar('_T')

    def __init__(self, file_location: Location, tokens: list[Token]) -> None:
        self.file = File(file_location)
        self.tokens = tokens
        self.index = 0

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

    def _peek(self, offset: int = 0) -> AnyToken:
        """Return the next token without advancing the cursor"""
        return self.tokens[self.index + offset]

    def _is_next(self, token_type: Type[AnyToken], offset: int = 0) -> bool:
        """Test if the next token if of the passed token type"""
        return isinstance(self._peek(offset), token_type)

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

    def parse_sequence(self, closing_token_type: Type[AnyToken]) -> list[Expression]:
        """
        Parse a sequence of expressions (actually, binary operations), separated by commas, until a token of type
        `closing_token_type` is found. Closing token is skipped.
        """
        elements = []
        while not self._eat(closing_token_type):
            elements.append(self.parse_binary_operation())
            if not self._eat(CommaToken):
                self._expect(closing_token_type)
                break
        return elements

    def parse_for_loop_iterator(self) -> ForLoopIterator:
        start_location = self._location()
        loop_variable = self._expect(IdentifierToken).name
        self._expect(ColonToken)
        loop_array = self.parse_binary_operation()
        iterator_location = self._location_from(start_location)
        return ForLoopIterator(iterator_location, loop_variable, loop_array)

    def parse_for_loop_iterators(self, closing_token_type: Type[AnyToken]) -> list[ForLoopIterator]:
        iterators = []
        while not self._eat(closing_token_type):
            iterators.append(self.parse_for_loop_iterator())
            if not self._eat(CommaToken):
                self._expect(closing_token_type)
                break
        return iterators

    def parse_array_literal_or_comprehension(self) -> Expression:
        start_location = self._location()
        self._expect(OpenBracketToken)
        # Empty array
        if self._eat(CloseBracketToken):
            return Array(self._location_from(start_location), [])
        # Non-empty array or array comprehension
        element = self.parse_binary_operation()
        # Array comprehension
        if self._eat(PipeToken):
            iterators = self.parse_for_loop_iterators(CloseBracketToken)
            return ArrayComprehension(self._location_from(start_location), element, iterators)
        # More than 1 element
        if self._eat(CommaToken):
            sequence = self.parse_sequence(CloseBracketToken)
            return Array(self._location_from(start_location), [element, *sequence])
        # Array with a single element
        self._expect(CloseBracketToken)
        return Array(self._location_from(start_location), [element])

    def parse_value(self) -> Expression:
        start_location = self._location()
        # Parenthesised expression
        if self._eat(OpenParToken):
            value = self.parse_expression()
            self._expect(CloseParToken)
            return value
        # Constant
        if self._eat(HashToken):
            identifier = self._expect(IdentifierToken).name
            return ConstantReference(self._location_from(start_location), identifier)
        # Function call
        if self._is_next(IdentifierToken) and (self._is_next(OpenParToken, 1) or self._is_next(DoubleColonToken, 1)):
            reference = self.parse_reference()
            self._expect(OpenParToken)
            arguments = self.parse_sequence(CloseParToken)
            return FunctionCall(self._location_from(start_location), reference, arguments)
        # Identifier
        if self._is_next(IdentifierToken):
            identifier = self._expect(IdentifierToken).name
            return Identifier(self._location_from(start_location), identifier)
        # Number literal
        if self._is_next(NumberLiteral):
            value = self._expect(NumberLiteral).value
            return Char(self._location_from(start_location), value)
        # String literal
        if self._is_next(StringLiteral):
            string_literal = self._expect(StringLiteral)
            return Array.from_string(string_literal.location, string_literal.value)
        # Array literal or comprehension
        if self._is_next(OpenBracketToken):
            return self.parse_array_literal_or_comprehension()
        raise CompilationException(self._location(), f'Expected value but found {self._peek().__doc__}')

    def parse_operand(self) -> Expression:
        start_location = self._location()
        operand = self.parse_value()
        # Subscripts / slices
        while self._is_next(OpenBracketToken):
            self._expect(OpenBracketToken)
            index = self.parse_expression()
            if self._eat(ColonToken):
                end = self.parse_expression()
                self._expect(CloseBracketToken)
                operand = ArraySlicingExpression(self._location_from(start_location), operand, index, end)
            else:
                self._expect(CloseBracketToken)
                operand = ArraySubscriptExpression(self._location_from(start_location), operand, index)
        return operand

    def parse_unary_operation(self) -> Expression:
        if self._peek().is_unary_operator():
            token = self._next()
            return UnaryArithmeticExpression(token.location, token.unary_operator, self.parse_unary_operation())
        return self.parse_operand()

    def parse_binary_operation(self) -> Expression:
        left = self.parse_unary_operation()
        priority = Priority.PARENTHESIS
        while self._peek().is_binary_operator():
            location = self._location()
            operator = self._next().binary_operator
            right = self.parse_unary_operation()
            if isinstance(left, BinaryArithmeticExpression) and priority < operator.priority:
                left.right = BinaryArithmeticExpression(location, operator, left.right, right)
            else:
                left = BinaryArithmeticExpression(location, operator, left, right)
            priority = left.operator.priority
        return left

    def parse_expression(self) -> Expression:
        start_location = self._location()
        elements = [self.parse_binary_operation()]
        while self._eat(CommaToken):
            elements.append(self.parse_binary_operation())
        return Tuple.from_elements(self._location_from(start_location), elements)

    def parse_primitive_assignment_target(self) -> PrimitiveAssignmentTarget:
        start_location = self._location()
        identifier = self._expect(IdentifierToken).name
        target = IdentifierAssignmentTarget(self._location_from(start_location), identifier)
        while self._eat(OpenBracketToken):
            index = self._expect(NumericLiteral).value
            target = ArrayElementAssignmentTarget(self._location_from(start_location), target, index)
        return target

    def parse_assignment_target(self) -> AssignmentTarget:
        start_location = self._location()
        targets: list[AssignmentTarget] = []
        while True:
            if self._eat(OpenParToken):
                targets.append(self.parse_assignment_target())
                self._expect(CloseParToken)
            else:
                targets.append(self.parse_primitive_assignment_target())
            if not self._eat(CommaToken):
                break
        return TupleAssignmentTarget(self._location_from(start_location), targets)

    def parse_reference(self) -> Reference:
        """Parse a reference to a namespace element."""
        start_location = self._location()
        identifier = self._expect(IdentifierToken).name
        reference = Reference(self._location_from(start_location), identifier)
        while self._eat(DoubleColonToken):
            identifier = self._expect(IdentifierToken).name
            reference = Reference(self._location_from(start_location), identifier, reference)
        return reference

    def parse_instruction(self) -> Instruction:
        """Parse an instruction (does not expect a semicolon at the end)."""
        start_location = self._location()
        # Incrementation
        if self._is_next(IdentifierToken) and self._is_next(DoublePlusToken, 1):
            identifier = self._expect(IdentifierToken).name
            self._expect(DoublePlusToken)
            return Incrementation(self._location_from(start_location), identifier)
        # Decrementation
        if self._is_next(IdentifierToken) and self._is_next(DoubleMinusToken, 1):
            identifier = self._expect(IdentifierToken).name
            self._expect(DoubleMinusToken)
            return Decrementation(self._location_from(start_location), identifier)
        # Procedure call
        if self._is_next(IdentifierToken) and (self._is_next(OpenParToken, 1) or self._is_next(DoubleColonToken, 1)):
            reference = self.parse_reference()
            self._expect(OpenParToken)
            arguments = self.parse_sequence(CloseParToken)
            return ProcedureCall(self._location_from(start_location), reference, arguments)
        # Variable declaration
        if self._eat(LetKeyword):
            variable_type = self.parse_type()
            variable_name = self._expect(IdentifierToken).name
            if self._eat(EqualToken):
                value = self.parse_expression()
                return VariableDeclaration(self._location_from(start_location), variable_name, variable_type, value)
            return VariableDeclaration(self._location_from(start_location), variable_name, variable_type)
        # Return instruction
        if self._eat(ReturnKeyword):
            expression = self.parse_expression()
            return ReturnInstruction(self._location_from(start_location), expression)
        # Context snapshot
        if self._is_next(IdentifierToken) and self._is_next(QuestionMarkToken, 1):
            identifier = self._expect(IdentifierToken).name
            self._expect(QuestionMarkToken)
            return ContextSnapshot(self._location_from(start_location), identifier)
        if self._eat(QuestionMarkToken):
            return ContextSnapshot(self._location_from(start_location))
        # Assignment
        assignment_target = self.parse_assignment_target()
        self._expect(EqualToken)
        value = self.parse_expression()
        return Assignment(self._location_from(start_location), assignment_target, value)

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
        # Do while loop
        if self._eat(DoKeyword):
            instructions = self.parse_instruction_block()
            self._expect(WhileKeyword)
            self._expect(OpenParToken)
            condition = self.parse_expression()
            self._expect(CloseParToken)
            return DoWhileLoopStatement(location, instructions, condition)
        # For loop
        if self._eat(ForKeyword):
            self._expect(OpenParToken)
            iterators = self.parse_for_loop_iterators(CloseParToken)
            instructions = self.parse_instruction_block()
            return ForLoopStatement(location, iterators, instructions)
        # Conditional statement
        if self._eat(IfKeyword):
            self._expect(OpenParToken)
            # Test
            test = self.parse_expression()
            self._expect(CloseParToken)
            # Instructions if true
            if_instructions = self.parse_instruction_block()
            # Optional else
            else_instructions = None
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

    def parse_base_type(self) -> DataType:
        # Parenthesised type description
        if self._eat(OpenParToken):
            operand = self.parse_type()
            self._expect(CloseParToken)
            return operand
        # Identifier
        start_location = self._location()
        identifier = self._expect(IdentifierToken).name
        return PrimitiveType.of(self._location_from(start_location), identifier)

    def parse_type_operand(self) -> DataType:
        operand = self.parse_base_type()
        # Subscripts
        while self._is_next(OpenBracketToken):
            self._expect(OpenBracketToken)
            size = self._expect(NumericLiteral).value
            self._expect(CloseBracketToken)
            operand = ArrayType(operand, size)
        return operand

    def parse_type(self) -> DataType:
        types = [self.parse_type_operand()]
        while self._eat(StarToken):
            types.append(self.parse_type_operand())
        return ProductType.from_operands(types)

    def parse_constant_definition(self, is_private: bool) -> Constant:
        start_location = self._expect(HashToken).location
        identifier = self._expect(IdentifierToken).name
        self._expect(EqualToken)
        expression = self.parse_expression()
        self._expect(SemicolonToken)
        location = self._location_from(start_location)
        return Constant(location, identifier, is_private, expression)

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

    def parse_subroutine_definition(self, is_private: bool) -> Subroutine:
        start_location = self._location()
        # Subroutine type
        is_native = self._eat(NativeKeyword)
        is_function = self._eat(FuncKeyword)
        if not is_function:
            self._expect(ProcKeyword)
        # Subroutine identifier
        identifier = self._expect(IdentifierToken).name
        # Subroutine signature
        arguments = self.parse_subroutine_argument_declaration()
        return_type = None
        if is_function:
            self._expect(ArrowToken)
            return_type = self.parse_type()
        # Native code
        if is_native:
            self._expect(TildeToken)
            offset = self._expect(NumericLiteral).value
            location = self._location_from(start_location)
            bf_code = self._expect(NativeCodeBlock).bf_code
            return NativeSubroutine(location, identifier, is_private, arguments, return_type, offset, bf_code)
        # Subroutine body
        location = self._location_from(start_location)
        body = self.parse_instruction_block()
        return Procedure(location, identifier, is_private, arguments, return_type, body)

    def parse_namespace_definition(self, is_private: bool) -> Namespace:
        start_location = self._location()
        self._expect(NamespaceKeyword)
        identifier = self._expect(IdentifierToken).name
        location = self._location_from(start_location)
        self._expect(OpenBraceToken)
        namespace = Namespace(location, identifier, is_private)
        while not self._eat(CloseBraceToken):
            self.parse_and_register_namespace_element(namespace)
        return namespace

    def parse_and_register_namespace_element(self, namespace: Namespace) -> None:
        is_private = self._eat(PrivateKeyword)
        # Constant definition
        if self._is_next(HashToken):
            constant = self.parse_constant_definition(is_private)
            namespace.register_constant(constant)
        # Namespace definition
        elif self._is_next(NamespaceKeyword):
            child_namespace = self.parse_namespace_definition(is_private)
            namespace.register_namespace(child_namespace, )
        # Subroutine definition
        else:
            subroutine = self.parse_subroutine_definition(is_private)
            namespace.register_subroutine(subroutine)

    def generate(self) -> File:
        while self._has_next():
            self.parse_and_register_namespace_element(self.file)
        return self.file


__all__ = ['NamespaceElement', 'Constant', 'SubroutineArgument', 'Subroutine', 'NativeSubroutine', 'Procedure',
           'Namespace', 'File', 'ASTGenerator']
