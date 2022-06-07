from abc import ABC, abstractmethod
from typing import Generator, Optional

from data_types import *
from exceptions import *
from intermediate_representation import *
from reference import Reference
from type_checking.operations import *
from type_checking.type_checked_instructions import *
from type_checking.typing_context import SubroutineSignature, SubroutineTypingContext


Value = int | list['Value'] | tuple['Value', ...]


def value_of(expression: TypedExpression) -> Value:
    if isinstance(expression, LiteralChar):
        return expression.value
    if isinstance(expression, LiteralArray):
        return [value_of(element) for element in expression.value]
    if isinstance(expression, LiteralTuple):
        return tuple(value_of(element) for element in expression.elements)
    raise CompilerException('Unable to get value from non-literal types at compile time')


def expression_of(location: Location, value: Value) -> TypedExpression:
    if isinstance(value, bool):
        return LiteralChar(location, int(value))
    if isinstance(value, int):
        return LiteralChar(location, value)
    if isinstance(value, list):
        return LiteralArray(location, [expression_of(location, element) for element in value])
    if isinstance(value, tuple):
        return LiteralTuple(location, [expression_of(location, element) for element in value])
    raise CompilerException(f'Invalid Python equivalent: {value!r}')


def compute_unary_operation(operation: UnaryOperationType, operand: Value) -> Value:
    if operation is UnaryOperationType.NEGATION:
        return int(not operand)
    if operation is UnaryOperationType.BOOL_NORMALIZATION:
        return int(bool(operand))
    if operation is UnaryOperationType.OPPOSITION:
        return -operand
    raise CompilerException(f'Unknown unary operation: {operation!r}')


def compute_binary_operation(operation: BinaryOperationType, left: Value, right: Value) -> Value:
    if operation is BinaryOperationType.EQUALITY_TEST:
        return int(left == right)
    if operation is BinaryOperationType.DIFFERENCE_TEST:
        return int(left != right)
    if operation is BinaryOperationType.STRICT_INEQUALITY_TEST:
        return int(left < right)
    if operation is BinaryOperationType.LARGE_INEQUALITY_TEST:
        return int(left <= right)
    if operation is BinaryOperationType.INVERSE_STRICT_INEQUALITY_TEST:
        return int(left > right)
    if operation is BinaryOperationType.INVERSE_LARGE_INEQUALITY_TEST:
        return int(left >= right)
    if operation is BinaryOperationType.CONJUNCTION:
        return int(left and right)
    if operation is BinaryOperationType.DISJUNCTION:
        return int(left or right)
    if operation is BinaryOperationType.ADDITION:
        return left + right
    if operation is BinaryOperationType.SUBTRACTION:
        return left - right
    if operation is BinaryOperationType.MULTIPLICATION:
        return left * right
    if operation is BinaryOperationType.DIVISION:
        return left // right
    if operation is BinaryOperationType.MODULO_OPERATION:
        return left % right
    if operation is BinaryOperationType.CONCATENATION:
        return left + right
    raise CompilerException(f'Unknown binary operation: {operation!r}')


def evaluate(constants: dict[str, TypedExpression], expression: Expression) -> TypedExpression:
    match expression:
        case ConstantReference(identifier=identifier):
            return constants[identifier]
        case Char() as char:
            return LiteralChar.from_char(char)
        case Array(location=location, value=value):
            return LiteralArray(location, [evaluate(constants, element) for element in value])
        case Tuple(location=location, elements=elements):
            return LiteralTuple(location, [evaluate(constants, element) for element in elements])
        case UnaryArithmeticExpression(location=location, operator=operator, operand=operand):
            typed_operand = evaluate(constants, operand)
            operation = UnaryOperation.from_operator(location, operator, typed_operand.type())
            result = compute_unary_operation(operation.operation_type, value_of(typed_operand))
            return expression_of(location, result)
        case BinaryArithmeticExpression(location=location, operator=operator, left=left, right=right):
            typed_left = evaluate(constants, left)
            typed_right = evaluate(constants, right)
            operation = BinaryOperation.from_operator(location, operator, typed_left.type(), typed_right.type())
            result = compute_binary_operation(operation.operation_type, value_of(typed_left), value_of(typed_right))
            return expression_of(location, result)
    message = f'Invalid expression type for compile time evaluation: {expression.__class__.__name__}'
    raise CompilationException(expression.location, message)


class TypeCheckedNamespaceElement(ABC):
    __slots__ = 'location', 'identifier'

    @classmethod
    def from_element(cls, namespace: 'TypeCheckedNamespace',
                     element: NamespaceElement) -> 'TypeCheckedNamespaceElement':
        if isinstance(element, Constant):
            return TypeCheckedConstant.from_constant(element, namespace.constants)
        if isinstance(element, NativeSubroutine):
            return TypeCheckedNativeSubroutine.from_native_subroutine(element)
        if isinstance(element, Procedure):
            signature = SubroutineSignature(element.arguments, element.return_type)
            context = SubroutineTypingContext(namespace, signature)
            return TypeCheckedProcedure.from_procedure(context, element)
        if isinstance(element, Namespace):
            return TypeCheckedNamespace(element, namespace)
        raise ImpossibleException(f'Unknown namespace element type: {element.__class__}')

    def __init__(self, location: Location, identifier: str) -> None:
        self.location = location
        self.identifier = identifier

    @abstractmethod
    def __str__(self) -> str:
        ...

    @abstractmethod
    def __repr__(self, indent: str = '') -> str:
        ...


class TypeCheckedConstant(TypeCheckedNamespaceElement):
    __slots__ = 'expression'

    @classmethod
    def from_constant(cls, constant: Constant,
                      available_constants: dict[str, TypedExpression]) -> 'TypeCheckedConstant':
        return cls(constant.location, constant.identifier, evaluate(available_constants, constant.expression))

    def __init__(self, location: Location, identifier: str, expression: TypedExpression) -> None:
        super().__init__(location, identifier)
        self.expression = expression

    def type(self) -> DataType:
        return self.expression.type()

    def __str__(self) -> str:
        return f'#{self.identifier} = {self.expression}'

    def __repr__(self, indent: str = '') -> str:
        return f'{self.__class__.__name__}[{self.identifier}, {self.expression!r}]'


class TypeCheckedSubroutine(TypeCheckedNamespaceElement, ABC):
    __slots__ = 'arguments', 'return_type'

    def __init__(self, location: Location, identifier: str,
                 arguments: list[SubroutineArgument], return_type: DataType | None) -> None:
        super().__init__(location, identifier)
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
        suffix = ''
        if self.is_function():
            suffix = f' -> {self.return_type}'
        return f'{self._keyword()} {self.identifier}({self._argument_string()}){suffix} line {self.location.line}'


class TypeCheckedNativeSubroutine(TypeCheckedSubroutine):
    __slots__ = 'offset', 'bf_code'

    @classmethod
    def from_native_subroutine(cls, subroutine: NativeSubroutine) -> 'TypeCheckedNativeSubroutine':
        return cls(subroutine.location, subroutine.identifier, subroutine.arguments, subroutine.return_type,
                   subroutine.offset, subroutine.bf_code)

    def __init__(self, location: Location, identifier: str,
                 arguments: list[SubroutineArgument], return_type: DataType | None, offset: int, bf_code: str) -> None:
        super().__init__(location, identifier, arguments, return_type)
        self.offset = offset
        self.bf_code = bf_code
        CompilationWarning.add(self.location, 'Using native Brainfuck code is not recommended. Every cell should be'
                                              ' reset to 0 (except those containing the return value).')

    def __str__(self) -> str:
        return 'native ' + super().__str__()

    def __repr__(self, indent: str = '') -> str:
        return f'```{self.bf_code}```'


class TypeCheckedProcedure(TypeCheckedSubroutine):
    __slots__ = 'body'

    @classmethod
    def from_procedure(cls, context: SubroutineTypingContext, procedure: Procedure) -> 'TypeCheckedProcedure':
        return cls(context, procedure.location, procedure.identifier, procedure.arguments, procedure.return_type,
                   procedure.body)

    def __init__(self, context: SubroutineTypingContext, location: Location, identifier: str,
                 arguments: list[SubroutineArgument], return_type: DataType | None, body: InstructionBlock) -> None:
        super().__init__(location, identifier, arguments, return_type)
        # Type checking
        context.expected_return_type = self.return_type
        for argument in self.arguments:
            context.add_variable(self.location, argument.identifier, argument.type)
        self.body = TypeCheckedInstructionBlock(context, body)

    def _argument_string(self) -> str:
        return ', '.join(repr(argument) for argument in self.arguments)

    def __repr__(self, indent: str = '') -> str:
        return f'({self._argument_string()}) -> {self.body.__repr__(indent)}'


class TypeCheckedNamespace(TypeCheckedNamespaceElement):
    """A type checked namespace is a namespace that has been type checked (the type of every expression is known)."""

    def __init__(self, namespace: Namespace, parent: Optional['TypeCheckedNamespace'] = None) -> None:
        super().__init__(namespace.location, namespace.identifier)
        self.parent = parent
        self.constants: dict[str, TypedExpression] = {}
        self.namespaces: dict[str, 'TypeCheckedNamespace'] = {}
        self.subroutine_signatures: dict[str, SubroutineSignature] = {}
        self.elements = [self._type_check_element(element) for element in namespace]

    def _type_check_element(self, element: NamespaceElement) -> TypeCheckedNamespaceElement:
        type_checked_element = TypeCheckedNamespaceElement.from_element(self, element)
        identifier = type_checked_element.identifier
        if isinstance(type_checked_element, TypeCheckedConstant):
            self.constants[identifier] = type_checked_element.expression
        elif isinstance(type_checked_element, TypeCheckedSubroutine):
            self.subroutine_signatures[identifier] = SubroutineSignature(type_checked_element.arguments,
                                                                         type_checked_element.return_type)
        elif isinstance(type_checked_element, TypeCheckedNamespace):
            self.namespaces[identifier] = type_checked_element
        else:
            raise ImpossibleException(f'Unknown namespace element type: {type_checked_element.__class__.__name__}')
        return type_checked_element

    def get_namespace(self, reference: Reference | None) -> 'TypeCheckedNamespace':
        if reference is None:
            return self
        parent = self.get_namespace(reference.namespace)
        if reference.identifier in parent.namespaces:
            return parent.namespaces[reference.identifier]
        if self.parent is not None:
            return self.parent.get_namespace(reference)
        raise CompilationException(reference.location, f'Unknown namespace: {reference}')

    def get_constant_value(self, reference: Reference) -> TypedExpression:
        namespace = self.get_namespace(reference.namespace)
        if reference.identifier in namespace.constants:
            return namespace.constants[reference.identifier]
        if self.parent is not None:
            return self.parent.get_constant_value(reference)
        raise CompilationException(reference.location, f'Unknown constant: #{reference}')

    def get_subroutine_signature(self, reference: Reference) -> SubroutineSignature:
        namespace = self.get_namespace(reference.namespace)
        if reference.identifier in namespace.subroutine_signatures:
            return namespace.subroutine_signatures[reference.identifier]
        if self.parent is not None:
            return self.parent.get_subroutine_signature(reference)
        raise CompilationException(reference.location, f'Unknown subroutine: {reference}')

    def __iter__(self) -> Generator[TypeCheckedNamespaceElement, None, None]:
        for element in self.elements:
            if not isinstance(element, TypeCheckedConstant):
                yield element

    def __str__(self) -> str:
        return f'namespace {self.identifier} line {self.location.line}'

    def __repr__(self, indent: str = '') -> str:
        inner_indent = indent + '    '
        s = f'{indent}{self.__class__.__name__}['
        if self.elements:
            s += '\n'
            for element in self.elements:
                s += f'{inner_indent}{element.identifier} = {element.__repr__(inner_indent)},\n'
            s += indent
        return s + ']'


__all__ = ['SubroutineTypingContext', 'TypeCheckedSubroutine', 'TypeCheckedNativeSubroutine', 'TypeCheckedProcedure',
           'TypeCheckedNamespace']
