from abc import ABC, abstractmethod
from typing import Generator, Optional

from data_types import *
from exceptions import *
from intermediate_representation import *
from reference import Reference
from type_checking.type_checked_instructions import *
from type_checking.typing_context import SubroutineSignature, SubroutineTypingContext


class TypeCheckedNamespaceElement(ABC):
    __slots__ = 'location', 'identifier', 'is_private'

    @classmethod
    def from_element(cls, namespace: 'TypeCheckedNamespace',
                     element: NamespaceElement) -> 'TypeCheckedNamespaceElement':
        if isinstance(element, Constant):
            return TypeCheckedConstant.from_constant(element, namespace.constants)
        if isinstance(element, NativeSubroutine):
            return TypeCheckedNativeSubroutine.from_native_subroutine(element)
        if isinstance(element, Procedure):
            signature = SubroutineSignature(element.location, element.is_private, element.arguments,
                                            element.return_type)
            context = SubroutineTypingContext(namespace, signature)
            return TypeCheckedProcedure.from_procedure(context, element)
        if isinstance(element, Namespace):
            return TypeCheckedNamespace(element, namespace)
        raise ImpossibleException(f'Unknown namespace element type: {element.__class__}')

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


class TypeCheckedConstant(TypeCheckedNamespaceElement):
    __slots__ = 'expression'

    @classmethod
    def from_constant(cls, constant: Constant,
                      available_constants: dict[str, TypedExpression]) -> 'TypeCheckedConstant':
        return cls(constant.location, constant.identifier, constant.is_private,
                   evaluate(available_constants, constant.expression))

    def __init__(self, location: Location, identifier: str, is_private: bool, expression: TypedExpression) -> None:
        super().__init__(location, identifier, is_private)
        self.expression = expression

    def type(self) -> DataType:
        return self.expression.type()

    def __str__(self) -> str:
        return f'#{self.identifier} = {self.expression}'

    def __repr__(self, indent: str = '') -> str:
        return f'{self.__class__.__name__}[{self.identifier}, {self.expression!r}]'


class TypeCheckedSubroutine(TypeCheckedNamespaceElement, ABC):
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


class TypeCheckedNativeSubroutine(TypeCheckedSubroutine):
    __slots__ = 'offset', 'bf_code'

    @classmethod
    def from_native_subroutine(cls, subroutine: NativeSubroutine) -> 'TypeCheckedNativeSubroutine':
        return cls(subroutine.location, subroutine.identifier, subroutine.is_private, subroutine.arguments,
                   subroutine.return_type, subroutine.offset, subroutine.bf_code)

    def __init__(self, location: Location, identifier: str, is_private: bool, arguments: list[SubroutineArgument],
                 return_type: DataType | None, offset: int, bf_code: str) -> None:
        super().__init__(location, identifier, is_private, arguments, return_type)
        self.offset = offset
        self.bf_code = bf_code
        CompilationWarning.add(self.location, 'Using native Brainfuck code is not recommended. Every cell should be'
                                              ' reset to 0 (except those containing the return value).')

    def _modifier_prefix(self) -> str:
        return super()._modifier_prefix() + 'native '

    def __repr__(self, indent: str = '') -> str:
        return f'```{self.bf_code}```'


class TypeCheckedProcedure(TypeCheckedSubroutine):
    __slots__ = 'body'

    @classmethod
    def from_procedure(cls, context: SubroutineTypingContext, procedure: Procedure) -> 'TypeCheckedProcedure':
        return cls(context, procedure.location, procedure.identifier, procedure.is_private, procedure.arguments,
                   procedure.return_type, procedure.body)

    def __init__(self, context: SubroutineTypingContext, location: Location, identifier: str, is_private: bool,
                 arguments: list[SubroutineArgument], return_type: DataType | None, body: InstructionBlock) -> None:
        super().__init__(location, identifier, is_private, arguments, return_type)
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
        super().__init__(namespace.location, namespace.identifier, namespace.is_private)
        self.parent = parent
        self.constants: dict[str, TypedExpression] = {}
        self.namespaces: dict[str, 'TypeCheckedNamespace'] = {}
        self.subroutine_signatures: dict[str, SubroutineSignature] = {}
        self.elements: list[TypeCheckedNamespaceElement] = []
        # Type checking
        for element in namespace:
            self.register_element(element)

    def register_element(self, element: NamespaceElement) -> None:
        type_checked_element = TypeCheckedNamespaceElement.from_element(self, element)
        identifier = type_checked_element.identifier
        if isinstance(type_checked_element, TypeCheckedConstant):
            self.constants[identifier] = type_checked_element.expression
        elif isinstance(type_checked_element, TypeCheckedSubroutine):
            self.subroutine_signatures[identifier] = SubroutineSignature(type_checked_element.location,
                                                                         type_checked_element.is_private,
                                                                         type_checked_element.arguments,
                                                                         type_checked_element.return_type)
            self.elements.append(type_checked_element)
        elif isinstance(type_checked_element, TypeCheckedNamespace):
            self.namespaces[identifier] = type_checked_element
            self.elements.append(type_checked_element)
        else:
            raise ImpossibleException(f'Unknown namespace element type: {type_checked_element.__class__.__name__}')

    def get_constant_value(self, reference: Reference) -> TypedExpression:
        namespace = self.get_namespace(reference.namespace)
        if reference.identifier in namespace.constants:
            return namespace.constants[reference.identifier]
        if self.parent is not None:
            return self.parent.get_constant_value(reference)
        raise CompilationException(reference.location, f'Unknown constant: #{reference}')

    def get_namespace(self, reference: Reference | None) -> 'TypeCheckedNamespace':
        if reference is None:
            return self
        parent = self.get_namespace(reference.namespace)
        if reference.identifier in parent.namespaces:
            element = parent.namespaces[reference.identifier]
            if element.is_private and reference.namespace is not None:
                message = f'Namespace {reference.identifier!r} is private (declared at {element.location!r})'
                raise CompilationException(reference.location, message)
            return element
        if self.parent is not None:
            return self.parent.get_namespace(reference)
        raise CompilationException(reference.location, f'Unknown namespace: {reference.identifier!r}')

    def get_subroutine_signature(self, reference: Reference) -> SubroutineSignature:
        namespace = self.get_namespace(reference.namespace)
        if reference.identifier in namespace.subroutine_signatures:
            signature = namespace.subroutine_signatures[reference.identifier]
            if signature.is_private and reference.namespace is not None:
                message = f'Subroutine {reference.identifier!r} is private (declared at {signature.location!r})'
                raise CompilationException(reference.location, message)
            return signature
        if self.parent is not None:
            return self.parent.get_subroutine_signature(reference)
        raise CompilationException(reference.location, f'Unknown subroutine: {reference.identifier!r}')

    def __iter__(self) -> Generator[TypeCheckedNamespaceElement, None, None]:
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


__all__ = ['SubroutineTypingContext', 'TypeCheckedSubroutine', 'TypeCheckedNativeSubroutine', 'TypeCheckedProcedure',
           'TypeCheckedNamespace']
