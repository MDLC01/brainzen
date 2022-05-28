from abc import ABC, abstractmethod
from typing import Generator

from data_types import DataType
from exceptions import *
from intermediate_representation import *
from type_checking.type_checked_instructions import *
from type_checking.typing_context import *


class TypeCheckedNamespaceElement(ABC):
    __slots__ = 'location', 'identifier'

    @classmethod
    def from_element(cls, context: SubroutineTypingContext, element: NamespaceElement) -> 'TypeCheckedNamespaceElement':
        if isinstance(element, NativeSubroutine):
            return TypeCheckedNativeSubroutine.from_native_subroutine(element)
        if isinstance(element, Procedure):
            return TypeCheckedProcedure.from_procedure(context, element)
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


class TypeCheckedNamespace:
    """A type checked namespace is a namespace that has been type checked (the type of every expression is known)."""

    def __init__(self, namespace: Namespace) -> None:
        self.location = namespace.location
        self.identifier = namespace.identifier
        self.subroutine_signatures: dict[str, SubroutineSignature] = {}
        self.elements: list[TypeCheckedNamespaceElement] = [self._type_check_element(element) for element in namespace]

    def _type_check_element(self, element: NamespaceElement) -> TypeCheckedNamespaceElement:
        context = SubroutineTypingContext(self.subroutine_signatures)
        type_checked_element = TypeCheckedNamespaceElement.from_element(context, element)
        identifier = type_checked_element.identifier
        if isinstance(type_checked_element, TypeCheckedSubroutine):
            self.subroutine_signatures[identifier] = SubroutineSignature(type_checked_element.arguments,
                                                                         type_checked_element.return_type)
        else:
            raise ImpossibleException(f'Unknown namespace element type: {type_checked_element.__class__}')
        return type_checked_element

    def __iter__(self) -> Generator[TypeCheckedNamespaceElement, None, None]:
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


__all__ = ['TypeCheckedSubroutine', 'TypeCheckedNativeSubroutine', 'TypeCheckedProcedure', 'TypeCheckedNamespace']
