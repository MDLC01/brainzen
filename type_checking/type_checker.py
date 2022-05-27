from abc import ABC, abstractmethod
from typing import Generator

from data_types import DataType
from exceptions import ImpossibleException, Location
from intermediate_representation import *
from type_checking.type_checked_instructions import *
from type_checking.typing_context import *


class TypeCheckedNamespaceElement(ABC):
    __slots__ = 'location', 'identifier'

    @classmethod
    def from_element(cls, context: SubroutineTypingContext, element: NamespaceElement) -> 'TypeCheckedNamespaceElement':
        if isinstance(element, Function):
            return TypeCheckedFunction.from_function(context, element)
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


class TypeCheckedProcedure(TypeCheckedNamespaceElement):
    __slots__ = 'arguments', 'instructions'

    @classmethod
    def from_procedure(cls, context: SubroutineTypingContext, procedure: Procedure) -> 'TypeCheckedProcedure':
        return cls(context, procedure.location, procedure.identifier, procedure.arguments, procedure.body)

    def __init__(self, context: SubroutineTypingContext, location: Location, identifier: str,
                 arguments: list[SubroutineArgument], body: InstructionBlock) -> None:
        super().__init__(location, identifier)
        self.arguments = arguments
        # Type checking
        for argument in self.arguments:
            context.add_variable(self.location, argument.identifier, argument.type)
        self.instructions = TypeCheckedInstructionBlock(context, body)

    def _argument_string(self) -> str:
        return ', '.join(repr(argument) for argument in self.arguments)

    def __str__(self) -> str:
        return f'proc {self.identifier}({self._argument_string()}) line {self.location.line}'

    def __repr__(self, indent: str = '') -> str:
        arguments = ', '.join(repr(argument) for argument in self.arguments)
        return f'({arguments}) -> {self.instructions.__repr__(indent)}'


class TypeCheckedFunction(TypeCheckedProcedure):
    __slots__ = 'return_type'

    @classmethod
    def from_function(cls, context: SubroutineTypingContext, function: Function) -> 'TypeCheckedFunction':
        context.expected_return_type = function.return_type
        return cls(context, function.location, function.identifier, function.arguments, function.body,
                   function.return_type)

    def __init__(self, context: SubroutineTypingContext, location: Location, identifier: str,
                 arguments: list[SubroutineArgument], body: InstructionBlock, return_type: DataType) -> None:
        super().__init__(context, location, identifier, arguments, body)
        self.return_type = return_type

    def __str__(self) -> str:
        return f'func {self.identifier}({self._argument_string()}) -> {self.return_type} line {self.location.line}'


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
        if isinstance(type_checked_element, TypeCheckedFunction):
            self.subroutine_signatures[identifier] = SubroutineSignature(type_checked_element.arguments,
                                                                         type_checked_element.return_type)
        elif isinstance(type_checked_element, TypeCheckedProcedure):
            self.subroutine_signatures[identifier] = SubroutineSignature(type_checked_element.arguments)
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


__all__ = ['TypeCheckedProcedure', 'TypeCheckedFunction', 'TypeCheckedNamespace']
