from abc import ABC, abstractmethod
from typing import Self

from type_checking.data_types import *
from exceptions import *
from intermediate_representation.assignment_targets import *
from type_checking.typing_context import CodeBlockTypingContext


class TypedDeclarationTarget(ABC):
    @classmethod
    def from_untyped(cls, context: CodeBlockTypingContext, target: AssignmentTarget,
                     data_type: DataType) -> 'TypedDeclarationTarget':
        if isinstance(target, IdentifierAssignmentTarget):
            return TypedIdentifierDeclarationTarget.from_untyped(context, target, data_type)
        if isinstance(target, ArrayElementAssignmentTarget):
            message = f'Unable to declare an array index (expected identifier or tuple of identifiers)'
            raise CompilationException(target.location, message)
        if isinstance(target, TupleAssignmentTarget):
            return TypedTupleDeclarationTarget.from_untyped(context, target, data_type)
        raise ImpossibleException(f'Unknown target type: {target.__class__.__name__}')

    def __init__(self, location: Location, target_type: DataType) -> None:
        self.location = location
        self.type = target_type

    @abstractmethod
    def __str__(self) -> str:
        ...

    def __repr__(self) -> str:
        return f'{self.__class__.__name__}[{self}]'


class TypedIdentifierDeclarationTarget(TypedDeclarationTarget):
    @classmethod
    def from_untyped(cls, context: CodeBlockTypingContext, target: IdentifierAssignmentTarget,
                     target_type: DataType) -> Self:
        context.register_variable(target.location, target.identifier, target_type)
        return cls(target.location, target.identifier, target_type)

    def __init__(self, location: Location, identifier: str, target_type: DataType) -> None:
        super().__init__(location, target_type)
        self.identifier = identifier

    def __str__(self) -> str:
        return self.identifier


class TypedTupleDeclarationTarget(TypedDeclarationTarget):
    @classmethod
    def from_untyped(cls, context: CodeBlockTypingContext, target: TupleAssignmentTarget,
                     target_type: DataType) -> Self:
        if not isinstance(target_type, ProductType):
            raise CompilationException(target.location, f'Invalid assignment target for {target_type}')
        if target_type.count() != len(target.elements):
            message = f'Expected {len(target.elements)} values to unpack but found {target_type.count()}'
            raise CompilationException(target.location, message)
        elements = []
        for i, element in enumerate(target.elements):
            elements.append(TypedDeclarationTarget.from_untyped(context, element, target_type.operands[i]))
        return cls(target.location, elements, target_type)

    def __init__(self, location: Location, elements: list[TypedDeclarationTarget], target_type: DataType) -> None:
        super().__init__(location, target_type)
        self.elements = elements
        if len(self.elements) < 2:
            raise CompilerException('Tuple assignment target must contain at least two elements')

    def __str__(self) -> str:
        return ', '.join(element.__str__() for element in self.elements)


__all__ = ['TypedDeclarationTarget', 'TypedIdentifierDeclarationTarget', 'TypedTupleDeclarationTarget']
