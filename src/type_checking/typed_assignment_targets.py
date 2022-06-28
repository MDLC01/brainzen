from abc import ABC, abstractmethod

from data_types import ArrayType, DataType, ProductType
from exceptions import *
from intermediate_representation.assignment_targets import *
from type_checking.typing_context import CodeBlockTypingContext


class TypedAssignmentTarget(ABC):
    @classmethod
    def from_untyped(cls, context: CodeBlockTypingContext, target: AssignmentTarget) -> 'TypedAssignmentTarget':
        if isinstance(target, PrimitiveAssignmentTarget):
            return TypedPrimitiveAssignmentTarget.from_untyped(context, target)
        if isinstance(target, TupleAssignmentTarget):
            return TypedTupleAssignmentTarget.from_untyped(context, target)
        raise ImpossibleException(f'Unknown assignment target type: {target.__class__.__name__}')

    def __init__(self, location: Location) -> None:
        self.location = location

    @abstractmethod
    def type(self) -> DataType:
        ...

    @abstractmethod
    def __str__(self) -> str:
        ...

    def __repr__(self) -> str:
        return f'{self.__class__.__name__}[{self}]'


class TypedPrimitiveAssignmentTarget(TypedAssignmentTarget):
    @classmethod
    def from_untyped(cls, context: CodeBlockTypingContext,
                     target: PrimitiveAssignmentTarget) -> 'TypedPrimitiveAssignmentTarget':
        # Get list of subscripts
        subscripts: list[tuple[Location, int]] = []
        while isinstance(target, ArrayElementAssignmentTarget):
            subscripts.append((target.location, target.index))
            target = target.array
        if not isinstance(target, IdentifierAssignmentTarget):
            raise ImpossibleException(f'Unknown primitive assignment target type: {target.__class__.__name__}')
        # Compute offset
        offset = 0
        indices = []
        current_type = context.get_variable_type(target.location, target.identifier)
        for subscript_location, index in reversed(subscripts):
            if not isinstance(current_type, ArrayType):
                raise CompilationException(subscript_location, f'Cannot subscript {current_type}')
            # Negative indices wrap around
            if index < 0:
                index = current_type.count + index
            if not (0 <= index < current_type.count):
                raise CompilationException(subscript_location, 'Array index out of bounds')
            offset += index * current_type.base_type.size()
            indices.append(index)
            current_type = current_type.base_type
        return cls(target.location, target.identifier, current_type, indices, offset)

    def __init__(self, location: Location, identifier: str, data_type: DataType, indices: list[int],
                 offset: int) -> None:
        super().__init__(location)
        self.identifier = identifier
        self._type = data_type
        self.indices = indices
        self.offset = offset

    def type(self) -> DataType:
        return self._type

    def __str__(self) -> str:
        if len(self.indices) == 0:
            return self.identifier
        indices = ']['.join(str(index) for index in self.indices)
        return f'{self.identifier}[{indices}]'


class TypedTupleAssignmentTarget(TypedAssignmentTarget):
    @classmethod
    def from_untyped(cls, context: CodeBlockTypingContext, target: TupleAssignmentTarget) -> 'TypedAssignmentTarget':
        elements = [TypedAssignmentTarget.from_untyped(context, element) for element in target.elements]
        return cls(target.location, elements)

    def __init__(self, location: Location, elements: list[TypedAssignmentTarget]) -> None:
        super().__init__(location)
        self.elements = elements
        if len(self.elements) < 2:
            raise CompilerException('Tuple assignment target must contain at least two elements')

    def type(self) -> ProductType:
        return ProductType([element.type() for element in self.elements])

    def __str__(self) -> str:
        return ', '.join(element.__str__() for element in self.elements)


__all__ = ['TypedAssignmentTarget', 'TypedPrimitiveAssignmentTarget', 'TypedTupleAssignmentTarget']
