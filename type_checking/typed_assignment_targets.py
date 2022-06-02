from abc import ABC, abstractmethod

from data_types import ArrayType, DataType, ProductType
from exceptions import *
from intermediate_representation.assignment_targets import *
from type_checking.typing_context import SubroutineTypingContext


class TypedAssignmentTarget(ABC):
    @classmethod
    def from_assignment_target(cls, context: SubroutineTypingContext,
                               assignment_target: AssignmentTarget) -> 'TypedAssignmentTarget':
        match assignment_target:
            case PrimitiveAssignmentTarget() as primitive_assignment_target:
                return TypedPrimitiveAssignmentTarget.from_primitive_assignment_target(context,
                                                                                       primitive_assignment_target)
            case TupleAssignmentTarget(location=location, elements=elements):
                return TypedTupleAssignmentTarget.of(context, location, elements)
        raise ImpossibleException(f'Unknown assignment target type: {assignment_target.__class__.__name__}')

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
    def from_primitive_assignment_target(cls, context: SubroutineTypingContext,
                                         target: PrimitiveAssignmentTarget) -> 'TypedPrimitiveAssignmentTarget':
        location = target.location
        subscripts: list[tuple[Location, int]] = []
        while isinstance(target, ArrayElementAssignmentTarget):
            subscripts.append((target.location, target.index))
            target = target.array
        subscripts.reverse()
        if not isinstance(target, IdentifierAssignmentTarget):
            raise ImpossibleException(f'Unknown primitive assignment target type: {target.__class__.__name__}')
        return cls(context, location, target.identifier, subscripts)

    def __init__(self, context: SubroutineTypingContext, location: Location, identifier: str,
                 subscripts: list[tuple[Location, int]]) -> None:
        super().__init__(location)
        self.identifier = identifier
        self._indices = [subscript[1] for subscript in subscripts]
        self.offset = 0
        array_type = context.get_variable_type(self.location, self.identifier)
        for subscript_location, index in subscripts:
            if not isinstance(array_type, ArrayType):
                raise CompilationException(subscript_location, f'Can not subscript {array_type}')
            # Negative indices wrap around
            if index < 0:
                index = array_type.count + index
            if not (0 <= index < array_type.count):
                raise CompilationException(subscript_location, 'Array index out of bounds')
            self.offset += index * array_type.base_type.size()
            array_type = array_type.base_type
        self._type = array_type

    def type(self) -> DataType:
        return self._type

    def __str__(self) -> str:
        if len(self._indices) == 0:
            return self.identifier
        indices = ']['.join(str(index) for index in self._indices)
        return f'{self.identifier}[{indices}]'


class TypedTupleAssignmentTarget(TypedAssignmentTarget):
    @classmethod
    def of(cls, context: SubroutineTypingContext, location: Location,
           elements: list[AssignmentTarget]) -> 'TypedAssignmentTarget':
        if len(elements) == 1:
            return TypedAssignmentTarget.from_assignment_target(context, elements[0])
        return cls(context, location, elements)

    def __init__(self, context: SubroutineTypingContext, location: Location, elements: list[AssignmentTarget]) -> None:
        super().__init__(location)
        self.elements = [TypedAssignmentTarget.from_assignment_target(context, element) for element in elements]
        if len(self.elements) < 2:
            raise CompilerException('Tuple assignment target must contain at least two elements')

    def type(self) -> ProductType:
        return ProductType([element.type() for element in self.elements])

    def __str__(self) -> str:
        return ', '.join(element.__str__() for element in self.elements)


__all__ = ['TypedAssignmentTarget', 'TypedPrimitiveAssignmentTarget', 'TypedTupleAssignmentTarget']
