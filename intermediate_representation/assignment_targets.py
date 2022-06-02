from abc import ABC, abstractmethod

from exceptions import CompilerException, Location


class AssignmentTarget(ABC):
    def __init__(self, location: Location) -> None:
        self.location = location

    @abstractmethod
    def __str__(self) -> str:
        ...

    def __repr__(self) -> str:
        return f'{self.__class__.__name__}[{self}]'


class PrimitiveAssignmentTarget(AssignmentTarget, ABC):
    pass


class IdentifierAssignmentTarget(PrimitiveAssignmentTarget):
    def __init__(self, location: Location, identifier: str) -> None:
        super().__init__(location)
        self.identifier = identifier

    def __str__(self) -> str:
        return self.identifier


class ArrayElementAssignmentTarget(PrimitiveAssignmentTarget):
    def __init__(self, location: Location, array: PrimitiveAssignmentTarget, index: int) -> None:
        super().__init__(location)
        self.array = array
        self.index = index

    def __str__(self) -> str:
        return f'{self.array}[{self.index}]'


class TupleAssignmentTarget(AssignmentTarget):
    @classmethod
    def of(cls, location: Location, elements: list[AssignmentTarget]) -> AssignmentTarget:
        if len(elements) < 1:
            raise CompilerException('Invalid assignment target: length less than 1')
        if len(elements) == 1:
            return elements[0]
        return cls(location, elements)

    def __init__(self, location: Location, elements: list[AssignmentTarget]) -> None:
        super().__init__(location)
        self.elements = elements

    def __str__(self) -> str:
        return ', '.join(element.__str__() for element in self.elements)


__all__ = ['AssignmentTarget', 'PrimitiveAssignmentTarget', 'IdentifierAssignmentTarget',
           'ArrayElementAssignmentTarget', 'TupleAssignmentTarget']
