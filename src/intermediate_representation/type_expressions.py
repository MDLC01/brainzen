from abc import ABC, abstractmethod
from typing import Optional, TYPE_CHECKING

from exceptions import Location
from reference import Reference


if TYPE_CHECKING:
    from intermediate_representation import Expression


class TypeExpression(ABC):
    __slots__ = 'location'

    def __init__(self, location: Location) -> None:
        self.location = location

    @abstractmethod
    def __str__(self) -> str:
        ...

    @abstractmethod
    def __repr__(self) -> str:
        ...


class TypeReference(TypeExpression):
    __slots__ = 'reference'

    def __init__(self, reference: Reference) -> None:
        super().__init__(reference.location)
        self.reference = reference

    def __str__(self) -> str:
        return f'{self.reference}'

    def __repr__(self) -> str:
        return f'{self.__class__.__name__}[{self.reference}]'


class TypeArray(TypeExpression):
    __slots__ = 'base_type', 'count'

    def __init__(self, location: Location, base_type: TypeExpression, count: Optional['Expression']) -> None:
        super().__init__(location)
        self.base_type = base_type
        self.count = count

    def __str__(self) -> str:
        if self.count is None:
            return f'{self.base_type}[]'
        return f'{self.base_type}[{self.count}]'

    def __repr__(self) -> str:
        if self.count is None:
            return f'{self.__class__.__name__}[{self.base_type!r}]'
        return f'{self.__class__.__name__}[{self.base_type!r}, count={self.count!r}]'


class TypeProduct(TypeExpression):
    __slots__ = 'operands'

    @classmethod
    def from_operands(cls, location: Location, operands: list[TypeExpression]) -> TypeExpression:
        if len(operands) == 1:
            return operands[0]
        return cls(location, operands)

    def __init__(self, location: Location, operands: list[TypeExpression]) -> None:
        super().__init__(location)
        self.operands = operands

    def __str__(self) -> str:
        return ' * '.join(operand.__str__() for operand in self.operands)

    def __repr__(self) -> str:
        operands = ', '.join(operand.__repr__() for operand in self.operands)
        return f'{self.__class__.__name__}[{operands}]'


__all__ = ['TypeExpression', 'TypeReference', 'TypeArray', 'TypeProduct']
