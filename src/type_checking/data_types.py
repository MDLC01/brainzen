from abc import ABC, abstractmethod

from exceptions import *


class DataType(ABC):
    """A type for variables, functions and subroutine arguments."""

    __slots__ = ()

    def is_string(self) -> bool:
        if isinstance(self, ArrayType):
            return self.base_type.is_string()
        return self == Types.CHAR

    def is_integer(self) -> bool:
        return self == Types.CHAR

    def is_numeric(self) -> bool:
        return self == Types.CHAR

    def is_array(self) -> bool:
        return isinstance(self, ArrayType)

    def array_depth(self) -> int:
        if isinstance(self, ArrayType):
            return 1 + self.base_type.array_depth()
        return 0

    @abstractmethod
    def size(self) -> int:
        """The size of a variable of this type, in cells."""

    @abstractmethod
    def __eq__(self, other) -> bool:
        """Test if the passed type is the same as this one."""

    @abstractmethod
    def __repr__(self) -> str:
        """Return the string representation of this type."""

    def __str__(self) -> str:
        return self.__repr__()


class PrimitiveType(DataType):
    __slots__ = 'identifier', '_size'

    def __init__(self, identifier: str, size: int) -> None:
        self.identifier = identifier
        self._size = size

    def size(self) -> int:
        return self._size

    def __eq__(self, other) -> bool:
        if not isinstance(other, type(self)):
            return False
        return other.identifier == self.identifier and other._size == self._size

    def __repr__(self) -> str:
        return self.identifier


class ProductType(DataType):
    __slots__ = 'operands'

    @classmethod
    def from_operands(cls, operands: list[DataType]) -> 'DataType':
        if len(operands) == 1:
            return operands[0]
        return cls(operands)

    def __init__(self, operands: list[DataType]) -> None:
        self.operands = operands
        if len(self.operands) < 2:
            raise CompilerException('Product type must contain at least two operand')

    def count(self) -> int:
        return len(self.operands)

    def offset_of(self, index: int) -> int:
        return sum(operand.size() for i, operand in enumerate(self.operands) if i < index)

    def size(self) -> int:
        return sum(t.size() for t in self.operands)

    def __eq__(self, other) -> bool:
        if not isinstance(other, type(self)):
            return False
        return other.operands == self.operands

    def __repr__(self) -> str:
        return ' * '.join(t.__repr__() for t in self.operands)


class ArrayType(DataType):
    __slots__ = 'base_type', 'count'

    def __init__(self, base_type: DataType, size: int) -> None:
        self.base_type = base_type
        self.count = size
        if size < 1:
            raise CompilationException(Location.unknown(), 'An array must contain at least 1 element')

    def size(self) -> int:
        return self.base_type.size() * self.count

    def __eq__(self, other) -> bool:
        if not isinstance(other, type(self)):
            return False
        return other.base_type == self.base_type and other.count == self.count

    def __repr__(self) -> str:
        return f'{self.base_type}[{self.count}]'


class Types:
    CHAR = PrimitiveType('char', 1)


__all__ = ['DataType', 'PrimitiveType', 'ProductType', 'ArrayType', 'Types']
