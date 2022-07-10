from abc import ABC, abstractmethod

from type_checking.data_types import *
from exceptions import *
from tokenization.tokens import *


class InvalidOperandType(CompilationException):
    def __init__(self, location: Location, operator: Token, operand_type: DataType) -> None:
        super().__init__(location, f'Invalid operand type for {operator}: {operand_type}')


class UnaryOperation(ABC):
    @staticmethod
    def _invalid_operand_type_exception(location: Location, operator: Token,
                                        operand_type: DataType) -> CompilationException:
        return CompilationException(location, f'Invalid operand type for {operator}: {operand_type}')

    @classmethod
    def from_operator(cls, location: Location, operator: Token, operand_type: DataType) -> 'UnaryOperation':
        # !
        if isinstance(operator, BangToken):
            if operand_type == Types.CHAR:
                return NegationOperation(operator)
            raise InvalidOperandType(location, operator, operand_type)
        # !!
        if isinstance(operator, DoubleBangToken):
            if operand_type == Types.CHAR:
                return BoolNormalizationOperation(operator)
            raise InvalidOperandType(location, operator, operand_type)
        # -
        if isinstance(operator, MinusToken):
            if operand_type == Types.CHAR:
                return OppositionOperation(operator)
            if isinstance(operand_type, ArrayType):
                return ArrayOppositionOperation(operator, operand_type.count)
            raise InvalidOperandType(location, operator, operand_type)
        # **
        if isinstance(operator, DoubleStarToken):
            if isinstance(operand_type, ArrayType):
                inner_type = operand_type.base_type
                if isinstance(inner_type, ArrayType):
                    return ArrayFlatteningOperation(operator, inner_type.base_type, operand_type.count,
                                                    inner_type.count)
            raise InvalidOperandType(location, operator, operand_type)
        raise CompilerException(f'Unknown unary operator: {operator}')

    def __init__(self, operator: Token) -> None:
        self.operator = operator

    @abstractmethod
    def operand_type(self) -> DataType:
        ...

    @abstractmethod
    def type(self) -> DataType:
        ...

    def __str__(self) -> str:
        return self.operator.__str__()


class NegationOperation(UnaryOperation):
    def operand_type(self) -> DataType:
        return Types.CHAR

    def type(self) -> DataType:
        return Types.CHAR


class BoolNormalizationOperation(UnaryOperation):
    def operand_type(self) -> DataType:
        return Types.CHAR

    def type(self) -> DataType:
        return Types.CHAR


class OppositionOperation(UnaryOperation):
    def operand_type(self) -> DataType:
        return Types.CHAR

    def type(self) -> DataType:
        return Types.CHAR


class ArrayOppositionOperation(UnaryOperation):
    def __init__(self, operator: Token, array_count: int) -> None:
        super().__init__(operator)
        self.array_count = array_count

    def operand_type(self) -> DataType:
        return ArrayType(Types.CHAR, self.array_count)

    def type(self) -> DataType:
        return ArrayType(Types.CHAR, self.array_count)


class ArrayFlatteningOperation(UnaryOperation):
    def __init__(self, operator: Token, base_type: DataType, outer_count: int, inner_count: int) -> None:
        super().__init__(operator)
        self.base_type = base_type
        self.outer_count = outer_count
        self.inner_count = inner_count

    def operand_type(self) -> DataType:
        inner_type = ArrayType(self.base_type, self.inner_count)
        return ArrayType(inner_type, self.outer_count)

    def type(self) -> DataType:
        return ArrayType(self.base_type, self.outer_count * self.inner_count)


__all__ = ['UnaryOperation', 'NegationOperation', 'BoolNormalizationOperation', 'OppositionOperation',
           'ArrayOppositionOperation', 'ArrayFlatteningOperation']
