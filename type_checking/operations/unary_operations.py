from abc import ABC, abstractmethod

from data_types import *
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
                return NegationOperation()
            raise InvalidOperandType(location, operator, operand_type)
        # !!
        if isinstance(operator, DoubleBangToken):
            if operand_type == Types.CHAR:
                return BoolNormalizationOperation()
            raise InvalidOperandType(location, operator, operand_type)
        # -
        if isinstance(operator, MinusToken):
            if operand_type == Types.CHAR:
                return OppositionOperation()
            if isinstance(operand_type, ArrayType):
                return ArrayOppositionOperation(operand_type.count)
            raise InvalidOperandType(location, operator, operand_type)
        raise CompilerException(f'Unknown unary operator: {operator}')

    @abstractmethod
    def operand_type(self) -> DataType:
        ...

    @abstractmethod
    def type(self) -> DataType:
        ...

    @abstractmethod
    def __str__(self) -> str:
        ...


class NegationOperation(UnaryOperation):
    def operand_type(self) -> DataType:
        return Types.CHAR

    def type(self) -> DataType:
        return Types.CHAR

    def __str__(self) -> str:
        return '!'


class BoolNormalizationOperation(UnaryOperation):
    def operand_type(self) -> DataType:
        return Types.CHAR

    def type(self) -> DataType:
        return Types.CHAR

    def __str__(self) -> str:
        return '!!'


class OppositionOperation(UnaryOperation):
    def operand_type(self) -> DataType:
        return Types.CHAR

    def type(self) -> DataType:
        return Types.CHAR

    def __str__(self) -> str:
        return '-'


class ArrayOppositionOperation(UnaryOperation):
    def __init__(self, array_count: int) -> None:
        self.array_count = array_count

    def operand_type(self) -> DataType:
        return ArrayType(Types.CHAR, self.array_count)

    def type(self) -> DataType:
        return ArrayType(Types.CHAR, self.array_count)

    def __str__(self) -> str:
        return '-'


__all__ = ['UnaryOperation', 'NegationOperation', 'BoolNormalizationOperation', 'OppositionOperation',
           'ArrayOppositionOperation']
