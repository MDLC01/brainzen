from abc import ABC, abstractmethod

from data_types import *
from exceptions import *
from tokenization.tokens import *


class InvalidLeftOperandType(CompilationException):
    def __init__(self, location: Location, operator: Token, operand_type: DataType) -> None:
        super().__init__(location, f'Invalid left operand type for {operator}: {operand_type}')


class InvalidRightOperandType(CompilationException):
    def __init__(self, location: Location, operator: Token, operand_type: DataType) -> None:
        super().__init__(location, f'Invalid right operand type for {operator}: {operand_type}')


class NotEqualOperandTypes(CompilationException):
    def __init__(self, location: Location, operator: Token, left_type: DataType, right_type: DataType) -> None:
        message = f'Invalid operand types for {operator}: both operand must be of the same type,' \
                  f' but found {left_type} and {right_type}'
        super().__init__(location, message)


class BinaryOperation(ABC):
    @classmethod
    def from_operator(cls, location: Location, operator: Token, left_type: DataType,
                      right_type: DataType) -> 'BinaryOperation':
        # ==
        if isinstance(operator, DoubleEqualToken):
            if left_type == right_type:
                return EqualityTestOperation(left_type)
            raise NotEqualOperandTypes(location, operator, left_type, right_type)
        # !=
        if isinstance(operator, BangEqualToken):
            if left_type == right_type:
                return DifferenceTestOperation(left_type)
            raise NotEqualOperandTypes(location, operator, left_type, right_type)
        # <
        if isinstance(operator, LessThanToken):
            if left_type == Types.CHAR:
                if right_type == Types.CHAR:
                    return StrictInequalityTestOperation()
                raise InvalidRightOperandType(location, operator, right_type)
            raise InvalidLeftOperandType(location, operator, left_type)
        # <=
        if isinstance(operator, LessThanEqualToken):
            if left_type == Types.CHAR:
                if right_type == Types.CHAR:
                    return LargeInequalityTestOperation()
                raise InvalidRightOperandType(location, operator, right_type)
            raise InvalidLeftOperandType(location, operator, left_type)
        # >
        if isinstance(operator, GreaterThanToken):
            if left_type == Types.CHAR:
                if right_type == Types.CHAR:
                    return InverseStrictInequalityTestOperation()
                raise InvalidRightOperandType(location, operator, right_type)
            raise InvalidLeftOperandType(location, operator, left_type)
        # >=
        if isinstance(operator, GreaterThanEqualToken):
            if left_type == Types.CHAR:
                if right_type == Types.CHAR:
                    return InverseLargeInequalityTestOperation()
                raise InvalidRightOperandType(location, operator, right_type)
            raise InvalidLeftOperandType(location, operator, left_type)
        # &&
        if isinstance(operator, DoubleAmpersandToken):
            if left_type == Types.CHAR:
                if right_type == Types.CHAR:
                    return ConjunctionOperation()
                raise InvalidRightOperandType(location, operator, right_type)
            raise InvalidLeftOperandType(location, operator, left_type)
        # ||
        if isinstance(operator, DoublePipeToken):
            if left_type == Types.CHAR:
                if right_type == Types.CHAR:
                    return DisjunctionOperation()
                raise InvalidRightOperandType(location, operator, right_type)
            raise InvalidLeftOperandType(location, operator, left_type)
        # +
        if isinstance(operator, PlusToken):
            if left_type == Types.CHAR:
                if right_type == Types.CHAR:
                    return AdditionOperation()
                raise InvalidRightOperandType(location, operator, right_type)
            if isinstance(left_type, ArrayType):
                if isinstance(right_type, ArrayType):
                    if left_type.base_type == right_type.base_type:
                        return ConcatenationOperation(left_type.base_type, left_type.count, right_type.count)
                raise InvalidRightOperandType(location, operator, right_type)
            raise InvalidLeftOperandType(location, operator, left_type)
        # -
        if isinstance(operator, MinusToken):
            if left_type == Types.CHAR:
                if right_type == Types.CHAR:
                    return SubtractionOperation()
                raise InvalidRightOperandType(location, operator, right_type)
            raise InvalidLeftOperandType(location, operator, left_type)
        # *
        if isinstance(operator, StarToken):
            if left_type == Types.CHAR:
                if right_type == Types.CHAR:
                    return MultiplicationOperation()
                if isinstance(right_type, ArrayType):
                    return ArrayScalingOperation(right_type.count)
                raise InvalidRightOperandType(location, operator, right_type)
            raise InvalidLeftOperandType(location, operator, left_type)
        # /
        if isinstance(operator, SlashToken):
            if left_type == Types.CHAR:
                if right_type == Types.CHAR:
                    return DivisionOperation()
                raise InvalidRightOperandType(location, operator, right_type)
            raise InvalidLeftOperandType(location, operator, left_type)
        # %
        if isinstance(operator, PercentToken):
            if left_type == Types.CHAR:
                if right_type == Types.CHAR:
                    return ModuloOperation()
                raise InvalidRightOperandType(location, operator, right_type)
            raise InvalidLeftOperandType(location, operator, left_type)
        raise CompilerException(f'Unknown binary operator: {operator}')

    @abstractmethod
    def left_type(self) -> DataType:
        ...

    @abstractmethod
    def right_type(self) -> DataType:
        ...

    @abstractmethod
    def type(self) -> DataType:
        ...

    @abstractmethod
    def __str__(self) -> str:
        ...


class EqualityTestOperation(BinaryOperation):
    def __init__(self, operand_type: DataType) -> None:
        self.operand_type = operand_type

    def left_type(self) -> DataType:
        return self.operand_type

    def right_type(self) -> DataType:
        return self.operand_type

    def type(self) -> DataType:
        return Types.CHAR

    def __str__(self) -> str:
        return '=='


class DifferenceTestOperation(BinaryOperation):
    def __init__(self, operand_type: DataType) -> None:
        self.operand_type = operand_type

    def left_type(self) -> DataType:
        return self.operand_type

    def right_type(self) -> DataType:
        return self.operand_type

    def type(self) -> DataType:
        return Types.CHAR

    def __str__(self) -> str:
        return '!='


class StrictInequalityTestOperation(BinaryOperation):

    def left_type(self) -> DataType:
        return Types.CHAR

    def right_type(self) -> DataType:
        return Types.CHAR

    def type(self) -> DataType:
        return Types.CHAR

    def __str__(self) -> str:
        return '<'


class LargeInequalityTestOperation(BinaryOperation):

    def left_type(self) -> DataType:
        return Types.CHAR

    def right_type(self) -> DataType:
        return Types.CHAR

    def type(self) -> DataType:
        return Types.CHAR

    def __str__(self) -> str:
        return '<='


class InverseStrictInequalityTestOperation(BinaryOperation):
    def left_type(self) -> DataType:
        return Types.CHAR

    def right_type(self) -> DataType:
        return Types.CHAR

    def type(self) -> DataType:
        return Types.CHAR

    def __str__(self) -> str:
        return '>'


class InverseLargeInequalityTestOperation(BinaryOperation):

    def left_type(self) -> DataType:
        return Types.CHAR

    def right_type(self) -> DataType:
        return Types.CHAR

    def type(self) -> DataType:
        return Types.CHAR

    def __str__(self) -> str:
        return '>='


class ConjunctionOperation(BinaryOperation):
    def left_type(self) -> DataType:
        return Types.CHAR

    def right_type(self) -> DataType:
        return Types.CHAR

    def type(self) -> DataType:
        return Types.CHAR

    def __str__(self) -> str:
        return '&&'


class DisjunctionOperation(BinaryOperation):
    def left_type(self) -> DataType:
        return Types.CHAR

    def right_type(self) -> DataType:
        return Types.CHAR

    def type(self) -> DataType:
        return Types.CHAR

    def __str__(self) -> str:
        return '||'


class AdditionOperation(BinaryOperation):
    def left_type(self) -> DataType:
        return Types.CHAR

    def right_type(self) -> DataType:
        return Types.CHAR

    def type(self) -> DataType:
        return Types.CHAR

    def __str__(self) -> str:
        return '+'


class SubtractionOperation(BinaryOperation):
    def left_type(self) -> DataType:
        return Types.CHAR

    def right_type(self) -> DataType:
        return Types.CHAR

    def type(self) -> DataType:
        return Types.CHAR

    def __str__(self) -> str:
        return '-'


class MultiplicationOperation(BinaryOperation):
    def left_type(self) -> DataType:
        return Types.CHAR

    def right_type(self) -> DataType:
        return Types.CHAR

    def type(self) -> DataType:
        return Types.CHAR

    def __str__(self) -> str:
        return '*'


class DivisionOperation(BinaryOperation):
    def left_type(self) -> DataType:
        return Types.CHAR

    def right_type(self) -> DataType:
        return Types.CHAR

    def type(self) -> DataType:
        return Types.CHAR

    def __str__(self) -> str:
        return '/'


class ModuloOperation(BinaryOperation):
    def left_type(self) -> DataType:
        return Types.CHAR

    def right_type(self) -> DataType:
        return Types.CHAR

    def type(self) -> DataType:
        return Types.CHAR

    def __str__(self) -> str:
        return '%'


class ConcatenationOperation(BinaryOperation):
    def __init__(self, base_type: DataType, left_array_count: int, right_array_count: int) -> None:
        self.base_type = base_type
        self.left_array_count = left_array_count
        self.right_array_count = right_array_count

    def left_type(self) -> DataType:
        return ArrayType(self.base_type, self.left_array_count)

    def right_type(self) -> DataType:
        return ArrayType(self.base_type, self.right_array_count)

    def type(self) -> DataType:
        return ArrayType(self.base_type, self.left_array_count + self.right_array_count)

    def __str__(self) -> str:
        return '+'


class ArrayScalingOperation(BinaryOperation):
    def __init__(self, array_count: int) -> None:
        self.array_count = array_count

    def left_type(self) -> DataType:
        return Types.CHAR

    def right_type(self) -> DataType:
        return ArrayType(Types.CHAR, self.array_count)

    def type(self) -> DataType:
        return ArrayType(Types.CHAR, self.array_count)

    def __str__(self) -> str:
        return '*'


__all__ = ['BinaryOperation', 'EqualityTestOperation', 'DifferenceTestOperation', 'StrictInequalityTestOperation',
           'LargeInequalityTestOperation', 'InverseStrictInequalityTestOperation',
           'InverseLargeInequalityTestOperation', 'ConjunctionOperation', 'DisjunctionOperation', 'AdditionOperation',
           'SubtractionOperation', 'MultiplicationOperation', 'DivisionOperation', 'ModuloOperation',
           'ConcatenationOperation', 'ArrayScalingOperation']
