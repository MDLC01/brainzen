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
                return EqualityTestOperation(operator, left_type)
            raise NotEqualOperandTypes(location, operator, left_type, right_type)
        # !=
        if isinstance(operator, BangEqualToken):
            if left_type == right_type:
                return DifferenceTestOperation(operator, left_type)
            raise NotEqualOperandTypes(location, operator, left_type, right_type)
        # <
        if isinstance(operator, LessThanToken):
            if left_type == Types.CHAR:
                if right_type == Types.CHAR:
                    return StrictInequalityTestOperation(operator)
                raise InvalidRightOperandType(location, operator, right_type)
            raise InvalidLeftOperandType(location, operator, left_type)
        # <=
        if isinstance(operator, LessThanEqualToken):
            if left_type == Types.CHAR:
                if right_type == Types.CHAR:
                    return LargeInequalityTestOperation(operator)
                raise InvalidRightOperandType(location, operator, right_type)
            raise InvalidLeftOperandType(location, operator, left_type)
        # >
        if isinstance(operator, GreaterThanToken):
            if left_type == Types.CHAR:
                if right_type == Types.CHAR:
                    return InverseStrictInequalityTestOperation(operator)
                raise InvalidRightOperandType(location, operator, right_type)
            raise InvalidLeftOperandType(location, operator, left_type)
        # >=
        if isinstance(operator, GreaterThanEqualToken):
            if left_type == Types.CHAR:
                if right_type == Types.CHAR:
                    return InverseLargeInequalityTestOperation(operator)
                raise InvalidRightOperandType(location, operator, right_type)
            raise InvalidLeftOperandType(location, operator, left_type)
        # &&
        if isinstance(operator, DoubleAmpersandToken):
            if left_type == Types.CHAR:
                if right_type == Types.CHAR:
                    return ConjunctionOperation(operator)
                raise InvalidRightOperandType(location, operator, right_type)
            raise InvalidLeftOperandType(location, operator, left_type)
        # ||
        if isinstance(operator, DoublePipeToken):
            if left_type == Types.CHAR:
                if right_type == Types.CHAR:
                    return DisjunctionOperation(operator)
                raise InvalidRightOperandType(location, operator, right_type)
            raise InvalidLeftOperandType(location, operator, left_type)
        # +
        if isinstance(operator, PlusToken):
            if left_type == Types.CHAR:
                if right_type == Types.CHAR:
                    return AdditionOperation(operator)
                raise InvalidRightOperandType(location, operator, right_type)
            if isinstance(left_type, ArrayType):
                if isinstance(right_type, ArrayType):
                    if left_type.base_type == right_type.base_type:
                        return ConcatenationOperation(operator, left_type.base_type, left_type.count, right_type.count)
                raise InvalidRightOperandType(location, operator, right_type)
            raise InvalidLeftOperandType(location, operator, left_type)
        # -
        if isinstance(operator, MinusToken):
            if left_type == Types.CHAR:
                if right_type == Types.CHAR:
                    return SubtractionOperation(operator)
                raise InvalidRightOperandType(location, operator, right_type)
            raise InvalidLeftOperandType(location, operator, left_type)
        # *
        if isinstance(operator, StarToken):
            if left_type == Types.CHAR:
                if right_type == Types.CHAR:
                    return MultiplicationOperation(operator)
                if isinstance(right_type, ArrayType):
                    return ArrayScalingOperation(operator, right_type.count)
                raise InvalidRightOperandType(location, operator, right_type)
            raise InvalidLeftOperandType(location, operator, left_type)
        # /
        if isinstance(operator, SlashToken):
            if left_type == Types.CHAR:
                if right_type == Types.CHAR:
                    return DivisionOperation(operator)
                raise InvalidRightOperandType(location, operator, right_type)
            raise InvalidLeftOperandType(location, operator, left_type)
        # %
        if isinstance(operator, PercentToken):
            if left_type == Types.CHAR:
                if right_type == Types.CHAR:
                    return ModuloOperation(operator)
                raise InvalidRightOperandType(location, operator, right_type)
            raise InvalidLeftOperandType(location, operator, left_type)
        raise CompilerException(f'Unknown binary operator: {operator}')

    def __init__(self, operator: Token) -> None:
        self.operator = operator

    @abstractmethod
    def left_type(self) -> DataType:
        ...

    @abstractmethod
    def right_type(self) -> DataType:
        ...

    @abstractmethod
    def type(self) -> DataType:
        ...

    def __str__(self) -> str:
        return self.operator.__str__()


class EqualityTestOperation(BinaryOperation):
    def __init__(self, operator: Token, operand_type: DataType) -> None:
        super().__init__(operator)
        self.operand_type = operand_type

    def left_type(self) -> DataType:
        return self.operand_type

    def right_type(self) -> DataType:
        return self.operand_type

    def type(self) -> DataType:
        return Types.CHAR


class DifferenceTestOperation(BinaryOperation):
    def __init__(self, operator: Token, operand_type: DataType) -> None:
        super().__init__(operator)
        self.operand_type = operand_type

    def left_type(self) -> DataType:
        return self.operand_type

    def right_type(self) -> DataType:
        return self.operand_type

    def type(self) -> DataType:
        return Types.CHAR


class StrictInequalityTestOperation(BinaryOperation):
    def left_type(self) -> DataType:
        return Types.CHAR

    def right_type(self) -> DataType:
        return Types.CHAR

    def type(self) -> DataType:
        return Types.CHAR


class LargeInequalityTestOperation(BinaryOperation):
    def left_type(self) -> DataType:
        return Types.CHAR

    def right_type(self) -> DataType:
        return Types.CHAR

    def type(self) -> DataType:
        return Types.CHAR


class InverseStrictInequalityTestOperation(BinaryOperation):
    def left_type(self) -> DataType:
        return Types.CHAR

    def right_type(self) -> DataType:
        return Types.CHAR

    def type(self) -> DataType:
        return Types.CHAR


class InverseLargeInequalityTestOperation(BinaryOperation):
    def left_type(self) -> DataType:
        return Types.CHAR

    def right_type(self) -> DataType:
        return Types.CHAR

    def type(self) -> DataType:
        return Types.CHAR


class ConjunctionOperation(BinaryOperation):
    def left_type(self) -> DataType:
        return Types.CHAR

    def right_type(self) -> DataType:
        return Types.CHAR

    def type(self) -> DataType:
        return Types.CHAR


class DisjunctionOperation(BinaryOperation):
    def left_type(self) -> DataType:
        return Types.CHAR

    def right_type(self) -> DataType:
        return Types.CHAR

    def type(self) -> DataType:
        return Types.CHAR


class AdditionOperation(BinaryOperation):
    def left_type(self) -> DataType:
        return Types.CHAR

    def right_type(self) -> DataType:
        return Types.CHAR

    def type(self) -> DataType:
        return Types.CHAR


class SubtractionOperation(BinaryOperation):
    def left_type(self) -> DataType:
        return Types.CHAR

    def right_type(self) -> DataType:
        return Types.CHAR

    def type(self) -> DataType:
        return Types.CHAR


class MultiplicationOperation(BinaryOperation):
    def left_type(self) -> DataType:
        return Types.CHAR

    def right_type(self) -> DataType:
        return Types.CHAR

    def type(self) -> DataType:
        return Types.CHAR


class DivisionOperation(BinaryOperation):
    def left_type(self) -> DataType:
        return Types.CHAR

    def right_type(self) -> DataType:
        return Types.CHAR

    def type(self) -> DataType:
        return Types.CHAR


class ModuloOperation(BinaryOperation):
    def left_type(self) -> DataType:
        return Types.CHAR

    def right_type(self) -> DataType:
        return Types.CHAR

    def type(self) -> DataType:
        return Types.CHAR


class ConcatenationOperation(BinaryOperation):
    def __init__(self, operator: Token, base_type: DataType, left_array_count: int, right_array_count: int) -> None:
        super().__init__(operator)
        self.base_type = base_type
        self.left_array_count = left_array_count
        self.right_array_count = right_array_count

    def left_type(self) -> DataType:
        return ArrayType(self.base_type, self.left_array_count)

    def right_type(self) -> DataType:
        return ArrayType(self.base_type, self.right_array_count)

    def type(self) -> DataType:
        return ArrayType(self.base_type, self.left_array_count + self.right_array_count)


class ArrayScalingOperation(BinaryOperation):
    def __init__(self, operator: Token, array_count: int) -> None:
        super().__init__(operator)
        self.array_count = array_count

    def left_type(self) -> DataType:
        return Types.CHAR

    def right_type(self) -> DataType:
        return ArrayType(Types.CHAR, self.array_count)

    def type(self) -> DataType:
        return ArrayType(Types.CHAR, self.array_count)


__all__ = ['BinaryOperation', 'EqualityTestOperation', 'DifferenceTestOperation', 'StrictInequalityTestOperation',
           'LargeInequalityTestOperation', 'InverseStrictInequalityTestOperation',
           'InverseLargeInequalityTestOperation', 'ConjunctionOperation', 'DisjunctionOperation', 'AdditionOperation',
           'SubtractionOperation', 'MultiplicationOperation', 'DivisionOperation', 'ModuloOperation',
           'ConcatenationOperation', 'ArrayScalingOperation']
