from abc import ABC, abstractmethod

from data_types import *
from exceptions import *
from tokenization.operators import BinaryOperator


class InvalidLeftOperandType(CompilationException):
    def __init__(self, location: Location, operator: BinaryOperator, operand_type: DataType) -> None:
        super().__init__(location, f'Invalid left operand type for {operator}: {operand_type}')


class InvalidRightOperandType(CompilationException):
    def __init__(self, location: Location, operator: BinaryOperator, operand_type: DataType) -> None:
        super().__init__(location, f'Invalid right operand type for {operator}: {operand_type}')


class NotEqualOperandTypes(CompilationException):
    def __init__(self, location: Location, operator: BinaryOperator, left_type: DataType, right_type: DataType) -> None:
        message = f'Invalid operand types for {operator}: both operand must be of the same type,' \
                  f' but found {left_type} and {right_type}'
        super().__init__(location, message)


class BinaryOperation(ABC):
    @classmethod
    def from_operator(cls, location: Location, operator: BinaryOperator, left_type: DataType,
                      right_type: DataType) -> 'BinaryOperation':
        # ==
        if operator is BinaryOperator.DOUBLE_EQUAL:
            if left_type == right_type:
                return EqualityTestOperation(left_type)
            raise NotEqualOperandTypes(location, operator, left_type, right_type)
        # !=
        if operator is BinaryOperator.BANG_EQUAL:
            if left_type == right_type:
                return DifferenceTestOperation(left_type)
            raise NotEqualOperandTypes(location, operator, left_type, right_type)
        # <
        if operator is BinaryOperator.LESS_THAN:
            if left_type == Types.CHAR:
                if right_type == Types.CHAR:
                    return StrictInequalityTestOperation()
                raise InvalidRightOperandType(location, operator, right_type)
            raise InvalidLeftOperandType(location, operator, left_type)
        # <=
        if operator is BinaryOperator.LESS_THAN_EQUAL:
            if left_type == Types.CHAR:
                if right_type == Types.CHAR:
                    return LargeInequalityTestOperation()
                raise InvalidRightOperandType(location, operator, right_type)
            raise InvalidLeftOperandType(location, operator, left_type)
        # >
        if operator is BinaryOperator.GREATER_THAN:
            if left_type == Types.CHAR:
                if right_type == Types.CHAR:
                    return InverseStrictInequalityTestOperation()
                raise InvalidRightOperandType(location, operator, right_type)
            raise InvalidLeftOperandType(location, operator, left_type)
        # >=
        if operator is BinaryOperator.GREATER_THAN_EQUAL:
            if left_type == Types.CHAR:
                if right_type == Types.CHAR:
                    return InverseLargeInequalityTestOperation()
                raise InvalidRightOperandType(location, operator, right_type)
            raise InvalidLeftOperandType(location, operator, left_type)
        # &&
        if operator is BinaryOperator.DOUBLE_AMPERSAND:
            if left_type == Types.CHAR:
                if right_type == Types.CHAR:
                    return ConjunctionOperation()
                raise InvalidRightOperandType(location, operator, right_type)
            raise InvalidLeftOperandType(location, operator, left_type)
        # ||
        if operator is BinaryOperator.DOUBLE_PIPE:
            if left_type == Types.CHAR:
                if right_type == Types.CHAR:
                    return DisjunctionOperation()
                raise InvalidRightOperandType(location, operator, right_type)
            raise InvalidLeftOperandType(location, operator, left_type)
        # +
        if operator is BinaryOperator.PLUS:
            if left_type == Types.CHAR:
                if right_type == Types.CHAR:
                    return AdditionOperation()
                raise InvalidRightOperandType(location, operator, right_type)
            raise InvalidLeftOperandType(location, operator, left_type)
        # -
        if operator is BinaryOperator.MINUS:
            if left_type == Types.CHAR:
                if right_type == Types.CHAR:
                    return SubtractionOperation()
                raise InvalidRightOperandType(location, operator, right_type)
            raise InvalidLeftOperandType(location, operator, left_type)
        # *
        if operator is BinaryOperator.STAR:
            if left_type == Types.CHAR:
                if right_type == Types.CHAR:
                    return MultiplicationOperation()
                if isinstance(right_type, ArrayType):
                    base_operation = cls.from_operator(location, operator, left_type, right_type.base_type)
                    return BinaryTermByTermArrayOperation(base_operation, right_type.count)
                raise InvalidRightOperandType(location, operator, right_type)
            raise InvalidLeftOperandType(location, operator, left_type)
        # /
        if operator is BinaryOperator.SLASH:
            if left_type == Types.CHAR:
                if right_type == Types.CHAR:
                    return DivisionOperation()
                raise InvalidRightOperandType(location, operator, right_type)
            raise InvalidLeftOperandType(location, operator, left_type)
        # %
        if operator is BinaryOperator.PERCENT:
            if left_type == Types.CHAR:
                if right_type == Types.CHAR:
                    return ModuloOperation()
                raise InvalidRightOperandType(location, operator, right_type)
            raise InvalidLeftOperandType(location, operator, left_type)
        # ..
        if operator is BinaryOperator.DOUBLE_DOT:
            if isinstance(left_type, ArrayType):
                if isinstance(right_type, ArrayType):
                    if left_type.base_type == right_type.base_type:
                        return ConcatenationOperation(left_type.base_type, left_type.count, right_type.count)
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


class EqualityTestOperation(BinaryOperation):
    def __init__(self, operand_type: DataType) -> None:
        self.operand_type = operand_type

    def left_type(self) -> DataType:
        return self.operand_type

    def right_type(self) -> DataType:
        return self.operand_type

    def type(self) -> DataType:
        return Types.CHAR


class DifferenceTestOperation(BinaryOperation):
    def __init__(self, operand_type: DataType) -> None:
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


class BinaryTermByTermArrayOperation(BinaryOperation):
    def __init__(self, operation: BinaryOperation, array_count: int):
        self.operation = operation
        self.array_count = array_count

    def left_type(self) -> DataType:
        return self.operation.left_type()

    def right_type(self) -> DataType:
        return ArrayType(self.operation.right_type(), self.array_count)

    def type(self) -> DataType:
        return ArrayType(self.operation.type(), self.array_count)


__all__ = ['BinaryOperation', 'EqualityTestOperation', 'DifferenceTestOperation', 'StrictInequalityTestOperation',
           'LargeInequalityTestOperation', 'InverseStrictInequalityTestOperation',
           'InverseLargeInequalityTestOperation', 'ConjunctionOperation', 'DisjunctionOperation', 'AdditionOperation',
           'SubtractionOperation', 'MultiplicationOperation', 'DivisionOperation', 'ModuloOperation',
           'ConcatenationOperation', 'BinaryTermByTermArrayOperation']
