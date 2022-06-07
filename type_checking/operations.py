from enum import Enum, auto

from data_types import *
from exceptions import *
from tokenization import BinaryOperator, UnaryOperator


class InvalidOperandType(CompilationException):
    def __init__(self, location: Location, operator: UnaryOperator, operand_type: DataType) -> None:
        super().__init__(location, f'Invalid operand type for {operator}: {operand_type}')


class UnaryOperationType(Enum):
    NEGATION = auto()
    BOOL_NORMALIZATION = auto()
    OPPOSITION = auto()


class UnaryOperation:
    @staticmethod
    def _invalid_operand_type_exception(location: Location, operator: UnaryOperator,
                                        operand_type: DataType) -> CompilationException:
        return CompilationException(location, f'Invalid operand type for {operator}: {operand_type}')

    @classmethod
    def from_operator(cls, location: Location, operator: UnaryOperator, operand_type: DataType) -> 'UnaryOperation':
        # !
        if operator is UnaryOperator.BANG:
            if operand_type == Types.CHAR:
                return cls(UnaryOperationType.NEGATION)
            raise InvalidOperandType(location, operator, operand_type)
        # !!
        if operator is UnaryOperator.DOUBLE_BANG:
            if operand_type == Types.CHAR:
                return cls(UnaryOperationType.BOOL_NORMALIZATION)
            raise InvalidOperandType(location, operator, operand_type)
        # -
        if operator is UnaryOperator.MINUS:
            if operand_type == Types.CHAR:
                return cls(UnaryOperationType.OPPOSITION)
            raise InvalidOperandType(location, operator, operand_type)
        raise CompilerException(f'Unknown unary operator: {operator}')

    def __init__(self, operation_type: UnaryOperationType, return_type: DataType = Types.CHAR) -> None:
        self.operation_type = operation_type
        self.return_type = return_type


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


class BinaryOperationType(Enum):
    EQUALITY_TEST = auto()
    DIFFERENCE_TEST = auto()
    STRICT_INEQUALITY_TEST = auto()
    LARGE_INEQUALITY_TEST = auto()
    INVERSE_STRICT_INEQUALITY_TEST = auto()
    INVERSE_LARGE_INEQUALITY_TEST = auto()
    CONJUNCTION = auto()
    DISJUNCTION = auto()
    ADDITION = auto()
    SUBTRACTION = auto()
    MULTIPLICATION = auto()
    DIVISION = auto()
    MODULO_OPERATION = auto()
    CONCATENATION = auto()


class BinaryOperation:
    @classmethod
    def from_operator(cls, location: Location, operator: BinaryOperator, left_type: DataType,
                      right_type: DataType) -> 'BinaryOperation':
        # ==
        if operator is BinaryOperator.DOUBLE_EQUAL:
            if left_type == right_type:
                return cls(BinaryOperationType.EQUALITY_TEST)
            raise NotEqualOperandTypes(location, operator, left_type, right_type)
        # !=
        if operator is BinaryOperator.BANG_EQUAL:
            if left_type == right_type:
                return cls(BinaryOperationType.DIFFERENCE_TEST)
            raise NotEqualOperandTypes(location, operator, left_type, right_type)
        # <
        if operator is BinaryOperator.LESS_THAN:
            if left_type == Types.CHAR:
                if right_type == Types.CHAR:
                    return cls(BinaryOperationType.STRICT_INEQUALITY_TEST)
                raise InvalidRightOperandType(location, operator, right_type)
            raise InvalidLeftOperandType(location, operator, left_type)
        # <=
        if operator is BinaryOperator.LESS_THAN_EQUAL:
            if left_type == Types.CHAR:
                if right_type == Types.CHAR:
                    return cls(BinaryOperationType.LARGE_INEQUALITY_TEST)
                raise InvalidRightOperandType(location, operator, right_type)
            raise InvalidLeftOperandType(location, operator, left_type)
        # >
        if operator is BinaryOperator.GREATER_THAN:
            if left_type == Types.CHAR:
                if right_type == Types.CHAR:
                    return cls(BinaryOperationType.INVERSE_STRICT_INEQUALITY_TEST)
                raise InvalidRightOperandType(location, operator, right_type)
            raise InvalidLeftOperandType(location, operator, left_type)
        # >=
        if operator is BinaryOperator.GREATER_THAN_EQUAL:
            if left_type == Types.CHAR:
                if right_type == Types.CHAR:
                    return cls(BinaryOperationType.INVERSE_LARGE_INEQUALITY_TEST)
                raise InvalidRightOperandType(location, operator, right_type)
            raise InvalidLeftOperandType(location, operator, left_type)
        # &&
        if operator is BinaryOperator.DOUBLE_AMPERSAND:
            if left_type == Types.CHAR:
                if right_type == Types.CHAR:
                    return cls(BinaryOperationType.CONJUNCTION)
                raise InvalidRightOperandType(location, operator, right_type)
            raise InvalidLeftOperandType(location, operator, left_type)
        # ||
        if operator is BinaryOperator.DOUBLE_PIPE:
            if left_type == Types.CHAR:
                if right_type == Types.CHAR:
                    return cls(BinaryOperationType.DISJUNCTION)
                raise InvalidRightOperandType(location, operator, right_type)
            raise InvalidLeftOperandType(location, operator, left_type)
        # +
        if operator is BinaryOperator.PLUS:
            if left_type == Types.CHAR:
                if right_type == Types.CHAR:
                    return cls(BinaryOperationType.ADDITION)
                raise InvalidRightOperandType(location, operator, right_type)
            raise InvalidLeftOperandType(location, operator, left_type)
        # -
        if operator is BinaryOperator.MINUS:
            if left_type == Types.CHAR:
                if right_type == Types.CHAR:
                    return cls(BinaryOperationType.SUBTRACTION)
                raise InvalidRightOperandType(location, operator, right_type)
            raise InvalidLeftOperandType(location, operator, left_type)
        # *
        if operator is BinaryOperator.STAR:
            if left_type == Types.CHAR:
                if right_type == Types.CHAR:
                    return cls(BinaryOperationType.MULTIPLICATION)
                raise InvalidRightOperandType(location, operator, right_type)
            raise InvalidLeftOperandType(location, operator, left_type)
        # /
        if operator is BinaryOperator.SLASH:
            if left_type == Types.CHAR:
                if right_type == Types.CHAR:
                    return cls(BinaryOperationType.DIVISION)
                raise InvalidRightOperandType(location, operator, right_type)
            raise InvalidLeftOperandType(location, operator, left_type)
        # %
        if operator is BinaryOperator.PERCENT:
            if left_type == Types.CHAR:
                if right_type == Types.CHAR:
                    return cls(BinaryOperationType.MODULO_OPERATION)
                raise InvalidRightOperandType(location, operator, right_type)
            raise InvalidLeftOperandType(location, operator, left_type)
        # ..
        if operator is BinaryOperator.DOUBLE_DOT:
            if isinstance(left_type, ArrayType):
                if isinstance(right_type, ArrayType):
                    if left_type.base_type == right_type.base_type:
                        total_size = left_type.size() + right_type.size()
                        return cls(BinaryOperationType.CONCATENATION, ArrayType(left_type.base_type, total_size))
                raise InvalidRightOperandType(location, operator, right_type)
            raise InvalidLeftOperandType(location, operator, left_type)
        raise CompilerException(f'Unknown binary operator: {operator}')

    def __init__(self, operation_type: BinaryOperationType, return_type: DataType = Types.CHAR) -> None:
        self.operation_type = operation_type
        self.return_type = return_type


__all__ = ['UnaryOperationType', 'UnaryOperation', 'BinaryOperationType', 'BinaryOperation']
