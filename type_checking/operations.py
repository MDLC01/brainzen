from enum import Enum

from data_types import *
from exceptions import *
from tokenization import BinaryOperator, UnaryOperator


class InvalidOperandType(CompilationException):
    def __init__(self, location: Location, operator: UnaryOperator, operand_type: DataType) -> None:
        super().__init__(location, f'Invalid operand type for {operator}: {operand_type}')


class UnaryOperation(Enum):
    @staticmethod
    def _invalid_operand_type_exception(location: Location, operator: UnaryOperator,
                                        operand_type: DataType) -> CompilationException:
        return CompilationException(location, f'Invalid operand type for {operator}')

    @classmethod
    def from_operator(cls, location: Location, operator: UnaryOperator, operand_type: DataType) -> 'UnaryOperation':
        # !
        if operator is UnaryOperator.BANG:
            if operand_type == Types.CHAR:
                return cls.NEGATION
            raise InvalidOperandType(location, operator, operand_type)
        # !!
        if operator is UnaryOperator.DOUBLE_BANG:
            if operand_type == Types.CHAR:
                return cls.BOOL_NORMALIZATION
            raise InvalidOperandType(location, operator, operand_type)
        # -
        if operator is UnaryOperator.MINUS:
            if operand_type == Types.CHAR:
                return cls.OPPOSITION
            raise InvalidOperandType(location, operator, operand_type)
        raise CompilerException(f'Unknown unary operator: {operator}')

    NEGATION = UnaryOperator.BANG
    BOOL_NORMALIZATION = UnaryOperator.DOUBLE_BANG
    OPPOSITION = UnaryOperator.MINUS


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


class BinaryOperation(Enum):
    @classmethod
    def from_operator(cls, location: Location, operator: BinaryOperator, left_type: DataType,
                      right_type: DataType) -> 'BinaryOperation':
        # ==
        if operator is BinaryOperator.DOUBLE_EQUAL:
            if left_type == right_type:
                return cls.EQUALITY_TEST
            raise NotEqualOperandTypes(location, operator, left_type, right_type)
        # !=
        if operator is BinaryOperator.BANG_EQUAL:
            if left_type == right_type:
                return cls.DIFFERENCE_TEST
            raise NotEqualOperandTypes(location, operator, left_type, right_type)
        # <
        if operator is BinaryOperator.LESS_THAN:
            if left_type == Types.CHAR:
                if right_type == Types.CHAR:
                    return cls.STRICT_INEQUALITY_TEST
                raise InvalidRightOperandType(location, operator, right_type)
            raise InvalidLeftOperandType(location, operator, left_type)
        # <=
        if operator is BinaryOperator.LESS_THAN_EQUAL:
            if left_type == Types.CHAR:
                if right_type == Types.CHAR:
                    return cls.LARGE_INEQUALITY_TEST
                raise InvalidRightOperandType(location, operator, right_type)
            raise InvalidLeftOperandType(location, operator, left_type)
        # >
        if operator is BinaryOperator.GREATER_THAN:
            if left_type == Types.CHAR:
                if right_type == Types.CHAR:
                    return cls.INVERSE_STRICT_INEQUALITY_TEST
                raise InvalidRightOperandType(location, operator, right_type)
            raise InvalidLeftOperandType(location, operator, left_type)
        # >=
        if operator is BinaryOperator.GREATER_THAN_EQUAL:
            if left_type == Types.CHAR:
                if right_type == Types.CHAR:
                    return cls.INVERSE_LARGE_INEQUALITY_TEST
                raise InvalidRightOperandType(location, operator, right_type)
            raise InvalidLeftOperandType(location, operator, left_type)
        # &&
        if operator is BinaryOperator.DOUBLE_AMPERSAND:
            if left_type == Types.CHAR:
                if right_type == Types.CHAR:
                    return cls.CONJUNCTION
                raise InvalidRightOperandType(location, operator, right_type)
            raise InvalidLeftOperandType(location, operator, left_type)
        # ||
        if operator is BinaryOperator.DOUBLE_PIPE:
            if left_type == Types.CHAR:
                if right_type == Types.CHAR:
                    return cls.DISJUNCTION
                raise InvalidRightOperandType(location, operator, right_type)
            raise InvalidLeftOperandType(location, operator, left_type)
        # +
        if operator is BinaryOperator.PLUS:
            if left_type == Types.CHAR:
                if right_type == Types.CHAR:
                    return cls.ADDITION
                raise InvalidRightOperandType(location, operator, right_type)
            raise InvalidLeftOperandType(location, operator, left_type)
        # -
        if operator is BinaryOperator.MINUS:
            if left_type == Types.CHAR:
                if right_type == Types.CHAR:
                    return cls.SUBTRACTION
                raise InvalidRightOperandType(location, operator, right_type)
            raise InvalidLeftOperandType(location, operator, left_type)
        # *
        if operator is BinaryOperator.STAR:
            if left_type == Types.CHAR:
                if right_type == Types.CHAR:
                    return cls.MULTIPLICATION
                raise InvalidRightOperandType(location, operator, right_type)
            raise InvalidLeftOperandType(location, operator, left_type)
        # /
        if operator is BinaryOperator.SLASH:
            if left_type == Types.CHAR:
                if right_type == Types.CHAR:
                    return cls.DIVISION
                raise InvalidRightOperandType(location, operator, right_type)
            raise InvalidLeftOperandType(location, operator, left_type)
        # %
        if operator is BinaryOperator.PERCENT:
            if left_type == Types.CHAR:
                if right_type == Types.CHAR:
                    return cls.MODULO_OPERATION
                raise InvalidRightOperandType(location, operator, right_type)
            raise InvalidLeftOperandType(location, operator, left_type)
        # ..
        if operator is BinaryOperator.DOUBLE_DOT:
            if isinstance(left_type, ArrayType):
                if isinstance(right_type, ArrayType):
                    if left_type.base_type == right_type.base_type:
                        return cls.CONCATENATION
                raise InvalidRightOperandType(location, operator, right_type)
            raise InvalidLeftOperandType(location, operator, left_type)
        raise CompilerException(f'Unknown binary operator: {operator}')

    EQUALITY_TEST = BinaryOperator.DOUBLE_EQUAL
    DIFFERENCE_TEST = BinaryOperator.BANG_EQUAL
    STRICT_INEQUALITY_TEST = BinaryOperator.LESS_THAN
    LARGE_INEQUALITY_TEST = BinaryOperator.LESS_THAN_EQUAL
    INVERSE_STRICT_INEQUALITY_TEST = BinaryOperator.GREATER_THAN
    INVERSE_LARGE_INEQUALITY_TEST = BinaryOperator.GREATER_THAN_EQUAL
    CONJUNCTION = BinaryOperator.DOUBLE_AMPERSAND
    DISJUNCTION = BinaryOperator.DOUBLE_PIPE
    ADDITION = BinaryOperator.PLUS
    SUBTRACTION = BinaryOperator.MINUS
    MULTIPLICATION = BinaryOperator.STAR
    DIVISION = BinaryOperator.SLASH
    MODULO_OPERATION = BinaryOperator.PERCENT
    CONCATENATION = BinaryOperator.DOUBLE_DOT


__all__ = ['UnaryOperation', 'BinaryOperation']
