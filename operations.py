from enum import IntEnum


class UnaryOperation:
    def __init__(self, symbol: str):
        self.symbol = symbol

    def __str__(self) -> str:
        return self.symbol

    def __repr__(self) -> str:
        return f'{self.__class__.__name__}[{self.symbol}]'

    BOOL_NORMALIZATION: 'UnaryOperation'
    NEGATION: 'UnaryOperation'
    OPPOSITION: 'UnaryOperation'


UnaryOperation.BOOL_NORMALIZATION = UnaryOperation('!!')
UnaryOperation.NEGATION = UnaryOperation('!')
UnaryOperation.OPPOSITION = UnaryOperation('-')


class Priority(IntEnum):
    MIN = 0
    CONJUNCTION = 1
    DISJUNCTION = 2
    COMPARISON = 3
    ADDITION = 4
    MULTIPLICATION = 5
    CONCATENATION = 6
    PARENTHESIS = 254
    MAX = 255


class BinaryOperation:
    def __init__(self, symbol: str, priority: Priority):
        self.symbol = symbol
        self.priority = priority

    def __str__(self) -> str:
        return self.symbol

    def __repr__(self) -> str:
        return f'{self.__class__.__name__}[{self.symbol}]'

    EQUALITY_TEST: 'BinaryOperation'
    DIFFERENCE_TEST: 'BinaryOperation'
    STRICT_INEQUALITY_TEST: 'BinaryOperation'
    LARGE_INEQUALITY_TEST: 'BinaryOperation'
    INVERSE_STRICT_INEQUALITY_TEST: 'BinaryOperation'
    INVERSE_LARGE_INEQUALITY_TEST: 'BinaryOperation'
    CONJUNCTION: 'BinaryOperation'
    DISJUNCTION: 'BinaryOperation'
    ADDITION: 'BinaryOperation'
    SUBTRACTION: 'BinaryOperation'
    MULTIPLICATION: 'BinaryOperation'
    DIVISION: 'BinaryOperation'
    MODULO_OPERATION: 'BinaryOperation'
    CONCATENATION: 'BinaryOperation'


BinaryOperation.EQUALITY_TEST = BinaryOperation('==', Priority.COMPARISON)
BinaryOperation.DIFFERENCE_TEST = BinaryOperation('!=', Priority.COMPARISON)
BinaryOperation.STRICT_INEQUALITY_TEST = BinaryOperation('<', Priority.COMPARISON)
BinaryOperation.LARGE_INEQUALITY_TEST = BinaryOperation('<=', Priority.COMPARISON)
BinaryOperation.INVERSE_STRICT_INEQUALITY_TEST = BinaryOperation('>', Priority.COMPARISON)
BinaryOperation.INVERSE_LARGE_INEQUALITY_TEST = BinaryOperation('>=', Priority.COMPARISON)
BinaryOperation.CONJUNCTION = BinaryOperation('&&', Priority.CONJUNCTION)
BinaryOperation.DISJUNCTION = BinaryOperation('||', Priority.DISJUNCTION)
BinaryOperation.ADDITION = BinaryOperation('+', Priority.ADDITION)
BinaryOperation.SUBTRACTION = BinaryOperation('-', Priority.ADDITION)
BinaryOperation.MULTIPLICATION = BinaryOperation('*', Priority.MULTIPLICATION)
BinaryOperation.DIVISION = BinaryOperation('/', Priority.MULTIPLICATION)
BinaryOperation.MODULO_OPERATION = BinaryOperation('%', Priority.MULTIPLICATION)
BinaryOperation.CONCATENATION = BinaryOperation('::', Priority.CONCATENATION)

__all__ = ['UnaryOperation', 'Priority', 'BinaryOperation']
