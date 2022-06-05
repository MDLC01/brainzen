from enum import IntEnum


class UnaryOperator:
    def __init__(self, symbol: str):
        self.symbol = symbol

    def __str__(self) -> str:
        return self.symbol

    def __repr__(self) -> str:
        return f'{self.__class__.__name__}[{self.symbol}]'

    BANG: 'UnaryOperator'
    DOUBLE_BANG: 'UnaryOperator'
    MINUS: 'UnaryOperator'


UnaryOperator.BANG = UnaryOperator('!')
UnaryOperator.DOUBLE_BANG = UnaryOperator('!!')
UnaryOperator.MINUS = UnaryOperator('-')


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


class BinaryOperator:
    def __init__(self, symbol: str, priority: Priority):
        self.symbol = symbol
        self.priority = priority

    def __str__(self) -> str:
        return self.symbol

    def __repr__(self) -> str:
        return f'{self.__class__.__name__}[{self.symbol}]'

    DOUBLE_EQUAL: 'BinaryOperator'
    BANG_EQUAL: 'BinaryOperator'
    LESS_THAN: 'BinaryOperator'
    LESS_THAN_EQUAL: 'BinaryOperator'
    GREATER_THAN: 'BinaryOperator'
    GREATER_THAN_EQUAL: 'BinaryOperator'
    DOUBLE_AMPERSAND: 'BinaryOperator'
    DOUBLE_PIPE: 'BinaryOperator'
    PLUS: 'BinaryOperator'
    MINUS: 'BinaryOperator'
    STAR: 'BinaryOperator'
    SLASH: 'BinaryOperator'
    PERCENT: 'BinaryOperator'
    DOUBLE_DOT: 'BinaryOperator'


BinaryOperator.DOUBLE_EQUAL = BinaryOperator('==', Priority.COMPARISON)
BinaryOperator.BANG_EQUAL = BinaryOperator('!=', Priority.COMPARISON)
BinaryOperator.LESS_THAN = BinaryOperator('<', Priority.COMPARISON)
BinaryOperator.LESS_THAN_EQUAL = BinaryOperator('<=', Priority.COMPARISON)
BinaryOperator.GREATER_THAN = BinaryOperator('>', Priority.COMPARISON)
BinaryOperator.GREATER_THAN_EQUAL = BinaryOperator('>=', Priority.COMPARISON)
BinaryOperator.DOUBLE_AMPERSAND = BinaryOperator('&&', Priority.CONJUNCTION)
BinaryOperator.DOUBLE_PIPE = BinaryOperator('||', Priority.DISJUNCTION)
BinaryOperator.PLUS = BinaryOperator('+', Priority.ADDITION)
BinaryOperator.MINUS = BinaryOperator('-', Priority.ADDITION)
BinaryOperator.STAR = BinaryOperator('*', Priority.MULTIPLICATION)
BinaryOperator.SLASH = BinaryOperator('/', Priority.MULTIPLICATION)
BinaryOperator.PERCENT = BinaryOperator('%', Priority.MULTIPLICATION)
BinaryOperator.DOUBLE_DOT = BinaryOperator('..', Priority.CONCATENATION)

__all__ = ['UnaryOperator', 'Priority', 'BinaryOperator']
