from typing import Type, TypeVar

from exceptions import *
from tokenization.operators import BinaryOperator, UnaryOperator


def is_allowed_in_word(char: str) -> bool:
    return char in '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_'


def is_valid_word(word: str) -> bool:
    return all(is_allowed_in_word(char) for char in word)


AnyToken = TypeVar('AnyToken', bound='Token')


class Token:
    __token_map: dict[str, Type[AnyToken]] = {}
    __keyword_map: dict[str, Type[AnyToken]] = {}

    @staticmethod
    def is_valid_token(token: str) -> bool:
        return token in Token.__token_map

    @staticmethod
    def is_valid_keyword(word: str) -> bool:
        return word in Token.__keyword_map

    @staticmethod
    def of(token: str, location: Location) -> 'Token':
        if Token.is_valid_token(token):
            return Token.__token_map[token](location)
        if Token.is_valid_keyword(token):
            return Token.__keyword_map[token](location)
        raise ImpossibleException(f'Unknown token: {token!r}.')

    def __init_subclass__(cls, **kwargs) -> None:
        super().__init_subclass__(**kwargs)
        if hasattr(cls, 'token') and cls.token is not None:  # noqa
            if cls.token in Token.__token_map:
                message = f'Token {cls.token!r} is already defined in {Token.__token_map[cls.token].__name__}'
                raise CompilerException(message)
            Token.__token_map[cls.token] = cls
            if cls.__doc__ is None:
                cls.__doc__ = f'{cls.token!r}'
        else:
            cls.token: str | None = None

        if hasattr(cls, 'keyword') and cls.keyword is not None:  # noqa
            if cls.keyword in Token.__keyword_map:
                message = f'Keyword {cls.keyword!r} is already defined in {Token.__keyword_map[cls.keyword].__name__}'
                raise CompilerException(message)
            Token.__keyword_map[cls.keyword] = cls
            if cls.__doc__ is None:
                cls.__doc__ = f'keyword {cls.keyword!r}'
        else:
            cls.keyword: str | None = None

        if not hasattr(cls, 'binary_operator'):
            cls.binary_operator: BinaryOperator | None = None

        if not hasattr(cls, 'unary_operator'):
            cls.unary_operator: UnaryOperator | None = None

    def __init__(self, location: Location) -> None:
        self.location = location

    def __repr__(self) -> str:
        return self.__class__.__name__

    def is_unary_operator(self) -> bool:
        return self.unary_operator is not None

    def is_binary_operator(self) -> bool:
        return self.binary_operator is not None


class OpenBraceToken(Token):
    """opening brace"""
    token = '{'


class CloseBraceToken(Token):
    """closing brace"""
    token = '}'


class OpenBracketToken(Token):
    """opening bracket"""
    token = '['


class CloseBracketToken(Token):
    """closing bracket"""
    token = ']'


class OpenParToken(Token):
    """opening parenthesis"""
    token = '('


class CloseParToken(Token):
    """closing parenthesis"""
    token = ')'


class SemicolonToken(Token):
    """semicolon"""
    token = ';'


class CommaToken(Token):
    token = ','


class EqualToken(Token):
    token = '='


class DoublePlusToken(Token):
    token = '++'


class DoubleMinusToken(Token):
    token = '--'


class BangToken(Token):
    token = '!'
    unary_operator = UnaryOperator.BANG


class DoubleBangToken(Token):
    token = '!!'
    unary_operator = UnaryOperator.DOUBLE_BANG


class DoubleEqualToken(Token):
    token = '=='
    binary_operator = BinaryOperator.DOUBLE_EQUAL


class BangEqualToken(Token):
    token = '!='
    binary_operator = BinaryOperator.BANG_EQUAL


class LessThanToken(Token):
    token = '<'
    binary_operator = BinaryOperator.LESS_THAN


class LessThanEqualToken(Token):
    token = '<='
    binary_operator = BinaryOperator.LESS_THAN_EQUAL


class GreaterThanToken(Token):
    token = '>'
    binary_operator = BinaryOperator.GREATER_THAN


class GreaterThanEqualToken(Token):
    token = '>='
    binary_operator = BinaryOperator.GREATER_THAN_EQUAL


class DoubleAmpersandToken(Token):
    token = '&&'
    binary_operator = BinaryOperator.DOUBLE_AMPERSAND


class DoublePipeToken(Token):
    token = '||'
    binary_operator = BinaryOperator.DOUBLE_PIPE


class PlusToken(Token):
    token = '+'
    binary_operator = BinaryOperator.PLUS


class MinusToken(Token):
    token = '-'
    binary_operator = BinaryOperator.MINUS
    unary_operator = UnaryOperator.MINUS


class StarToken(Token):
    token = '*'
    binary_operator = BinaryOperator.STAR


class SlashToken(Token):
    token = '/'
    binary_operator = BinaryOperator.SLASH


class PercentToken(Token):
    token = '%'
    binary_operator = BinaryOperator.PERCENT


class DoubleDotToken(Token):
    token = '..'
    binary_operator = BinaryOperator.DOUBLE_DOT


class ColonToken(Token):
    token = ':'


class DoubleColonToken(Token):
    token = '::'


class ArrowToken(Token):
    token = '->'


class TildeToken(Token):
    token = '~'


class HashToken(Token):
    token = '#'


class QuestionMarkToken(Token):
    token = '?'


class NamespaceKeyword(Token):
    keyword = 'namespace'


class NativeKeyword(Token):
    keyword = 'native'


class ProcKeyword(Token):
    keyword = 'proc'


class FuncKeyword(Token):
    keyword = 'func'


class LetKeyword(Token):
    keyword = 'let'


class LoopKeyword(Token):
    keyword = 'loop'


class ForKeyword(Token):
    keyword = 'for'


class WhileKeyword(Token):
    keyword = 'while'


class DoKeyword(Token):
    keyword = 'do'


class IfKeyword(Token):
    keyword = 'if'


class ElseKeyword(Token):
    keyword = 'else'


class ReturnKeyword(Token):
    keyword = 'return'


class EOFToken(Token):
    """end of file"""


class NumberLiteral(Token):
    """number literal"""

    def __init__(self, location: Location, value: int) -> None:
        super().__init__(location)
        self.value = value
        if value < 0 or value > 255:
            raise CompilationException(location, f'Invalid {self.__doc__}: literal must be between 0 and 255')

    def __str__(self) -> str:
        return str(self.value)

    def __repr__(self) -> str:
        return f'{self.__class__.__name__}[{self.value}]'


class NumericLiteral(NumberLiteral):
    """numeric literal"""

    @classmethod
    def parse(cls, location: Location, word: str) -> 'NumericLiteral':
        if word.startswith('0x'):
            return cls(location, int(word[2:], base=16))
        return cls(location, int(word))

    def __init__(self, location: Location, value: int) -> None:
        super().__init__(location, value)


class CharacterLiteral(NumberLiteral):
    """character literal"""

    def __init__(self, location: Location, value: str) -> None:
        try:
            super().__init__(location, ord(value))
        except Exception:
            raise CompilationException(location, f'Invalid {self.__doc__}: non-ascii character found: {value!r}')

    def __str__(self) -> str:
        return chr(self.value)


class StringLiteral(Token):
    """string literal"""

    def __init__(self, location: Location, value: str) -> None:
        super().__init__(location)
        self.value = value

    def __str__(self) -> str:
        return self.value

    def __repr__(self) -> str:
        return f'{self.__class__.__name__}[{self.value!r}]'


class IdentifierToken(Token):
    """identifier"""

    def __init__(self, location: Location, name: str) -> None:
        super().__init__(location)
        self.name = name
        if not is_valid_word(name):
            raise CompilationException(location, f'Invalid identifier: {name!r}')

    def __str__(self) -> str:
        return self.name

    def __repr__(self) -> str:
        return f'{self.__class__.__name__}[{self.name}]'


class NativeCodeBlock(Token):
    """native code fragment"""

    def __init__(self, location: Location, bf_code: str) -> None:
        super().__init__(location)
        self.bf_code = bf_code

    def __str__(self) -> str:
        return self.bf_code

    def __repr__(self) -> str:
        return f'{self.__class__.__name__}[{self.bf_code!r}]'


__all__ = ['is_allowed_in_word', 'is_valid_word', 'AnyToken', 'Token', 'OpenBraceToken', 'CloseBraceToken',
           'OpenBracketToken', 'CloseBracketToken', 'OpenParToken', 'CloseParToken', 'SemicolonToken', 'CommaToken',
           'EqualToken', 'DoublePlusToken', 'DoubleMinusToken', 'BangToken', 'DoubleBangToken', 'DoubleEqualToken',
           'BangEqualToken', 'LessThanToken', 'LessThanEqualToken', 'GreaterThanToken', 'GreaterThanEqualToken',
           'DoubleAmpersandToken', 'DoublePipeToken', 'PlusToken', 'MinusToken', 'StarToken', 'SlashToken',
           'PercentToken', 'DoubleDotToken', 'ColonToken', 'DoubleColonToken', 'ArrowToken', 'HashToken',
           'QuestionMarkToken', 'NamespaceKeyword', 'NativeKeyword', 'TildeToken', 'ProcKeyword', 'FuncKeyword',
           'LetKeyword', 'LoopKeyword', 'ForKeyword', 'WhileKeyword', 'DoKeyword', 'IfKeyword', 'ElseKeyword',
           'ReturnKeyword', 'EOFToken', 'NumberLiteral', 'NumericLiteral', 'CharacterLiteral', 'StringLiteral',
           'IdentifierToken', 'NativeCodeBlock']
