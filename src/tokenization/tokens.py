from abc import ABC
from enum import IntEnum
from typing import Type, TypeVar

from exceptions import *


def is_allowed_in_word(char: str) -> bool:
    return char in '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_'


def is_valid_word(word: str) -> bool:
    return all(is_allowed_in_word(char) for char in word)


class Priority(IntEnum):
    MINIMAL = 0
    CONJUNCTION = 1
    DISJUNCTION = 2
    COMPARISON = 3
    ADDITION = 4
    MULTIPLICATION = 5
    PARENTHESIS = 254
    MAXIMAL = 255


AnyToken = TypeVar('AnyToken', bound='Token')


class Token(ABC):
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

    def __init__(self, location: Location) -> None:
        self.location = location

    def is_unary_operator(self) -> bool:
        return False

    def binary_operator_priority(self) -> Priority | None:
        return None

    def is_binary_operator(self) -> bool:
        return self.binary_operator_priority() is not None

    def __str__(self) -> str:
        if self.token is not None:
            return self.token
        if self.keyword is not None:
            return self.keyword
        return self.__repr__()

    def __repr__(self) -> str:
        return self.__class__.__name__


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

    def is_unary_operator(self) -> bool:
        return True


class DoubleBangToken(Token):
    token = '!!'

    def is_unary_operator(self) -> bool:
        return True


class DoubleEqualToken(Token):
    token = '=='

    def binary_operator_priority(self) -> Priority | None:
        return Priority.COMPARISON


class BangEqualToken(Token):
    token = '!='

    def binary_operator_priority(self) -> Priority | None:
        return Priority.COMPARISON


class LessThanToken(Token):
    token = '<'

    def binary_operator_priority(self) -> Priority | None:
        return Priority.COMPARISON


class LessThanEqualToken(Token):
    token = '<='

    def binary_operator_priority(self) -> Priority | None:
        return Priority.COMPARISON


class GreaterThanToken(Token):
    token = '>'

    def binary_operator_priority(self) -> Priority | None:
        return Priority.COMPARISON


class GreaterThanEqualToken(Token):
    token = '>='

    def binary_operator_priority(self) -> Priority | None:
        return Priority.COMPARISON


class DoubleAmpersandToken(Token):
    token = '&&'

    def binary_operator_priority(self) -> Priority | None:
        return Priority.CONJUNCTION


class PipeToken(Token):
    token = '|'


class DoublePipeToken(Token):
    token = '||'

    def binary_operator_priority(self) -> Priority | None:
        return Priority.DISJUNCTION


class PlusToken(Token):
    token = '+'

    def binary_operator_priority(self) -> Priority | None:
        return Priority.ADDITION


class MinusToken(Token):
    token = '-'

    def is_unary_operator(self) -> bool:
        return True

    def binary_operator_priority(self) -> Priority | None:
        return Priority.ADDITION


class StarToken(Token):
    token = '*'

    def binary_operator_priority(self) -> Priority | None:
        return Priority.MULTIPLICATION


class DoubleStarToken(Token):
    token = '**'

    def is_unary_operator(self) -> bool:
        return True


class SlashToken(Token):
    token = '/'

    def binary_operator_priority(self) -> Priority | None:
        return Priority.MULTIPLICATION


class PercentToken(Token):
    token = '%'

    def binary_operator_priority(self) -> Priority | None:
        return Priority.MULTIPLICATION


class DoubleDotToken(Token):
    token = '..'


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


class PrivateKeyword(Token):
    keyword = 'private'


class TypeKeyword(Token):
    keyword = 'type'


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

    def __str__(self) -> str:
        return str(self.value)

    def __repr__(self) -> str:
        return f'{self.__class__.__name__}[{self.value}]'


class NumericLiteral(NumberLiteral):
    """numeric literal"""

    @classmethod
    def parse(cls, location: Location, word: str) -> 'NumericLiteral':
        try:
            if word.startswith('0x'):
                return cls(location, int(word[2:], base=16))
            return cls(location, int(word))
        except ValueError:
            raise CompilationException(location, f'Invalid numeric literal: {word!r}')

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


__all__ = ['is_allowed_in_word', 'is_valid_word', 'Priority', 'AnyToken', 'Token', 'OpenBraceToken', 'CloseBraceToken',
           'OpenBracketToken', 'CloseBracketToken', 'OpenParToken', 'CloseParToken', 'SemicolonToken', 'CommaToken',
           'EqualToken', 'DoublePlusToken', 'DoubleMinusToken', 'BangToken', 'DoubleBangToken', 'DoubleEqualToken',
           'BangEqualToken', 'LessThanToken', 'LessThanEqualToken', 'GreaterThanToken', 'GreaterThanEqualToken',
           'DoubleAmpersandToken', 'PipeToken', 'DoublePipeToken', 'PlusToken', 'MinusToken', 'StarToken',
           'DoubleStarToken', 'SlashToken', 'PercentToken', 'DoubleDotToken', 'ColonToken', 'DoubleColonToken',
           'ArrowToken', 'HashToken', 'QuestionMarkToken', 'PrivateKeyword', 'TypeKeyword', 'NamespaceKeyword',
           'NativeKeyword', 'TildeToken', 'ProcKeyword', 'FuncKeyword', 'LetKeyword', 'LoopKeyword', 'ForKeyword',
           'WhileKeyword', 'DoKeyword', 'IfKeyword', 'ElseKeyword', 'ReturnKeyword', 'EOFToken', 'NumberLiteral',
           'NumericLiteral', 'CharacterLiteral', 'StringLiteral', 'IdentifierToken', 'NativeCodeBlock']
