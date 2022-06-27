from exceptions import *
from tokenization.tokens import *


class Tokenizer:
    def __init__(self, file_name: str, code: str):
        self.file_name = file_name
        self.code = code
        self.index = 0
        self.location = Location(self.file_name)

    def _has_next(self, offset: int = 0) -> bool:
        """Test if there is a character next or if the end of the file has been reached"""
        return self.index + offset < len(self.code)

    def _peek(self, count: int = 1) -> str:
        """Return the next character without advancing the cursor"""
        if not self._has_next(count - 1):
            return ''
        return self.code[self.index: self.index + count]

    def _next(self, count: int = 1) -> str:
        """Return the next `count` characters and advance the cursor by that amount"""
        string = self._peek(count)
        self.index += count
        for char in string:
            if char == '\n':
                self.location = self.location.next_line()
            else:
                self.location = self.location.next_column()
        return string

    def _eat(self, string: str) -> bool:
        """Advance the cursor if the string starting at the current position starts with the passed one"""
        if self._peek(len(string)) == string:
            self._next(len(string))
            return True
        return False

    def _skip_line(self):
        """Skip the current line"""
        while self._has_next() and self._peek() != '\n':
            self._next()

    def _skip_whitespace(self):
        """Skip until the next non-whitespace character"""
        while self._has_next() and self._peek().isspace():
            self._next()

    def _advance(self):
        """Skip whitespaces and comments"""
        inline_comment_depth = 0
        inline_comment_start = self.location
        while self._has_next():
            self._skip_whitespace()
            if self._eat('/*'):
                if inline_comment_depth == 0:
                    inline_comment_start = self.location
                inline_comment_depth += 1
            elif inline_comment_depth > 0:
                if self._eat('*/'):
                    inline_comment_depth -= 1
                else:
                    self._next()
            elif self._eat('//'):
                self._skip_line()
            else:
                break
        if inline_comment_depth > 0:
            raise CompilationException(inline_comment_start, 'Unclosed inline comment')

    def _read_word(self) -> str:
        """Read the word starting at the current position"""
        word = ''
        while is_allowed_in_word(self._peek()):
            word += self._next()
        if len(word) == 0:
            raise CompilationException(self.location, f'Unexpected {self._peek()!r}')
        return word

    def _parse_escape_sequence(self) -> str:
        location = self.location + (0, -1)
        char = self._next()
        if char == 'n':
            return '\n'
        if char == 'r':
            return '\r'
        if char == 't':
            return '\t'
        if char == 'b':
            return '\b'
        if char == 'f':
            return '\f'
        if char == '\\':
            return '\\'
        if char == '"':
            return '"'
        if char == "'":
            return "'"
        if char == 'x':
            return chr(int(self._next(2), base=16))
        raise CompilationException(location.with_length(2), rf"Invalid escape sequence: '\{char}'")

    def _parse_character_literal(self) -> CharacterLiteral:
        """Read the character literal starting at the current position"""
        location = self.location
        start_index = self.index
        if self._next() != "'":
            raise CompilationException(location, 'Character literal should start with single quote')

        char = self._next()
        if char == '\n':
            raise CompilationException(location, 'Unterminated character literal')
        if char == "'":
            raise CompilationException(location.with_length(2), 'Empty character literal')
        if char == '\\':
            char = self._parse_escape_sequence()

        end = self._next()
        token_location = location.with_length(self.index - start_index)
        if end != "'":
            message = 'Character literal must contain a single character (expected single quote)'
            raise CompilationException(token_location, message)
        return CharacterLiteral(token_location, char)

    def _parse_string_literal(self) -> StringLiteral:
        """Read the string literal starting at the current position"""
        location = self.location
        start_index = self.index
        if self._next() != '"':
            raise CompilationException(location, 'String literal should start with double quote')

        string = ''
        escape = False
        while True:
            if self._peek() == '\n':
                message = 'Unterminated string literal'
                raise CompilationException(location.with_length(self.index - start_index), message)
            elif escape:
                string += self._parse_escape_sequence()
                escape = False
            elif self._eat('\\'):
                escape = True
            elif self._eat('"'):
                break
            else:
                string += self._next()

        return StringLiteral(location.with_length(self.index - start_index), string)

    def _parse_native_code(self) -> NativeCodeBlock:
        """Read the native code block starting at the current position"""
        location = self.location
        if self._next() != '`':
            raise CompilationException(location, 'Native code should be surrounded with backticks')
        close_quote = '```' if self._eat('``') else '`'
        bf_code = ''
        while not self._eat(close_quote):
            bf_code += self._next()
        return NativeCodeBlock(location, bf_code)

    def parse_next_token(self) -> Token:
        location = self.location

        for n in range(3, 0, -1):
            if Token.is_valid_token(self._peek(n)):
                return Token.of(self._next(n), location.with_length(n))

        # Character literal
        if self._peek() == "'":
            return self._parse_character_literal()

        # String literal
        if self._peek() == '"':
            return self._parse_string_literal()

        # Native code block
        if self._peek() == '`':
            return self._parse_native_code()

        word = self._read_word()
        word_location = location.with_length(len(word))

        # Keyword
        if Token.is_valid_keyword(word):
            return Token.of(word, word_location)

        # Number literal
        if word[0] in '0123456789':
            return NumericLiteral.parse(word_location, word)

        # Identifier
        return IdentifierToken(word_location, word)

    def tokenize(self) -> list[Token]:
        tokens = []
        self._advance()
        while self._has_next():
            tokens.append(self.parse_next_token())
            self._advance()
        tokens.append(EOFToken(self.location))
        return tokens


__all__ = ['Tokenizer']
