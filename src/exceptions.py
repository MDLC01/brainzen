import sys
from enum import Enum
from typing import Self


class Location:
    __slots__ = 'file', 'line', 'column', 'length'

    @classmethod
    def unknown(cls) -> Self:
        return cls(None, None, None)

    @classmethod
    def stdlib(cls) -> Self:
        return cls('<stdlib>', None, None)

    @classmethod
    def primitive(cls) -> Self:
        return cls('<primitive>', None, None)

    @classmethod
    def in_file(cls, file: str) -> Self:
        return cls(file, None, None)

    def __init__(self, file: str | None, line: int | None = 1, column: int | None = 1, length: int = 1) -> None:
        self.file = file
        self.line = line
        self.column = column
        self.length = length

    def __str__(self) -> str:
        if self.file is None:
            return ''
        return f'At {self!r}: '

    def __repr__(self) -> str:
        location = self.file if self.file is not None else '<unknown>'
        if self.line is not None:
            location += f':{self.line}'
            if self.column is not None:
                location += f':{self.column}'
        return location

    def __add__(self, offset: tuple[int, int]) -> Self:
        return Location(self.file, self.line + offset[0], self.column + offset[1])

    def next_line(self) -> Self:
        return Location(self.file, self.line + 1)

    def next_column(self) -> Self:
        return Location(self.file, self.line, self.column + 1)

    def with_length(self, length: int, *, offset: int = 0) -> Self:
        return Location(self.file, self.line, self.column + offset, length)

    def extend(self, amount: int) -> Self:
        return Location(self.file, self.line, self.column, self.length + amount)

    def extend_to(self, location: 'Location') -> Self:
        if self.file != location.file:
            raise CompilerException('Cannot extend location to multiple files')
        end = location.column + location.length
        return Location(self.file, self.line, self.column, end - self.column)

    def after(self) -> Self:
        return Location(self.file, self.line, self.column + self.length)

    def print_position(self, source_code: str, *, out=sys.stderr) -> None:
        if self.line is None or self.column is None:
            return
        lines = source_code.splitlines()
        if self.line < 0 or self.line - 1 >= len(lines):
            return
        line = lines[self.line - 1]
        stripped_line = line.lstrip()
        offset = len(line) - len(stripped_line)
        line_number = str(self.line)
        print(line_number, '|', stripped_line, file=out)
        print(' ' * len(line_number), '|', ' ' * (self.column - offset - 1) + '^' * self.length, file=out)


class CompilerException(Exception):
    """An exception that is raised by the compiler when something went wrong in the compilation process."""

    def __init__(self, message: str, should_be_prevented: bool = False) -> None:
        super().__init__()
        self.message = message
        if should_be_prevented:
            self.message += '. This exception should have been prevented.'

    def __str__(self) -> str:
        return self.message


class ImpossibleException(CompilerException):
    """An exception that is not meant to be raised."""

    def __init__(self, message: str) -> None:
        super().__init__(message)


class CompilationException(Exception):
    """An exception that is thrown when an error happens while compiling a Brainzen program."""

    __slots__ = 'location', 'message', 'hint'

    def __init__(self, location: Location, message: str, hint: str | None = None) -> None:
        super().__init__()
        self.location = location
        self.message = message
        self.hint = hint

    def __str__(self) -> str:
        return f'{self.location}{self.message}'


class WarningType(Enum):
    OUT_OF_RANGE = 'out-of-range'
    REDECLARATION = 'redeclaration'
    NAME_SHADOWING = 'shadowing'
    RESERVED_NAME = 'reserved-name'
    IGNORED_RESULT = 'ignored-result'
    NATIVE_CODE = 'native-code'
    NAMING_CONVENTIONS = 'naming'
    DEBUG_FEATURE = 'debug-feature'

    @classmethod
    def all(cls) -> set[Self]:
        return {value for value in cls}

    @classmethod
    def likely_errors(cls) -> set[Self]:
        return {cls.OUT_OF_RANGE, cls.RESERVED_NAME}

    @classmethod
    def default(cls) -> set[Self]:
        return {cls.OUT_OF_RANGE, cls.REDECLARATION, cls.NAME_SHADOWING, cls.RESERVED_NAME, cls.DEBUG_FEATURE}

    @classmethod
    def debug(cls) -> set[Self]:
        return {cls.OUT_OF_RANGE, cls.REDECLARATION, cls.NAME_SHADOWING, cls.RESERVED_NAME, cls.IGNORED_RESULT}

    @classmethod
    def none(cls) -> set[Self]:
        return set()

    @classmethod
    def get(cls, name: str) -> Self:
        for warning_type in cls:
            if warning_type.value == name:
                return warning_type
        raise ValueError(f'Unknown warning type: {name!r}')

    @classmethod
    def get_selection_or_singleton(cls, name: str) -> set[Self]:
        if name == 'likely-errors':
            return cls.likely_errors()
        if name == 'default':
            return cls.default()
        if name == 'debug':
            return cls.debug()
        return {cls.get(name)}

    @classmethod
    def selection_from_string(cls, allowed_warnings: str) -> set[Self]:
        if allowed_warnings == '*':
            return cls.all()
        if allowed_warnings in ('none', '-', ''):
            return cls.none()
        selection = set()
        for name in allowed_warnings.split(','):
            selection = selection.union(cls.get_selection_or_singleton(name.strip()))
        return selection


class CompilationWarning(Warning):
    __slots__ = 'location', 'type', 'message', 'hint'

    warnings: list['CompilationWarning'] = []

    def __init__(self, location: Location, warning_type: WarningType, message: str, hint: str | None = None) -> None:
        self.location = location
        self.type = warning_type
        self.message = message
        self.hint = hint

    def __str__(self) -> str:
        return f'{self.location}Warning: {self.message}'

    @classmethod
    def add(cls, location: Location, warning_type: WarningType, message: str, hint: str | None = None) -> None:
        cls.warnings.append(cls(location, warning_type, message, hint))

    @classmethod
    def suggest_pascal_case(cls, location: Location, identifier: str, message: str | None = None) -> None:
        if identifier[0].isupper() and '_' not in identifier:
            return
        if message is None:
            message = 'Name should use PascalCase'
        suggested_name = ''
        for i, c in enumerate(identifier):
            if c == '_':
                pass
            elif i == 0 or identifier[i - 1] == '_':
                suggested_name += c.upper()
            else:
                suggested_name += c
        cls.add(location, WarningType.NAMING_CONVENTIONS, message, f'Try renaming to {suggested_name!r}')

    @classmethod
    def suggest_screaming_snake_case(cls, location: Location, identifier: str, message: str | None = None) -> None:
        if identifier.isupper():
            return
        if message is None:
            message = 'Name should use SCREAMING_SNAKE_CASE'
        suggested_name = ''
        for i, c in enumerate(identifier):
            if i != 0 and identifier[i - 1] != '_' and c.isupper():
                suggested_name += f'_{c}'
            else:
                suggested_name += c.upper()
        cls.add(location, WarningType.NAMING_CONVENTIONS, message, f'Try renaming to {suggested_name!r}')

    @classmethod
    def print_warnings(cls, source_code: str, allowed_types: set[WarningType], *, out=sys.stderr) -> None:
        first = True
        for warning in cls.warnings:
            if warning.type in allowed_types:
                if first:
                    first = False
                else:
                    print(file=out)
                print(warning, file=out)
                warning.location.print_position(source_code, out=out)
                if warning.hint is not None:
                    print(f'Hint: {warning.hint}', file=out)


__all__ = ['Location', 'CompilerException', 'ImpossibleException', 'CompilationException', 'WarningType',
           'CompilationWarning']
