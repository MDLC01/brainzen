import sys


class Location:
    __slots__ = ['file', 'line', 'column', 'length']

    @classmethod
    def unknown(cls) -> 'Location':
        return cls(None, None, None)

    @classmethod
    def stdlib(cls) -> 'Location':
        return cls('<stdlib>', None, None)

    @classmethod
    def primitive(cls) -> 'Location':
        return cls('<primitive>', None, None)

    @classmethod
    def in_file(cls, file: str) -> 'Location':
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

    def __add__(self, offset: tuple[int, int]) -> 'Location':
        return Location(self.file, self.line + offset[0], self.column + offset[1])

    def next_line(self) -> 'Location':
        return Location(self.file, self.line + 1)

    def next_column(self) -> 'Location':
        return Location(self.file, self.line, self.column + 1)

    def with_length(self, length: int, *, offset: int = 0) -> 'Location':
        return Location(self.file, self.line, self.column + offset, length)

    def extend(self, amount: int) -> 'Location':
        return Location(self.file, self.line, self.column, self.length + amount)

    def extend_to(self, location: 'Location') -> 'Location':
        if self.file != location.file:
            raise CompilerException('Can not extend location to multiple files')
        end = location.column + location.length
        return Location(self.file, self.line, self.column, end - self.column)

    def print_position(self, source_code: str, *, out=sys.stderr):
        if self.line is None or self.column is None:
            return
        line = source_code.splitlines()[self.line - 1]
        print(line, file=out)
        print(' ' * (self.column - 1) + '^' * self.length, file=out)


class CompilerException(Exception):
    """An exception that is raised by the compiler when something went wrong in the compilation process."""

    def __init__(self, message: str, should_be_prevented: bool = False):
        super().__init__()
        self.message = message
        if should_be_prevented:
            self.message += '. This exception should have been prevented.'

    def __str__(self):
        return self.message


class ImpossibleException(CompilerException):
    """An exception that is not meant to be raised."""

    def __init__(self, message: str):
        super().__init__(message)


class CompilationException(Exception):
    """An exception that is thrown when an error happens while compiling a Brainzen program."""

    def __init__(self, location: Location, message: str):
        super().__init__()
        self.location = location
        self.message = message

    def __str__(self):
        return f'{self.location}{self.message}'


class CompilationWarning(Warning):
    warnings: list['CompilationWarning'] = []

    def __init__(self, location: Location, message: str):
        self.location = location
        self.message = message

    def __str__(self):
        return f'{self.location}{self.message}'

    @classmethod
    def add(cls, location: Location, message: str):
        cls.warnings.append(cls(location, message))

    @classmethod
    def print_warnings(cls, source_code: str, *, out=sys.stderr):
        for warning in cls.warnings:
            print(warning, file=out)
            warning.location.print_position(source_code, out=out)
            print(file=out)


__all__ = ['Location', 'CompilerException', 'ImpossibleException', 'CompilationException', 'CompilationWarning']
