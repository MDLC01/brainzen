from data_types import DataType, Types
from exceptions import CompilerException


_counter = 0


def generate_unique_identifier() -> str:
    global _counter
    _counter += 1
    return f'$var{_counter}'


class IdentifierInfo:
    __slots__ = ['type', 'index', 'identifier', 'is_pointer']

    def __init__(self, variable_type: DataType, index: int, identifier: str, is_pointer: bool = False) -> None:
        self.type = variable_type
        self.index = index
        self.identifier = identifier
        self.is_pointer = is_pointer

    def size(self) -> int:
        return self.type.size()

    def __str__(self) -> str:
        type_string = f'{self.type}'
        if self.is_pointer:
            type_string = f'pointer<{type_string}>'
        return f'{type_string} at {self.index}'

    def __repr__(self) -> str:
        # This is not good practice, but the only place where this is called is implicitly
        # in SubroutineCompiler.create_context_snapshot. The problem is that dicts do not
        # define a __str__, so when str(scope) is called, it uses the repr of whatever is
        # inside the dict (i.e., an instance of this class). It's simpler to alias __repr_
        # to __str__ rather than creating a custom method for converting dicts to strings
        # using the __str__ instead of __repr__ everywhere.
        # TL;DR: I know __repr__ should not be used as an alias of __str__, but in this
        # case it's the least bad option.
        return self.__str__()


class Memory:
    def __init__(self):
        self.memory: dict[int, int] = {}
        self._size = 0

    def _find_available_block(self, size: int) -> int:
        i = 0
        current_block_size = 0
        while True:
            if i in self.memory and self.memory[i] > 0:
                i += self.memory[i]
                current_block_size = 0
            else:
                current_block_size += 1
                i += 1
            if current_block_size == size:
                return i - current_block_size

    def define_block(self, index: int, size: int) -> None:
        """
        This method is exposed because it is used to define subroutine arguments,
        but it should not be used in another context.
        This is a temporary solution that aims to be replaced in the long run.
        TODO: Find a better solution to declare subroutine arguments.
        """
        self._size = max(self._size, index + size)
        self.memory[index] = size

    def alloc(self, size: int) -> int:
        index = self._find_available_block(size)
        self.define_block(index, size)
        return index

    def free(self, index: int) -> None:
        if self.memory[index] > 0:
            del self.memory[index]

    def size(self) -> int:
        return self._size


class ScopeManager:
    """A scope manager is capable of managing a scope and its children."""

    Scope = dict[str, IdentifierInfo]

    def __init__(self) -> None:
        self.memory = Memory()
        self.scopes: list[dict[str, IdentifierInfo]] = [{}]
        self._argument_buffer_size = 0

    def __contains__(self, identifier: str | IdentifierInfo) -> bool:
        return any(identifier in scope for scope in self.scopes)

    def _get_info(self, identifier: str) -> IdentifierInfo:
        for scope in reversed(self.scopes):
            if identifier in scope:
                return scope[identifier]
        raise CompilerException(f'Unknown identifier: {identifier!r}')

    def __getitem__(self, identifier: str) -> int:
        return self._get_info(identifier).index

    def __delitem__(self, identifier: str) -> None:
        if identifier not in self.scopes[-1]:
            raise CompilerException(f'Can not delete identifier {identifier!r} from outer scope')
        variable = self._get_info(identifier)
        if not variable.is_pointer:
            self.memory.free(self.index_of(identifier))
        del self.scopes[-1][identifier]

    def type_of(self, identifier: str) -> DataType:
        return self._get_info(identifier).type

    def size_of(self, identifier: str) -> int:
        return self._get_info(identifier).type.size()

    def index_of(self, identifier: str) -> int:
        return self._get_info(identifier).index

    def register_argument(self, identifier: str, argument_type: DataType) -> None:
        if len(self.scopes) > 1:
            raise CompilerException('Can not register subroutine argument in child scope')
        index = self._argument_buffer_size
        size = argument_type.size()
        self.memory.define_block(index, size)
        self._argument_buffer_size += size
        self.scopes[0][identifier] = IdentifierInfo(argument_type, index, identifier)

    def register_identifier(self, identifier: str, variable_type: DataType) -> None:
        index = self.memory.alloc(variable_type.size())
        self.scopes[-1][identifier] = IdentifierInfo(variable_type, index, identifier, False)

    def register_pointer(self, identifier: str, data_type: DataType, index: int) -> None:
        self.scopes[-1][identifier] = IdentifierInfo(data_type, index, identifier, True)

    def require_memory(self, variable_type: DataType = Types.CHAR, count: int = 1) -> tuple[str, ...]:
        identifiers = tuple(generate_unique_identifier() for _ in range(count))
        for identifier in identifiers:
            self.register_identifier(identifier, variable_type)
        return identifiers

    def update_pointer(self, identifier: str, index: int) -> None:
        variable = self._get_info(identifier)
        if not variable.is_pointer:
            raise CompilerException(f'{identifier!r} is not a pointer')
        variable.index = index

    def memory_size(self) -> int:
        return self.memory.size()

    def __enter__(self) -> 'ScopeManager':
        self.scopes.append({})
        return self

    def __exit__(self, exc_type, exc_val, exc_tb) -> None:
        for variable in list(self.scopes[-1]):
            del self[variable]
        self.scopes.pop()


__all__ = ['generate_unique_identifier', 'ScopeManager']
