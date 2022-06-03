from abc import ABC, abstractmethod

from data_types import DataType, Types
from exceptions import CompilerException
from intermediate_representation import SubroutineArgument


_counter = 0


def generate_unique_identifier() -> str:
    global _counter
    _counter += 1
    return f'$var{_counter}'


class Memory:
    def __init__(self, origin: int):
        self.memory: dict[int, int] = {}
        self._size = 0
        self.origin = origin

    def _find_available_block(self, size: int) -> int:
        i = self.origin
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

    def _define_block(self, index: int, size: int) -> None:
        self._size = max(self._size, index + size)
        self.memory[index] = size

    def alloc(self, size: int) -> int:
        index = self._find_available_block(size)
        self._define_block(index, size)
        return index

    def free(self, index: int) -> None:
        if self.memory[index] > 0:
            del self.memory[index]

    def size(self) -> int:
        return self._size


class Name:
    __slots__ = 'name_manager', 'identifier', 'type', 'index', 'shadows', 'is_opened'

    def __init__(self, name_manager: 'NameManager', identifier: str, variable_type: DataType,
                 index: int) -> None:
        self.name_manager = name_manager
        self.identifier = identifier
        self.type = variable_type
        self.index = index
        self.shadows: Variable | None = None
        self.is_opened = False

    def __repr__(self) -> str:
        return f'{self.type} {self.identifier} at {self.index}'

    def __enter__(self) -> 'Name':  # TODO: Replace with Self when updating to Python 3.11
        if self.is_opened:
            raise CompilerException(f'Name {self} is already opened')
        self.is_opened = True
        self.shadows = self.name_manager.names.get(self.identifier)
        self.name_manager.names[self.identifier] = self
        return self

    def __exit__(self, exc_type, exc_val, exc_tb) -> None:
        if not self.is_opened:
            raise CompilerException(f'Name {self} is not opened')
        self.is_opened = False
        if self.shadows is not None:
            self.name_manager.names[self.identifier] = self.shadows
        else:
            del self.name_manager.names[self.identifier]

    def size(self) -> int:
        return self.type.size()


class Variable(Name):
    __slots__ = ()

    def __exit__(self, exc_type, exc_val, exc_tb) -> None:
        super().__exit__(exc_type, exc_val, exc_tb)
        self.name_manager.clear_variable_memory(self)


class Pointer(Name):
    __slots__ = ()

    def update_index(self, index: int) -> None:
        self.index = index


class Scope:
    def __init__(self, name_manager: 'NameManager') -> None:
        self.name_manager = name_manager
        self.names: set[Name] = set()

    def __enter__(self):
        self.name_manager.scopes.append(self)
        return self

    def __exit__(self, exc_type, exc_val, exc_tb) -> None:
        for variable in self.names:
            variable.__exit__(exc_type, exc_val, exc_tb)
        if self.name_manager.scopes.pop() is not self:
            raise CompilerException('Invalid scope closing order')

    def add_name(self, name: Name) -> None:
        self.names.add(name)


class NameManager(ABC):
    def __init__(self, arguments: list[SubroutineArgument], return_type: DataType | None = None) -> None:
        argument_variables = []
        argument_buffer_size = 0
        for argument in arguments:
            argument_variables.append(Variable(self, argument.identifier, argument.type, argument_buffer_size))
            argument_buffer_size += argument.type.size()
        self.memory = Memory(argument_buffer_size)
        self.names: dict[str, Name] = {}
        self.scopes: list[Scope] = []
        for argument_variable in argument_variables:
            argument_variable.__enter__()
        self.tmp = self.variable(identifier='$tmp').__enter__()
        self.return_index = 0
        if return_type is not None:
            self.return_index = self.variable(return_type, '$return').__enter__().index

    def memory_size(self) -> int:
        return self.memory.size()

    def __contains__(self, identifier: str) -> bool:
        return identifier in self.names

    def __getitem__(self, identifier: str) -> Name:
        if identifier in self.names:
            return self.names[identifier]
        raise CompilerException(f'Unknown identifier: {identifier!r}')

    @abstractmethod
    def clear_variable_memory(self, variable: Variable) -> None:
        """This method is called when a variable is killed and is used to clear the corresponding cells in memory."""

    def type_of(self, identifier: str) -> DataType:
        return self[identifier].type

    def size_of(self, identifier: str) -> int:
        return self[identifier].size()

    def index_of(self, identifier: str) -> int:
        return self[identifier].index

    def variable(self, variable_type: DataType = Types.CHAR, identifier: str | None = None) -> Variable:
        if identifier is None:
            identifier = generate_unique_identifier()
        index = self.memory.alloc(variable_type.size())
        return Variable(self, identifier, variable_type, index)

    def pointer(self, index: int, data_type: DataType = Types.CHAR, identifier: str | None = None) -> Pointer:
        if identifier is None:
            identifier = generate_unique_identifier()
        return Pointer(self, identifier, data_type, index)

    def scope(self) -> Scope:
        return Scope(self)

    def active_scope(self) -> Scope:
        if len(self.scopes) < 1:
            raise CompilerException('No active scope')
        return self.scopes[-1]

    def scoped_variable(self, identifier: str, variable_type: DataType) -> Variable:
        variable = self.variable(variable_type, identifier).__enter__()
        self.active_scope().add_name(variable)
        return variable


__all__ = ['generate_unique_identifier', 'Variable', 'Name', 'NameManager']
