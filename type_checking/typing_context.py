from typing import Optional

from data_types import DataType
from exceptions import CompilationException, CompilationWarning, CompilerException, Location
from intermediate_representation import SubroutineArgument


class SubroutineSignature:
    __slots__ = ['arguments', 'return_type']

    def __init__(self, arguments: list[SubroutineArgument], return_type: DataType) -> None:
        self.arguments: list[SubroutineArgument] = arguments
        self.return_type: DataType = return_type

    def get_argument_types(self) -> list[DataType]:
        return [argument.type for argument in self.arguments]


class VariableScope:
    def __init__(self, parent: Optional['VariableScope'] = None) -> None:
        self.parent: VariableScope | None = parent
        self.variables: dict[str, DataType] = {}

    def __getitem__(self, identifier: str) -> DataType:
        if identifier in self.variables:
            return self.variables[identifier]
        if self.parent is not None:
            return self.parent[identifier]
        raise CompilerException(f'Variable {identifier!r} is not defined')

    def get_variable_type(self, location: Location, identifier: str) -> DataType:
        try:
            return self[identifier]
        except CompilerException as e:
            raise CompilationException(location, e.message)

    def __setitem__(self, identifier: str, variable_type: DataType):
        self.variables[identifier] = variable_type

    def make_child(self) -> 'VariableScope':
        """Create a child scope that inherit variables from this scope."""
        return self.__class__(self)

    def is_shadow(self, identifier: str) -> bool:
        """Test if the passed identifier shadows a variable from an outer scope."""
        if self.parent is None:
            return False
        return identifier in self.parent

    def __contains__(self, identifier: str):
        if identifier in self.variables:
            return True
        if self.parent is not None:
            return identifier in self.parent
        return False


class TypingContext:
    def __init__(self) -> None:
        self.subroutines: dict[str, SubroutineSignature] = {}
        self.variables: VariableScope = VariableScope()
        self.current_subroutine_identifier: str | None = None
        self.current_subroutine_signature: SubroutineSignature | None = None

    def start_subroutine_definition(self, identifier: str, signature: SubroutineSignature) -> None:
        if self.current_subroutine_identifier is not None:
            raise CompilerException('Can not start subroutine definition because the previous one has not ended')
        self.current_subroutine_identifier = identifier
        self.current_subroutine_signature = signature

    def end_subroutine_definition(self) -> None:
        if self.current_subroutine_identifier is None:
            raise CompilerException('Can not end subroutine definition because it was never started')
        self.subroutines[self.current_subroutine_identifier] = self.current_subroutine_signature
        self.current_subroutine_identifier = None
        self.current_subroutine_signature = None
        self.variables = VariableScope()

    def get_variable_type(self, location: Location, identifier: str) -> DataType:
        if identifier in self.variables:
            return self.variables[identifier]
        raise CompilationException(location, f'Variable {identifier!r} is not defined')

    def open_scope(self) -> None:
        self.variables = self.variables.make_child()

    def close_scope(self) -> None:
        self.variables = self.variables.parent

    def get_subroutine_argument_types(self, location: Location, identifier: str) -> list[DataType]:
        if identifier in self.subroutines:
            return self.subroutines[identifier].get_argument_types()
        raise CompilationException(location, f'Subroutine {identifier!r} is not defined')

    def get_function_return_type(self, location: Location, identifier: str) -> DataType:
        if identifier in self.subroutines:
            return_type = self.subroutines[identifier].return_type
            if return_type.is_void():
                raise CompilationException(location, f'{identifier!r} is not a function (marked as void)')
            return return_type

        raise CompilationException(location, f'Subroutine {identifier!r} is not defined')

    def add_variable(self, location: Location, identifier: str, variable_type: DataType) -> None:
        if self.variables.is_shadow(identifier):
            CompilationWarning.add(location, f'Declaration of {identifier!r} shadows variable from outer scope')
        elif identifier in self.variables:
            original_type = self.variables[identifier]
            if original_type == variable_type:
                CompilationWarning.add(location, f'Redeclaration of variable {identifier!r} (type unchanged)')
            else:
                CompilationWarning.add(location, f'Redeclaration of variable {identifier!r} (type changed from'
                                                 f' {original_type} to {variable_type})')
        self.variables[identifier] = variable_type


__all__ = ['SubroutineArgument', 'SubroutineSignature', 'TypingContext']
