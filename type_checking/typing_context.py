from typing import Optional

from data_types import DataType
from exceptions import *
from intermediate_representation import SubroutineArgument


class SubroutineSignature:
    __slots__ = 'arguments', 'return_type'

    def __init__(self, arguments: list[SubroutineArgument], return_type: DataType | None = None) -> None:
        self.arguments = arguments
        self.return_type = return_type

    def get_argument_types(self) -> list[DataType]:
        return [argument.type for argument in self.arguments]

    def is_function(self) -> bool:
        return self.return_type is not None

    def __repr__(self) -> str:
        arguments = ', '.join(str(argument) for argument in self.arguments)
        if self.is_function():
            return f'({arguments}) -> {self.return_type}'
        return f'({arguments}) -> void'


class SubroutineTypingContext:
    class Scope:
        def __init__(self, parent: Optional['SubroutineTypingContext.Scope'] = None) -> None:
            self.parent: SubroutineTypingContext.Scope | None = parent
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

        def make_child(self) -> 'SubroutineTypingContext.Scope':
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

    def __init__(self, namespace: 'TypeCheckedNamespace', signature: SubroutineSignature) -> None:
        self.namespace = namespace
        self.signature = signature
        self.variables = self.Scope()

    def expected_return_type(self) -> DataType | None:
        return self.signature.return_type

    def get_subroutine_argument_types(self, location: Location, identifier: str) -> list[DataType]:
        if identifier in self.namespace.subroutine_signatures:
            return self.namespace.subroutine_signatures[identifier].get_argument_types()
        raise CompilationException(location, f'Subroutine {identifier!r} is not defined')

    def get_function_return_type(self, location: Location, identifier: str) -> DataType:
        if identifier in self.namespace.subroutine_signatures:
            subroutine = self.namespace.subroutine_signatures[identifier]
            if not subroutine.is_function():
                raise CompilationException(location, f'Subroutine {identifier!r} is not a function')
            return subroutine.return_type
        raise CompilationException(location, f'Function {identifier!r} does not exist')

    def get_variable_type(self, location: Location, identifier: str) -> DataType:
        if identifier in self.variables:
            return self.variables[identifier]
        raise CompilationException(location, f'Variable {identifier!r} is not defined')

    def add_variable(self, location: Location, identifier: str, variable_type: DataType) -> None:
        if self.variables.is_shadow(identifier):
            CompilationWarning.add(location, f'Declaration of {identifier!r} shadows variable from outer scope')
        elif identifier in self.variables:
            CompilationWarning.add(location, f'Redeclaration of variable {identifier!r}')
        self.variables[identifier] = variable_type

    def open_scope(self) -> None:
        self.variables = self.variables.make_child()

    def close_scope(self) -> None:
        self.variables = self.variables.parent

    def __repr__(self) -> str:
        return repr(self.__dict__)


__all__ = ['SubroutineArgument', 'SubroutineSignature', 'SubroutineTypingContext']
