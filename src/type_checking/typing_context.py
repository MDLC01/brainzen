from typing import Optional, TYPE_CHECKING

from data_types import DataType
from exceptions import *
from intermediate_representation import SubroutineArgument
from reference import Reference


if TYPE_CHECKING:
    from type_checked_instructions import TypedExpression


class SubroutineSignature:
    __slots__ = 'location', 'is_private', 'arguments', 'arguments_by_name', 'return_type'

    def __init__(self, location: Location, is_private: bool, arguments: list[SubroutineArgument],
                 return_type: DataType | None = None) -> None:
        self.location = location
        self.is_private = is_private
        self.arguments = arguments
        self.arguments_by_name = {argument.identifier: argument.type for argument in self.arguments}
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


class NamespaceTypingContext:
    __slots__ = 'identifier', 'parent', 'namespaces', 'constants', 'subroutines'

    def __init__(self, identifier: str, parent: Optional['NamespaceTypingContext'] = None) -> None:
        self.identifier = identifier
        self.parent = parent
        self.namespaces: dict[str, 'NamespaceTypingContext'] = {}
        self.constants: dict[str, TypedExpression] = {}
        self.subroutines: dict[str, SubroutineSignature] = {}

    def get_namespace(self, reference: Reference | None) -> 'NamespaceTypingContext':
        if reference is None:
            return self
        parent = self.get_namespace(reference.namespace)
        if reference.identifier in parent.namespaces:
            return parent.namespaces[reference.identifier]
        if self.parent is not None:
            return self.parent.get_namespace(reference)
        raise CompilationException(reference.location, f'Unknown namespace: {reference}')

    def get_constant_value(self, reference: Reference) -> 'TypedExpression':
        namespace = self.get_namespace(reference.namespace)
        if reference.identifier in namespace.constants:
            return namespace.constants[reference.identifier]
        if self.parent is not None:
            return self.parent.get_constant_value(reference)
        raise CompilationException(reference.location, f'Unknown constant: #{reference}')

    def get_subroutine_signature(self, reference: Reference) -> SubroutineSignature:
        namespace = self.get_namespace(reference.namespace)
        if reference.identifier in namespace.subroutines:
            return namespace.subroutines[reference.identifier]
        if self.parent is not None:
            return self.parent.get_subroutine_signature(reference)
        raise CompilationException(reference.location, f'Unknown subroutine: {reference}')

    def register_constant(self, identifier: str, expression: 'TypedExpression') -> None:
        """Register a constant value."""
        self.constants[identifier] = expression

    def namespace(self, identifier: str) -> 'NamespaceTypingContext':
        """Create a context for a child namespace."""
        return NamespaceTypingContext(identifier, self)

    def register_namespace(self, identifier: str, namespace: 'NamespaceTypingContext') -> None:
        """Register a child namespace."""
        self.namespaces[identifier] = namespace

    def subroutine(self, identifier: str, signature: SubroutineSignature) -> 'SubroutineTypingContext':
        """Create a context for a subroutine."""
        return SubroutineTypingContext(self, identifier, signature)

    def register_subroutine(self, identifier: str, signature: SubroutineSignature) -> None:
        """Register a subroutine."""
        self.subroutines[identifier] = signature

    def __enter__(self) -> 'NamespaceTypingContext':
        return self

    def __exit__(self, exc_type, exc_val, exc_tb) -> None:
        self.parent.register_namespace(self.identifier, self)


class VariableInfo:
    __slots__ = 'location', 'type'

    def __init__(self, location: Location, variable_type: DataType) -> None:
        self.location = location
        self.type = variable_type


class CodeBlockTypingContext:
    __slots__ = 'namespace', 'return_type', 'parent', 'variables'

    def __init__(self, namespace: NamespaceTypingContext, parent: Optional['CodeBlockTypingContext'] = None, *,
                 return_type: DataType | None = None) -> None:
        """Private constructor"""
        self.namespace = namespace
        self.return_type = return_type
        self.parent = parent
        self.variables: dict[str, VariableInfo] = {}

    def __contains__(self, identifier: str) -> bool:
        if identifier in self.variables:
            return True
        if self.parent is not None:
            return identifier in self.parent
        return False

    def __getitem__(self, identifier: str) -> VariableInfo:
        if identifier in self.variables:
            return self.variables[identifier]
        if self.parent is not None:
            return self.parent[identifier]
        raise CompilerException(f'Unknown identifier: {identifier!r}')

    def register_variable(self, location: Location, identifier: str, variable_type: DataType) -> None:
        if identifier in self.variables:
            original = self.variables[identifier]
            message = f'Variable {identifier!r} has already been defined at {original.location!r}'
            CompilationWarning.add(location, message, WarningType.REDECLARATION)
        elif self.is_shadow(identifier):
            original = self.parent[identifier]
            message = f'Declaration shadows identifier {identifier!r} from outer scope (declared at' \
                      f' {original.location!r})'
            CompilationWarning.add(location, message, WarningType.NAME_SHADOWING)
        self.variables[identifier] = VariableInfo(location, variable_type)

    def get_variable_type(self, location: Location, identifier: str) -> DataType:
        if identifier in self:
            return self[identifier].type
        raise CompilationException(location, f'Variable {identifier!r} does not exist')

    def is_shadow(self, identifier: str) -> bool:
        """Test if the passed identifier shadows a variable from an outer scope."""
        if self.parent is None:
            return False
        return identifier in self.parent

    def subscope(self, *, allow_return: bool = False) -> 'CodeBlockTypingContext':
        return_type = self.return_type if allow_return else None
        return CodeBlockTypingContext(self.namespace, self, return_type=return_type)


class SubroutineTypingContext(CodeBlockTypingContext):
    __slots__ = 'identifier', 'signature'

    def __init__(self, namespace: NamespaceTypingContext, identifier: str, signature: SubroutineSignature) -> None:
        super().__init__(namespace, return_type=signature.return_type)
        self.identifier = identifier
        self.signature = signature
        for argument in self.signature.arguments:
            self.variables[argument.identifier] = VariableInfo(argument.location, argument.type)

    def __enter__(self) -> 'SubroutineTypingContext':
        return self

    def __exit__(self, exc_type, exc_val, exc_tb) -> None:
        self.namespace.register_subroutine(self.identifier, self.signature)


__all__ = ['SubroutineArgument', 'SubroutineSignature', 'NamespaceTypingContext', 'CodeBlockTypingContext']
