from typing import Generic, Optional, TYPE_CHECKING, TypeVar

from exceptions import *
from intermediate_representation import *
from reference import Reference
from type_checking.data_types import *


if TYPE_CHECKING:
    from type_checked_instructions import TypedExpression


class TypedSubroutineArgument:
    __slots__ = 'location', 'identifier', 'type'

    @classmethod
    def from_untyped(cls, context: 'NamespaceTypingContext',
                     subroutine_argument: SubroutineArgument) -> 'TypedSubroutineArgument':
        argument_type = context.build_type(subroutine_argument.type)
        return cls(subroutine_argument.location, subroutine_argument.identifier, argument_type)

    def __init__(self, location: Location, identifier: str, argument_type: DataType) -> None:
        self.location = location
        self.identifier = identifier
        self.type = argument_type

    def __repr__(self) -> str:
        return f'{self.type} {self.identifier}'


class SubroutineSignature:
    __slots__ = 'location', 'arguments', 'arguments_by_name', 'return_type'

    def __init__(self, location: Location, arguments: list[TypedSubroutineArgument],
                 return_type: DataType | None = None) -> None:
        self.location = location
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


_T = TypeVar('_T')


class NamespaceElementInfo(Generic[_T]):
    __slots__ = 'identifier', 'is_private', 'element'

    def __init__(self, identifier: str, is_private: bool, element: _T) -> None:
        self.identifier = identifier
        self.is_private = is_private
        self.element = element


class NamespaceTypingContext:
    __slots__ = 'identifier', 'is_private', 'parent', 'namespaces', 'types', 'constants', 'subroutines'

    def __init__(self, identifier: str, is_private: bool, parent: Optional['NamespaceTypingContext'] = None) -> None:
        self.identifier = identifier
        self.is_private = is_private
        self.parent = parent
        self.namespaces = dict[str, NamespaceElementInfo[NamespaceTypingContext]]()
        self.types = dict[str, NamespaceElementInfo[DataType]]()
        self.constants = dict[str, NamespaceElementInfo['TypedExpression']]()
        self.subroutines = dict[str, NamespaceElementInfo[SubroutineSignature]]()

    def get_namespace(self, reference: Reference) -> 'NamespaceTypingContext':
        # Unqualified reference (reference from within the namespace)
        if reference.namespace is None:
            if reference.identifier in self.namespaces:
                return self.namespaces[reference.identifier].element
            if self.parent is not None:
                return self.parent.get_namespace(reference)
            else:
                raise CompilationException(reference.location, f'Unknown namespace: {reference!r}')
        # Fully qualified reference (reference from outside the namespace)
        parent = self.get_namespace(reference.namespace)
        if reference.identifier not in parent.namespaces:
            raise CompilationException(reference.location, f'Unknown namespace: {reference!r}')
        namespace = parent.namespaces[reference.identifier]
        if namespace.is_private:
            raise CompilationException(reference.location, f'Unable to access private namespace {reference!r}')
        return namespace.element

    def get_type(self, reference: Reference) -> DataType:
        # Unqualified reference (reference from within the namespace)
        if reference.namespace is None:
            if reference.identifier in self.types:
                return self.types[reference.identifier].element
            if self.parent is not None:
                return self.parent.get_type(reference)
            else:
                raise CompilationException(reference.location, f'Unknown type: {reference!r}')
        # Fully qualified reference (reference from outside the namespace)
        parent = self.get_namespace(reference.namespace)
        if reference.identifier not in parent.types:
            raise CompilationException(reference.location, f'Unknown type: {reference!r}')
        data_type = parent.types[reference.identifier]
        if data_type.is_private:
            raise CompilationException(reference.location, f'Unable to access private type {reference!r}')
        return data_type.element

    def build_type(self, type_expression: TypeExpression) -> DataType:
        match type_expression:
            case TypeReference(reference=reference):
                return self.get_type(reference)
            case TypeArray(base_type=base_type, count=count_expression):
                from type_checking.type_checked_instructions import evaluate, LiteralChar

                # This means an array cannot be longer than 255 elements, which is sad
                count = evaluate(self, count_expression)
                if not isinstance(count, LiteralChar):
                    raise CompilationException(count.location, f'Expected {Types.CHAR} but found {count.type()}')
                return ArrayType(self.build_type(base_type), count.value)
            case TypeProduct(operands=operands):
                return ProductType([self.build_type(operand) for operand in operands])
        raise ImpossibleException(f'Unknown type expression type: {type_expression.__class__.__name__}')

    def get_constant_value(self, reference: Reference) -> 'TypedExpression':
        # Unqualified reference (reference from within the namespace)
        if reference.namespace is None:
            if reference.identifier in self.constants:
                return self.constants[reference.identifier].element
            if self.parent is not None:
                return self.parent.get_constant_value(reference)
            else:
                raise CompilationException(reference.location, f'Unknown constant: {reference!r}')
        # Fully qualified reference (reference from outside the namespace)
        parent = self.get_namespace(reference.namespace)
        if reference.identifier not in parent.constants:
            raise CompilationException(reference.location, f'Unknown constant: {reference!r}')
        constant = parent.constants[reference.identifier]
        if constant.is_private:
            raise CompilationException(reference.location, f'Unable to access private constant {reference!r}')
        return constant.element

    def get_subroutine_signature(self, reference: Reference) -> SubroutineSignature:
        # Unqualified reference (reference from within the namespace)
        if reference.namespace is None:
            if reference.identifier in self.subroutines:
                return self.subroutines[reference.identifier].element
            if self.parent is not None:
                return self.parent.get_subroutine_signature(reference)
            else:
                raise CompilationException(reference.location, f'Unknown subroutine: {reference!r}')
        # Fully qualified reference (reference from outside the namespace)
        parent = self.get_namespace(reference.namespace)
        if reference.identifier not in parent.subroutines:
            raise CompilationException(reference.location, f'Unknown subroutine: {reference!r}')
        constant = parent.subroutines[reference.identifier]
        if constant.is_private:
            raise CompilationException(reference.location, f'Unable to access private subroutine {reference!r}')
        return constant.element

    def register_type(self, identifier: str, is_private: bool, data_type: DataType) -> None:
        """Register a type."""
        self.types[identifier] = NamespaceElementInfo(identifier, is_private, data_type)

    def register_constant(self, identifier: str, is_private: bool, expression: 'TypedExpression') -> None:
        """Register a constant value."""
        self.constants[identifier] = NamespaceElementInfo(identifier, is_private, expression)

    def namespace(self, identifier: str, is_private: bool) -> 'NamespaceTypingContext':
        """Create a context for a child namespace."""
        return NamespaceTypingContext(identifier, is_private, self)

    def register_namespace(self, identifier: str, is_private: bool, namespace: 'NamespaceTypingContext') -> None:
        """Register a child namespace."""
        self.namespaces[identifier] = NamespaceElementInfo(identifier, is_private, namespace)

    def subroutine(self, identifier: str, is_private: bool,
                   signature: SubroutineSignature) -> 'SubroutineTypingContext':
        """Create a context for a subroutine."""
        return SubroutineTypingContext(self, identifier, is_private, signature)

    def register_subroutine(self, identifier: str, is_private: bool, signature: SubroutineSignature) -> None:
        """Register a subroutine."""
        self.subroutines[identifier] = NamespaceElementInfo(identifier, is_private, signature)

    def __enter__(self) -> 'NamespaceTypingContext':
        return self

    def __exit__(self, exc_type, exc_val, exc_tb) -> None:
        self.parent.register_namespace(self.identifier, self.is_private, self)


class FileTypingContext(NamespaceTypingContext):
    __slots__ = ()

    def __init__(self, identifier: str) -> None:
        super().__init__(identifier, False)
        self.register_type('char', False, Types.CHAR)


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
            CompilationWarning.add(location, WarningType.REDECLARATION,
                                   f'Variable {identifier!r} has already been defined at {original.location!r}')
        elif self.is_shadow(identifier):
            original = self.parent[identifier]
            CompilationWarning.add(location, WarningType.NAME_SHADOWING,
                                   f'Declaration shadows identifier {identifier!r} from outer scope',
                                   f'Declared originally at {original.location!r}')
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
    __slots__ = 'identifier', 'is_private', 'signature'

    def __init__(self, namespace: NamespaceTypingContext, identifier: str, is_private: bool,
                 signature: SubroutineSignature) -> None:
        super().__init__(namespace, return_type=signature.return_type)
        self.identifier = identifier
        self.is_private = is_private
        self.signature = signature
        for argument in self.signature.arguments:
            self.variables[argument.identifier] = VariableInfo(argument.location, argument.type)

    def __enter__(self) -> 'SubroutineTypingContext':
        return self

    def __exit__(self, exc_type, exc_val, exc_tb) -> None:
        self.namespace.register_subroutine(self.identifier, self.is_private, self.signature)


__all__ = ['TypedSubroutineArgument', 'SubroutineSignature', 'NamespaceTypingContext', 'FileTypingContext',
           'CodeBlockTypingContext']
