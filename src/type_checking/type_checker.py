from abc import ABC, abstractmethod
from typing import Generator

from data_types import *
from exceptions import *
from intermediate_representation import *
from type_checking.type_checked_instructions import *
from type_checking.typing_context import *


class TypeCheckedNamespaceElement(ABC):
    __slots__ = 'location', 'identifier', 'is_private'

    @classmethod
    def from_untyped(cls, context: NamespaceTypingContext, element: NamespaceElement) -> 'TypeCheckedNamespaceElement':
        if isinstance(element, Constant):
            return TypeCheckedConstant.from_untyped(context, element)
        if isinstance(element, NativeSubroutine):
            return TypeCheckedNativeSubroutine.from_untyped(context, element)
        if isinstance(element, Procedure):
            return TypeCheckedProcedure.from_untyped(context, element)
        if isinstance(element, Namespace):
            return TypeCheckedNamespace.from_untyped(context, element)
        raise ImpossibleException(f'Unknown namespace element type: {element.__class__}')

    def __init__(self, location: Location, identifier: str, is_private: bool) -> None:
        self.location = location
        self.identifier = identifier
        self.is_private = is_private

    def _modifier_prefix(self) -> str:
        s = ''
        if self.is_private:
            s += 'private '
        return s

    @abstractmethod
    def __str__(self) -> str:
        ...

    @abstractmethod
    def __repr__(self, indent: str = '') -> str:
        ...


class TypeCheckedConstant(TypeCheckedNamespaceElement):
    __slots__ = 'expression'

    @classmethod
    def from_untyped(cls, context: NamespaceTypingContext, constant: Constant) -> 'TypeCheckedConstant':
        value = evaluate(context, constant.expression)
        context.register_constant(constant.identifier, constant.is_private, value)
        return cls(constant.location, constant.identifier, constant.is_private, value)

    def __init__(self, location: Location, identifier: str, is_private: bool, expression: TypedExpression) -> None:
        super().__init__(location, identifier, is_private)
        self.expression = expression

    def type(self) -> DataType:
        return self.expression.type()

    def __str__(self) -> str:
        return f'#{self.identifier} = {self.expression}'

    def __repr__(self, indent: str = '') -> str:
        return f'{self.__class__.__name__}[{self.identifier}, {self.expression!r}]'


class TypeCheckedSubroutine(TypeCheckedNamespaceElement, ABC):
    __slots__ = 'arguments', 'return_type'

    def __init__(self, location: Location, identifier: str, is_private: bool, arguments: list[SubroutineArgument],
                 return_type: DataType | None) -> None:
        super().__init__(location, identifier, is_private)
        if identifier in ('print', 'println', 'input', 'log'):
            message = f'This subroutine cannot be called from this namespace because {identifier!r} is a reserved name'
            CompilationWarning.add(location, message, WarningType.RESERVED_NAME)
        self.arguments = arguments
        self.return_type = return_type
        locations = {}
        for argument in self.arguments:
            if argument.identifier in locations:
                message = f'Argument {argument.identifier!r} has already been defined at {locations[argument.identifier]!r}'
                CompilationWarning.add(argument.location, message, WarningType.REDECLARATION)
            locations[argument.identifier] = argument.location

    def is_function(self) -> bool:
        return self.return_type is not None

    def _keyword(self) -> str:
        if self.is_function():
            return 'func'
        return 'proc'

    def _argument_string(self) -> str:
        return ', '.join(repr(argument) for argument in self.arguments)

    def __str__(self) -> str:
        prefix = f'{self._modifier_prefix()}{self._keyword()}'
        suffix = ''
        if self.is_function():
            suffix = f' -> {self.return_type}'
        return f'{prefix} {self.identifier}({self._argument_string()}){suffix} line {self.location.line}'


class TypeCheckedNativeSubroutine(TypeCheckedSubroutine):
    __slots__ = 'offset', 'bf_code'

    @classmethod
    def from_untyped(cls, context: NamespaceTypingContext,
                     subroutine: NativeSubroutine) -> 'TypeCheckedNativeSubroutine':
        signature = SubroutineSignature(subroutine.location, subroutine.arguments, subroutine.return_type)
        context.register_subroutine(subroutine.identifier, subroutine.is_private, signature)
        return cls(subroutine.location, subroutine.identifier, subroutine.is_private, subroutine.arguments,
                   subroutine.return_type, subroutine.offset, subroutine.bf_code)

    def __init__(self, location: Location, identifier: str, is_private: bool, arguments: list[SubroutineArgument],
                 return_type: DataType | None, offset: int, bf_code: str) -> None:
        super().__init__(location, identifier, is_private, arguments, return_type)
        self.offset = offset
        self.bf_code = bf_code
        message = 'Using native Brainfuck code is not recommended. Every cell should be reset to 0 (except those' \
                  ' containing the return value).'
        CompilationWarning.add(self.location, message, WarningType.NATIVE_CODE)

    def _modifier_prefix(self) -> str:
        return super()._modifier_prefix() + 'native '

    def __repr__(self, indent: str = '') -> str:
        return f'```{self.bf_code}```'


class TypeCheckedProcedure(TypeCheckedSubroutine):
    __slots__ = 'body'

    @classmethod
    def from_untyped(cls, context: NamespaceTypingContext, procedure: Procedure) -> 'TypeCheckedProcedure':
        signature = SubroutineSignature(procedure.location, procedure.arguments, procedure.return_type)
        with context.subroutine(procedure.identifier, procedure.is_private, signature) as subroutine_context:
            body = TypeCheckedInstructionBlock.from_untyped(subroutine_context, procedure.body, allow_return=True)
        return cls(procedure.location, procedure.identifier, procedure.is_private, procedure.arguments,
                   procedure.return_type, body)

    def __init__(self, location: Location, identifier: str, is_private: bool, arguments: list[SubroutineArgument],
                 return_type: DataType | None, body: TypeCheckedInstructionBlock) -> None:
        super().__init__(location, identifier, is_private, arguments, return_type)
        self.body = body

    def _argument_string(self) -> str:
        return ', '.join(repr(argument) for argument in self.arguments)

    def __repr__(self, indent: str = '') -> str:
        return f'({self._argument_string()}) -> {self.body.__repr__(indent)}'


class TypeCheckedNamespace(TypeCheckedNamespaceElement):
    """A type checked namespace is a namespace that has been type checked (the type of every expression is known)."""
    __slots__ = 'elements'

    @classmethod
    def from_file(cls, file: File) -> 'TypeCheckedNamespace':
        typing_context = NamespaceTypingContext(file.identifier, False)
        return cls.from_untyped(typing_context, file)

    @classmethod
    def from_untyped(cls, context: NamespaceTypingContext, namespace: Namespace) -> 'TypeCheckedNamespace':
        elements = []
        with context.namespace(namespace.identifier, namespace.is_private) as namespace_context:
            for element in namespace.elements:
                type_checked_element = TypeCheckedNamespaceElement.from_untyped(namespace_context, element)
                elements.append(type_checked_element)
        return cls(namespace.location, namespace.identifier, namespace.is_private, elements)

    def __init__(self, location: Location, identifier: str, is_private: bool,
                 elements: list[TypeCheckedNamespaceElement]) -> None:
        super().__init__(location, identifier, is_private)
        self.elements = elements

    def __iter__(self) -> Generator[TypeCheckedNamespaceElement, None, None]:
        for element in self.elements:
            yield element

    def __str__(self) -> str:
        return f'{self._modifier_prefix()}namespace {self.identifier} line {self.location.line}'

    def __repr__(self, indent: str = '') -> str:
        inner_indent = indent + '    '
        s = f'{indent}{self.__class__.__name__}['
        if self.elements:
            s += '\n'
            for element in self.elements:
                s += f'{inner_indent}{element.identifier} = {element.__repr__(inner_indent)},\n'
            s += indent
        return s + ']'


__all__ = ['TypeCheckedConstant', 'TypeCheckedSubroutine', 'TypeCheckedNativeSubroutine', 'TypeCheckedProcedure',
           'TypeCheckedNamespace']
