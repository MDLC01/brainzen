from collections import OrderedDict
from typing import Generator

from data_types import DataType
from exceptions import Location
from intermediate_representation import InstructionBlock, Namespace, Subroutine
from type_checking.type_checked_instructions import *
from type_checking.typing_context import *


class TypeCheckedSubroutine:
    @classmethod
    def from_subroutine(cls, context: TypingContext, subroutine: Subroutine) -> 'TypeCheckedSubroutine':
        return cls(context, subroutine.location, subroutine.identifier, subroutine.return_type, subroutine.arguments,
                   subroutine.instructions)

    def __init__(self, context: TypingContext, location: Location, identifier: str, return_type: DataType,
                 arguments: list[SubroutineArgument], instructions: InstructionBlock) -> None:
        self.location: Location = location
        self.identifier: str = identifier
        self.return_type: DataType = return_type
        self.arguments: list[SubroutineArgument] = arguments
        # Type checking
        for argument in self.arguments:
            context.add_variable(self.location, argument.identifier, argument.type)
        self.instructions: TypeCheckedInstructionBlock = TypeCheckedInstructionBlock(context, instructions)

    def __repr__(self, indent='') -> str:
        arguments = ', '.join(repr(argument) for argument in self.arguments)
        return f'({arguments}) -> {self.instructions.__repr__(indent)}'


class TypeCheckedNamespace:
    """A type checked namespace is a namespace that has been type checked (the type of every expression is known)."""

    def __init__(self, namespace: Namespace) -> None:
        self.location: Location = namespace.location
        self.identifier: str = namespace.identifier
        self.subroutines: OrderedDict[str, TypeCheckedSubroutine] = self._type_check_subroutines(namespace)

    def _type_check_subroutines(self, namespace: Namespace) -> OrderedDict[str, TypeCheckedSubroutine]:
        context = TypingContext()
        type_checked_subroutines: OrderedDict[str, TypeCheckedSubroutine] = OrderedDict()
        for identifier, subroutine in namespace:
            signature = SubroutineSignature(subroutine.arguments, subroutine.return_type)
            context.start_subroutine_definition(identifier, signature)
            type_checked_subroutines[identifier] = TypeCheckedSubroutine.from_subroutine(context, subroutine)
            context.end_subroutine_definition()
        return type_checked_subroutines

    def __iter__(self) -> Generator[tuple[str, TypeCheckedSubroutine], None, None]:
        for identifier, subroutine in self.subroutines.items():
            yield identifier, subroutine

    def __repr__(self) -> str:
        indent = '    '
        s = f'{self.__class__.__name__}['
        if self.subroutines:
            s += '\n'
        for identifier, subroutine in self:
            s += f'{indent}{identifier} = {subroutine.__repr__(indent)},\n'
        return s + ']'


__all__ = ['TypeCheckedSubroutine', 'TypeCheckedNamespace']
