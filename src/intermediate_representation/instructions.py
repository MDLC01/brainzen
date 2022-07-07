from abc import ABC, abstractmethod
from typing import Generator

from data_types import DataType
from exceptions import CompilerException, Location
from intermediate_representation.assignment_targets import AssignmentTarget
from reference import *
from tokenization import Token


class Instruction(ABC):
    def __init__(self, location: Location) -> None:
        self.location = location

    def __repr__(self) -> str:
        return self.__class__.__name__


class InstructionBlock(Instruction):
    def __init__(self, location: Location, *instructions: Instruction) -> None:
        super().__init__(location)
        self.instructions: list[Instruction] = [*instructions]

    def __len__(self) -> int:
        return len(self.instructions)

    def is_empty(self) -> bool:
        return len(self) == 0

    def append(self, instruction: Instruction) -> None:
        self.instructions.append(instruction)

    def __iter__(self) -> Generator[Instruction, None, None]:
        for instruction in self.instructions:
            yield instruction

    def __repr__(self, indent='') -> str:
        instructions = '{'
        if not self.is_empty():
            for instruction in self.instructions:
                instructions += f'\n{indent}    {instruction.__repr__()};'
            instructions += '\n' + indent + '}'
        else:
            instructions += '}'
        return instructions


class Expression(Instruction, ABC):
    """An expression is a peace of code that has a value. In Brainzen, every expression is a valid instruction."""


class ConstantReference(Expression):
    """A reference to a constant."""

    def __init__(self, location: Location, reference: Reference) -> None:
        super().__init__(location)
        self.reference = reference

    def __str__(self) -> str:
        return f'#{self.reference}'

    def __repr__(self) -> str:
        return f'{self.__class__.__name__}[{self.reference}]'


class Char(Expression):
    """A character literal."""

    def __init__(self, location: Location, value: int) -> None:
        super().__init__(location)
        self.value = value

    def __str__(self) -> str:
        return str(self.value)

    def __repr__(self) -> str:
        return f'{self.__class__.__name__}[{self.value!r}]'


class Array(Expression):
    """An array literal."""

    @classmethod
    def from_string(cls, location: Location, string: str) -> 'Array':
        return cls(location, [Char(location, ord(char)) for char in string])

    def __init__(self, location: Location, value: list[Expression]) -> None:
        super().__init__(location)
        self.value = value

    def __str__(self) -> str:
        elements = ', '.join(str(element) for element in self.value)
        return f'[{elements}]'

    def __repr__(self) -> str:
        return f'{self.__class__.__name__}[{self.value!r}]'


class Range(Expression):
    def __init__(self, location: Location, start: int, end: int) -> None:
        super().__init__(location)
        self.start = start
        self.end = end

    def __str__(self) -> str:
        return f'{self.start}..{self.end}'

    def __repr__(self) -> str:
        return f'{self.__class__.__name__}[{self.start!r}, {self.end!r}]'


class Iterator(ABC):
    __slots__ = 'location', 'target'

    def __init__(self, location: Location, target: AssignmentTarget) -> None:
        self.location = location
        self.target = target

    @abstractmethod
    def __str__(self) -> str:
        ...

    @abstractmethod
    def __repr__(self) -> str:
        ...


class ArrayIterator(Iterator):
    __slots__ = 'array'

    def __init__(self, location: Location, target: AssignmentTarget, array: Expression) -> None:
        super().__init__(location, target)
        self.array = array

    def __str__(self) -> str:
        return f'{self.target} : {self.array}'

    def __repr__(self) -> str:
        return f'{self.__class__.__name__}[{self.target!r}, {self.array!r}]'


class IteratorGroup:
    __slots__ = 'location', 'iterators'

    def __init__(self, location: Location, iterators: list[Iterator]) -> None:
        self.location = location
        self.iterators = iterators

    def __str__(self) -> str:
        return ', '.join(iterator.__str__() for iterator in self.iterators)

    def __repr__(self) -> str:
        iterators = ', '.join(iterator.__repr__() for iterator in self.iterators)
        return f'{self.__class__.__name__}[{iterators}]'


class IteratorChain:
    __slots__ = 'location', 'groups'

    def __init__(self, location: Location, groups: list[IteratorGroup]) -> None:
        self.location = location
        self.groups = groups

    def __str__(self) -> str:
        return ' | '.join(group.__str__() for group in self.groups)

    def __repr__(self) -> str:
        groups = ', '.join(group.__repr__() for group in self.groups)
        return f'{self.__class__.__name__}[{groups}]'


class ArrayComprehension(Expression):
    def __init__(self, location: Location, element_format: Expression, iterators: IteratorChain) -> None:
        super().__init__(location)
        self.element_format = element_format
        self.iterators = iterators

    def __str__(self) -> str:
        return f'[{self.element_format} | {self.iterators}]'

    def __repr__(self) -> str:
        return f'{self.__class__.__name__}[{self.element_format!r}, {self.iterators!r}]'


class Tuple(Expression):
    """A tuple literal."""

    @classmethod
    def from_elements(cls, location: Location, elements: list[Expression]) -> Expression:
        if len(elements) == 1:
            return elements[0]
        return cls(location, elements)

    def __init__(self, location: Location, elements: list[Expression]) -> None:
        super().__init__(location)
        self.elements = elements
        if len(self.elements) < 2:
            raise CompilerException('Tuple must be contain at least two elements')

    def __str__(self) -> str:
        elements = ', '.join(str(element) for element in self.elements)
        return f'({elements})'

    def __repr__(self) -> str:
        return f'{self.__class__.__name__}[{self.elements!r}]'


class Identifier(Expression):
    def __init__(self, location: Location, name: str) -> None:
        super().__init__(location)
        self.name = name

    def __str__(self) -> str:
        return self.name

    def __repr__(self) -> str:
        return f'{self.__class__.__name__}[{self.name}]'


class ArithmeticExpression(Expression, ABC):
    """An arithmetic expression contains an operation tree."""


class UnaryArithmeticExpression(ArithmeticExpression):
    def __init__(self, location: Location, operator: Token, operand: Expression) -> None:
        super().__init__(location)
        self.operator = operator
        self.operand = operand

    def __str__(self) -> str:
        return f'({self.operator}{self.operand})'

    def __repr__(self) -> str:
        return f'{self.__class__.__name__}[{self.operator!r}, {self.operand!r}]'


class BinaryArithmeticExpression(ArithmeticExpression):
    def __init__(self, location: Location, operator: Token, left: Expression, right: Expression) -> None:
        super().__init__(location)
        self.operator = operator
        self.left = left
        self.right = right

    def __str__(self) -> str:
        return f'({self.left} {self.operator} {self.right})'

    def __repr__(self) -> str:
        return f'{self.__class__.__name__}[{self.operator!r}, {self.left!r}, {self.right!r}]'


class ArraySubscriptExpression(Expression):
    def __init__(self, location: Location, array: Expression, index: Expression) -> None:
        super().__init__(location)
        self.array = array
        self.index = index

    def __str__(self) -> str:
        return f'{self.array}[{self.index}]'

    def __repr__(self) -> str:
        return f'{self.__class__.__name__}[{self.array!r}, {self.index!r}]'


class ArraySlicingExpression(Expression):
    def __init__(self, location: Location, array: Expression, start: Expression, stop: Expression) -> None:
        super().__init__(location)
        self.array = array
        self.start = start
        self.stop = stop

    def __str__(self) -> str:
        return f'{self.array}[{self.start}:{self.stop}]'

    def __repr__(self) -> str:
        return f'{self.__class__.__name__}[{self.array!r}, {self.start}, {self.stop}]'


class ProcedureCall(Instruction):
    """A procedure call executes the instructions of the subroutine, without expecting a return value"""

    def __init__(self, location: Location, reference: Reference, arguments: list[Expression]) -> None:
        super().__init__(location)
        self.reference = reference
        self.arguments = arguments

    def __str__(self) -> str:
        arguments = ', '.join(str(argument) for argument in self.arguments)
        return f'{self.reference}({arguments})'

    def __repr__(self) -> str:
        if self.arguments:
            arguments = ', '.join(repr(argument) for argument in self.arguments)
            return f'{self.__class__.__name__}[{self.reference}, {arguments}]'
        return f'{self.__class__.__name__}[{self.reference}]'


class FunctionCall(ProcedureCall, Expression):
    """A function call executes the instructions of the subroutine and expects a return value."""

    def __init__(self, location: Location, reference: Reference, arguments: list[Expression]) -> None:
        super().__init__(location, reference, arguments)


class Incrementation(Instruction):
    def __init__(self, location: Location, identifier: str) -> None:
        super().__init__(location)
        self.identifier = identifier

    def __str__(self) -> str:
        return f'{self.identifier}++'

    def __repr__(self) -> str:
        return f'{self.__class__.__name__}[{self.identifier}]'


class Decrementation(Instruction):
    def __init__(self, location: Location, identifier: str) -> None:
        super().__init__(location)
        self.identifier = identifier

    def __str__(self) -> str:
        return f'{self.identifier}--'

    def __repr__(self) -> str:
        return f'{self.__class__.__name__}[{self.identifier}]'


class VariableDeclaration(Instruction):
    def __init__(self, location: Location, target: AssignmentTarget, variable_type: DataType) -> None:
        super().__init__(location)
        self.target = target
        self.type = variable_type

    def __str__(self) -> str:
        return f'let {self.target}: {self.type}'

    def __repr__(self) -> str:
        return f'{self.__class__.__name__}[{self.target!r}, {self.type!r}]'


class VariableCreation(Instruction):
    def __init__(self, location: Location, target: AssignmentTarget, value: Expression) -> None:
        super().__init__(location)
        self.target = target
        self.value = value

    def __str__(self) -> str:
        return f'let {self.target} = {self.value}'

    def __repr__(self) -> str:
        return f'{self.__class__.__name__}[{self.target!r}, {self.value!r}]'


class Assignment(Instruction):
    def __init__(self, location: Location, target: AssignmentTarget, value: Expression) -> None:
        super().__init__(location)
        self.target = target
        self.value = value

    def __str__(self) -> str:
        return f'{self.target} = {self.value}'

    def __repr__(self) -> str:
        return f'{self.__class__.__name__}[{self.target!r}, {self.value!r}]'


class LoopStatement(Instruction):
    def __init__(self, location: Location, count: Expression, body: InstructionBlock) -> None:
        super().__init__(location)
        self.count = count
        self.body = body

    def __str__(self) -> str:
        return f'loop ({self.count}) line {self.location.line}'

    def __repr__(self) -> str:
        return f'{self.__class__.__name__}[{self.count!r}, {self.body!r}]'


class WhileLoopStatement(Instruction):
    def __init__(self, location: Location, test: Expression, body: InstructionBlock) -> None:
        super().__init__(location)
        self.test = test
        self.body = body

    def __str__(self) -> str:
        return f'while ({self.test}) line {self.location.line}'

    def __repr__(self) -> str:
        return f'{self.__class__.__name__}[{self.test!r}, {self.body!r}]'


class DoWhileLoopStatement(Instruction):
    def __init__(self, location: Location, body: InstructionBlock, test: Expression) -> None:
        super().__init__(location)
        self.body = body
        self.test = test

    def __str__(self) -> str:
        return f'do while ({self.test}) line {self.location.line}'

    def __repr__(self) -> str:
        return f'{self.__class__.__name__}[{self.body!r}, {self.test!r}]'


class ForLoopStatement(Instruction):
    def __init__(self, location: Location, iterators: IteratorGroup, body: InstructionBlock) -> None:
        super().__init__(location)
        self.iterators = iterators
        self.body = body

    def __str__(self) -> str:
        return f'for ({self.iterators}) line {self.location.line}'

    def __repr__(self) -> str:
        return f'{self.__class__.__name__}[{self.iterators!r}, {self.body!r}]'


class ConditionalStatement(Instruction):
    def __init__(self, location: Location, test: Expression, if_body: InstructionBlock,
                 else_body: InstructionBlock | None) -> None:
        super().__init__(location)
        self.test = test
        self.if_body = if_body
        self.else_body = else_body

    def has_else(self) -> bool:
        return self.else_body is not None

    def __str__(self) -> str:
        return f'if ({self.test}) line {self.location.line}'

    def __repr__(self) -> str:
        return f'{self.__class__.__name__}[{self.test!r}, {self.if_body!r}, {self.else_body!r}]'


class ReturnInstruction(Instruction):
    def __init__(self, location: Location, value: Expression) -> None:
        super().__init__(location)
        self.value = value

    def __str__(self) -> str:
        return f'return {self.value}'

    def __repr__(self) -> str:
        return f'{self.__class__.__name__}[{self.value!r}]'


class ContextSnapshot(Instruction):
    def __init__(self, location: Location, identifier: str | None = None) -> None:
        super().__init__(location)
        self.identifier = identifier

    def __str__(self) -> str:
        if self.identifier is not None:
            return f'{self.identifier}?'
        return '?'

    def __repr__(self) -> str:
        return f'{self.__class__.__name__}'


__all__ = ['Instruction', 'InstructionBlock', 'Expression', 'ConstantReference', 'Char', 'Array', 'Range', 'Iterator',
           'ArrayIterator', 'IteratorGroup', 'IteratorChain', 'ArrayComprehension', 'Tuple', 'Identifier',
           'ArithmeticExpression', 'UnaryArithmeticExpression', 'BinaryArithmeticExpression',
           'ArraySubscriptExpression', 'ArraySlicingExpression', 'ProcedureCall', 'FunctionCall', 'Incrementation',
           'Decrementation', 'VariableDeclaration', 'VariableCreation', 'Assignment', 'LoopStatement',
           'WhileLoopStatement', 'DoWhileLoopStatement', 'ForLoopStatement', 'ConditionalStatement',
           'ReturnInstruction', 'ContextSnapshot']
