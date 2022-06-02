from abc import ABC
from typing import Generator

from data_types import DataType
from exceptions import CompilerException, Location
from intermediate_representation.assignment_targets import AssignmentTarget
from operations import *


class Instruction(ABC):
    def __init__(self, location: Location) -> None:
        self.location = location

    def may_return(self) -> bool:
        return False

    def may_break(self) -> bool:
        return False

    def early_exists(self) -> bool:
        return self.may_return() or self.early_exists()

    def __repr__(self) -> str:
        return self.__class__.__name__


class InstructionBlock(Instruction):
    @classmethod
    def empty(cls, location: Location = Location.unknown()):
        return cls(location)

    def __init__(self, location: Location, *instructions: Instruction) -> None:
        super().__init__(location)
        self._may_return: bool = False
        self._may_break: bool = False
        self.instructions: list[Instruction] = [*instructions]

    def may_return(self) -> bool:
        return self._may_return

    def may_break(self) -> bool:
        return self._may_break

    def __len__(self) -> int:
        return len(self.instructions)

    def is_empty(self) -> bool:
        return len(self) == 0

    def append(self, instruction: Instruction) -> None:
        self.instructions.append(instruction)
        if instruction.may_return():
            self._may_return = True
        if instruction.may_break():
            self._may_break = True

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


class Char(Expression):
    """A character literal."""

    def __init__(self, location: Location, value: int | bool) -> None:
        super().__init__(location)
        self.value = int(value) % 256

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


class Identifier(Expression):
    def __init__(self, location: Location, name: str) -> None:
        super().__init__(location)
        self.name = name

    def __str__(self) -> str:
        return self.name

    def __repr__(self) -> str:
        return f'{self.__class__.__name__}[{self.name}]'


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


class ArithmeticExpression(Expression, ABC):
    """An arithmetic expression contains an operation tree."""


class UnaryArithmeticExpression(ArithmeticExpression):
    def __init__(self, location: Location, operation: UnaryOperation, operand: Expression) -> None:
        super().__init__(location)
        self.operation = operation
        self.operand = operand

    def __str__(self) -> str:
        return f'({self.operation}{self.operand})'

    def __repr__(self) -> str:
        return f'{self.__class__.__name__}[{self.operation!r}, {self.operand!r}]'


class BinaryArithmeticExpression(ArithmeticExpression):
    def __init__(self, location: Location, operation: BinaryOperation, left: Expression, right: Expression) -> None:
        super().__init__(location)
        self.operation = operation
        self.left = left
        self.right = right

    def __str__(self) -> str:
        return f'({self.left} {self.operation} {self.right})'

    def __repr__(self) -> str:
        return f'{self.__class__.__name__}[{self.operation!r}, {self.left!r}, {self.right!r}]'


class ArrayAccessExpression(Expression):
    def __init__(self, location: Location, array: Expression, index: int) -> None:
        super().__init__(location)
        self.array = array
        self.index = index

    def __str__(self) -> str:
        return f'{self.array}[{self.index}]'

    def __repr__(self) -> str:
        return f'{self.__class__.__name__}[{self.array!r}, {self.index!r}]'


class ProcedureCall(Instruction):
    """A procedure call executes the instructions of the subroutine, without expecting a return value"""

    def __init__(self, location: Location, identifier: str, arguments: list[Expression]) -> None:
        super().__init__(location)
        self.identifier = identifier
        self.arguments = arguments

    def __str__(self) -> str:
        arguments = ', '.join(str(argument) for argument in self.arguments)
        return f'{self.identifier}({arguments})'

    def __repr__(self) -> str:
        if self.arguments:
            arguments = ', '.join(repr(argument) for argument in self.arguments)
            return f'{self.__class__.__name__}[{self.identifier}, {arguments}]'
        return f'{self.__class__.__name__}[{self.identifier}]'


class FunctionCall(ProcedureCall, Expression):
    """A function call executes the instructions of the subroutine and expects a return value."""

    def __init__(self, location: Location, identifier: str, arguments: list[Expression]) -> None:
        super().__init__(location, identifier, arguments)


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
    def __init__(self, location: Location, identifier: str, variable_type: DataType,
                 value: Expression | None = None) -> None:
        super().__init__(location)
        self.identifier = identifier
        self.type = variable_type
        self.value = value

    def __str__(self) -> str:
        return f'let {self.type} {self.identifier} = {self.value}'

    def __repr__(self) -> str:
        return f'{self.__class__.__name__}[{self.identifier}, {self.type!r}, {self.value!r}]'


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

    def may_return(self) -> bool:
        return self.body.may_return()

    def __str__(self) -> str:
        return f'loop ({self.count}) line {self.location.line}'

    def __repr__(self) -> str:
        return f'{self.__class__.__name__}[{self.count!r}, {self.body!r}]'


class WhileLoopStatement(Instruction):
    def __init__(self, location: Location, test: Expression, body: InstructionBlock) -> None:
        super().__init__(location)
        self.test = test
        self.body = body

    def may_return(self) -> bool:
        return self.body.may_return()

    def __str__(self) -> str:
        return f'while ({self.test}) line {self.location.line}'

    def __repr__(self) -> str:
        return f'{self.__class__.__name__}[{self.test!r}, {self.body!r}]'


class ForLoopStatement(Instruction):
    def __init__(self, location: Location, loop_type: DataType, loop_variable: str,
                 loop_array: Expression, body: InstructionBlock) -> None:
        super().__init__(location)
        self.loop_type = loop_type
        self.loop_variable = loop_variable
        self.loop_array = loop_array
        self.body = body

    def may_return(self) -> bool:
        return self.body.may_return()

    def __str__(self) -> str:
        return f'for ({self.loop_type} {self.loop_variable} : {self.loop_array}) line {self.location.line}'

    def __repr__(self) -> str:
        return f'{self.__class__.__name__}[{self.loop_type!r}, {self.loop_variable!r}, {self.loop_array!r},' \
               f' {self.body!r}]'


class ConditionalStatement(Instruction):
    def __init__(self, location: Location, test: Expression, if_body: InstructionBlock,
                 else_body: InstructionBlock) -> None:
        super().__init__(location)
        self.test = test
        self.if_body = if_body
        self.else_body = else_body

    def may_return(self) -> bool:
        return self.if_body.may_return() or self.else_body.may_return()

    def may_break(self) -> bool:
        return self.if_body.may_break() or self.else_body.may_break()

    def has_else(self) -> bool:
        return not self.else_body.is_empty()

    def __str__(self) -> str:
        return f'if ({self.test}) line {self.location.line}'

    def __repr__(self) -> str:
        return f'{self.__class__.__name__}[{self.test!r}, {self.if_body!r}, {self.else_body!r}]'


class ReturnInstruction(Instruction):
    def __init__(self, location: Location, value: Expression) -> None:
        super().__init__(location)
        self.value = value

    def may_return(self) -> bool:
        return True

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


__all__ = ['Instruction', 'InstructionBlock', 'Expression', 'Char', 'Array', 'Identifier', 'Tuple',
           'ArithmeticExpression', 'UnaryArithmeticExpression', 'BinaryArithmeticExpression', 'ArrayAccessExpression',
           'ProcedureCall', 'FunctionCall', 'Incrementation', 'Decrementation', 'VariableDeclaration', 'Assignment',
           'LoopStatement', 'WhileLoopStatement', 'ForLoopStatement', 'ConditionalStatement', 'ReturnInstruction',
           'ContextSnapshot']
