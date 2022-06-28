from abc import ABC, abstractmethod
from typing import Generator

from data_types import *
from exceptions import *
from intermediate_representation.instructions import *
from reference import Reference
from type_checking.operations import *
from type_checking.typed_assignment_targets import *
from type_checking.typed_declaration_target import *
from type_checking.typing_context import *


class TypeCheckedInstruction(ABC):
    @staticmethod
    def from_instruction(context: CodeBlockTypingContext, instruction: Instruction) -> 'TypeCheckedInstruction':
        match instruction:
            case InstructionBlock() as instruction_block:
                return TypeCheckedInstructionBlock(context, instruction_block)
            case Expression() as expression:
                return TypedExpression.from_expression(context, expression)
            case ProcedureCall(reference=Reference(namespace=None, identifier='print')) as procedure_call:
                return PrintCall(context, procedure_call)
            case ProcedureCall(reference=Reference(namespace=None, identifier='println')) as procedure_call:
                return PrintCall(context, procedure_call, True)
            case ProcedureCall(reference=Reference(namespace=None, identifier='input')) as procedure_call:
                return InputCall(procedure_call)
            case ProcedureCall(reference=Reference(namespace=None, identifier='log')) as procedure_call:
                return LogCall.from_procedure_call(context, procedure_call)
            case ProcedureCall() as procedure_call:
                return TypeCheckedProcedureCall(context, procedure_call)
            case Incrementation() as incrementation:
                return TypeCheckedIncrementation(context, incrementation)
            case Decrementation() as decrementation:
                return TypeCheckedDecrementation(context, decrementation)
            case VariableDeclaration() as variable_declaration:
                return TypeCheckedVariableDeclaration.from_variable_declaration(context, variable_declaration)
            case Assignment() as assignment:
                return TypeCheckedAssignment(context, assignment)
            case LoopStatement() as loop_statement:
                return TypeCheckedLoopStatement(context, loop_statement)
            case WhileLoopStatement() as while_loop_statement:
                return TypeCheckedWhileLoopStatement(context, while_loop_statement)
            case DoWhileLoopStatement() as do_while_loop_statement:
                return TypeCheckedDoWhileLoopStatement(context, do_while_loop_statement)
            case ForLoopStatement() as for_loop_statement:
                return TypeCheckedForLoopStatement.from_for_loop_statement(context, for_loop_statement)
            case ConditionalStatement() as conditional_statement:
                return TypeCheckedConditionalStatement(context, conditional_statement)
            case ReturnInstruction() as return_instruction:
                return TypeCheckedReturnInstruction(context, return_instruction)
            case ContextSnapshot() as context_snapshot:
                return TypeCheckedContextSnapshot(context_snapshot)
        raise ImpossibleException(f'Unknown instruction type: {instruction.__class__.__name__}')

    def __init__(self, location: Location) -> None:
        self.location = location

    def __repr__(self) -> str:
        return self.__class__.__name__


class TypeCheckedInstructionBlock(TypeCheckedInstruction):
    def __init__(self, context: CodeBlockTypingContext, instruction_block: InstructionBlock, *,
                 allow_return: bool = False) -> None:
        super().__init__(instruction_block.location)
        subscope = context.subscope(allow_return=allow_return)
        self.instructions = [TypeCheckedInstruction.from_instruction(subscope, instruction)
                             for instruction in instruction_block.instructions]

    def is_empty(self) -> bool:
        return len(self.instructions) == 0

    def __len__(self) -> int:
        return len(self.instructions)

    def __iter__(self) -> Generator[TypeCheckedInstruction, None, None]:
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


class TypedExpression(TypeCheckedInstruction, ABC):
    """An expression whose type is known."""

    @staticmethod
    def from_expression(context: CodeBlockTypingContext, expression: Expression) -> 'TypedExpression':
        match expression:
            case ConstantReference(reference=reference):
                return context.namespace.get_constant_value(reference)
            case Char() as char:
                return LiteralChar.from_char(char)
            case Array() as array:
                return LiteralArray.from_array(context, array)
            case ArrayComprehension() as array_comprehension:
                return TypedArrayComprehension.from_untyped(context, array_comprehension)
            case Tuple() as tuple_expression:
                return LiteralTuple.from_tuple_expression(context, tuple_expression)
            case Identifier() as identifier:
                return TypedIdentifier(context, identifier)
            case UnaryArithmeticExpression() as unary_arithmetic_expression:
                return TypedUnaryArithmeticExpression(context, unary_arithmetic_expression)
            case BinaryArithmeticExpression() as binary_arithmetic_expression:
                return TypedBinaryArithmeticExpression(context, binary_arithmetic_expression)
            case ArraySubscriptExpression() as array_subscript_expression:
                return TypedArraySubscriptExpression(context, array_subscript_expression)
            case ArraySlicingExpression() as array_slicing_expression:
                return TypedArraySlicingExpression(context, array_slicing_expression)
            case FunctionCall(location=location, reference=Reference(namespace=None, identifier='print')):
                raise CompilationException(location, f"Procedure 'print' does not return anything")
            case FunctionCall(location=location, reference=Reference(namespace=None, identifier='println')):
                raise CompilationException(location, f"Procedure 'println' does not return anything")
            case FunctionCall(reference=Reference(namespace=None, identifier='input')) as function_call:
                return InputCall(function_call)
            case FunctionCall(location=location, reference=Reference(namespace=None, identifier='log')):
                raise CompilationException(location, f"Procedure 'log' does not return anything")
            case FunctionCall() as function_call:
                return TypedFunctionCall(context, function_call)
        raise ImpossibleException(f'Unknown expression type: {expression.__class__.__name__}')

    @abstractmethod
    def type(self) -> DataType:
        ...


Value = int | list['Value'] | tuple['Value', ...]


def value_of(expression: TypedExpression) -> Value:
    if isinstance(expression, LiteralChar):
        return expression.value
    if isinstance(expression, LiteralArray):
        return [value_of(element) for element in expression.value]
    if isinstance(expression, LiteralTuple):
        return tuple(value_of(element) for element in expression.elements)
    raise CompilerException('Unable to get value from non-literal types at compile time')


def expression_of(location: Location, value: Value) -> TypedExpression:
    if isinstance(value, bool):
        return LiteralChar(location, int(value))
    if isinstance(value, int):
        return LiteralChar(location, value)
    if isinstance(value, list):
        return LiteralArray(location, [expression_of(location, element) for element in value])
    if isinstance(value, tuple):
        return LiteralTuple(location, [expression_of(location, element) for element in value])
    raise CompilerException(f'Invalid Python equivalent: {value!r}')


def compute_unary_operation(operation: UnaryOperation, operand: Value) -> Value:
    match operation:
        case NegationOperation():
            return int(not operand)
        case BoolNormalizationOperation():
            return int(bool(operand))
        case OppositionOperation():
            return -operand
        case ArrayOppositionOperation():
            return [-element for element in operand]
        case ArrayFlatteningOperation():
            return [element for inner_array in operand for element in inner_array]
    raise CompilerException(f'Unknown unary operation: {operation!r}')


def compute_binary_operation(operation: BinaryOperation, left: Value, right: Value) -> Value:
    match operation:
        case EqualityTestOperation():
            return int(left == right)
        case DifferenceTestOperation():
            return int(left != right)
        case StrictInequalityTestOperation():
            return int(left < right)
        case LargeInequalityTestOperation():
            return int(left <= right)
        case InverseStrictInequalityTestOperation():
            return int(left > right)
        case InverseLargeInequalityTestOperation():
            return int(left >= right)
        case ConjunctionOperation():
            return int(left and right)
        case DisjunctionOperation():
            return int(left or right)
        case AdditionOperation():
            return left + right
        case SubtractionOperation():
            return left - right
        case MultiplicationOperation():
            return left * right
        case DivisionOperation():
            return left // right
        case ModuloOperation():
            return left % right
        case ConcatenationOperation():
            return left + right
        case ArrayScalingOperation():
            return [left * element for element in right]
    raise CompilerException(f'Unknown binary operation: {operation!r}')


def evaluate(context: NamespaceTypingContext, expression: Expression) -> TypedExpression:
    match expression:
        case ConstantReference(reference=reference):
            return context.get_constant_value(reference)
        case Char() as char:
            return LiteralChar.from_char(char)
        case Array(location=location, value=value):
            return LiteralArray(location, [evaluate(context, element) for element in value])
        case Tuple(location=location, elements=elements):
            return LiteralTuple(location, [evaluate(context, element) for element in elements])
        case UnaryArithmeticExpression(location=location, operator=operator, operand=operand):
            typed_operand = evaluate(context, operand)
            operation = UnaryOperation.from_operator(location, operator, typed_operand.type())
            result = compute_unary_operation(operation, value_of(typed_operand))
            return expression_of(location, result)
        case BinaryArithmeticExpression(location=location, operator=operator, left=left, right=right):
            typed_left = evaluate(context, left)
            typed_right = evaluate(context, right)
            operation = BinaryOperation.from_operator(location, operator, typed_left.type(), typed_right.type())
            result = compute_binary_operation(operation, value_of(typed_left), value_of(typed_right))
            return expression_of(location, result)
    message = f'Invalid expression type for compile time evaluation: {expression.__class__.__name__}'
    raise CompilationException(expression.location, message)


class LiteralChar(TypedExpression):
    @classmethod
    def from_char(cls, char: Char) -> 'LiteralChar':
        if char.value < 0 or char.value > 255:
            CompilationWarning.add(char.location, 'Char literal is outside of the possible range')
        return cls(char.location, char.value)

    def __init__(self, location: Location, value: int) -> None:
        super().__init__(location)
        self.value = value % 256

    def type(self) -> DataType:
        return Types.CHAR

    def __str__(self) -> str:
        return str(self.value)

    def __repr__(self) -> str:
        return f'{self.__class__.__name__}[{self.value!r}]'


class LiteralArray(TypedExpression):
    class EmptyType(DataType):
        def size(self) -> int:
            return 0

        def __eq__(self, other) -> bool:
            return isinstance(other, type(self))

        def __repr__(self) -> str:
            return repr(type(self))

    @classmethod
    def from_array(cls, context: CodeBlockTypingContext, array: Array) -> 'LiteralArray':
        return cls(array.location, [TypedExpression.from_expression(context, expression) for expression in array.value])

    def __init__(self, location: Location, value: list[TypedExpression]) -> None:
        super().__init__(location)
        self.value: list[TypedExpression] = value
        self.content_type: DataType = LiteralArray.EmptyType()
        for element in self.value:
            if self.content_type == LiteralArray.EmptyType():
                self.content_type = element.type()
            elif element.type() != self.content_type:
                message = f'Expected element of type {self.content_type} but found {element.type()}'
                raise CompilationException(element.location, message)

    def __len__(self) -> int:
        return len(self.value)

    def type(self) -> DataType:
        return ArrayType(self.content_type, len(self.value))

    def __str__(self) -> str:
        s = '"'
        for element in self.value:
            if not isinstance(element, LiteralChar):
                elements = ', '.join(str(element) for element in self.value)
                return f'[{elements}]'
            s += chr(element.value)
        return s + '"'

    def __repr__(self) -> str:
        return f'{self.__class__.__name__}[{self.value!r}]'


class TypedIterator(ABC):
    __slots__ = 'location', 'target'

    @classmethod
    def from_untyped(cls, context: CodeBlockTypingContext, iterator: Iterator) -> 'TypedIterator':
        if isinstance(iterator, ArrayIterator):
            return TypedArrayIterator.from_untyped(context, iterator)
        raise ImpossibleException(f'Unknown iterator type: {iterator.__class__.__name__}')

    def __init__(self, location: Location, target: TypedDeclarationTarget) -> None:
        self.location = location
        self.target = target

    @abstractmethod
    def count(self) -> int:
        ...

    @abstractmethod
    def type(self) -> DataType:
        ...

    @abstractmethod
    def __str__(self) -> str:
        ...

    @abstractmethod
    def __repr__(self) -> str:
        ...


class TypedArrayIterator(TypedIterator):
    __slots__ = 'array', 'array_type'

    @classmethod
    def from_untyped(cls, context: CodeBlockTypingContext, array_iterator: ArrayIterator) -> 'TypedArrayIterator':
        array = TypedExpression.from_expression(context, array_iterator.array)
        array_type = array.type()
        if not isinstance(array_type, ArrayType):
            raise CompilationException(array.location, f'Expected array but found {array_type}')
        target = TypedDeclarationTarget.from_untyped(context, array_iterator.target, array_type.base_type)
        return cls(array_iterator.location, target, array)

    def __init__(self, location: Location, target: TypedDeclarationTarget, array: TypedExpression) -> None:
        super().__init__(location, target)
        self.array = array
        array_type = self.array.type()
        if not isinstance(array_type, ArrayType):
            raise CompilerException(f'Expected array but found {array_type}')
        self.array_type = array_type

    def count(self) -> int:
        return self.array_type.count

    def type(self) -> DataType:
        return self.array_type.base_type

    def __str__(self) -> str:
        return f'{self.target} : {self.array}'

    def __repr__(self) -> str:
        return f'{self.__class__.__name__}[{self.target!r}, {self.array!r}]'


class TypedIteratorGroup:
    __slots__ = 'location', 'iterators'

    @classmethod
    def from_untyped(cls, context: CodeBlockTypingContext, iterator_group: IteratorGroup) -> 'TypedIteratorGroup':
        iterators = []
        expected_count = None
        for untyped_iterator in iterator_group.iterators:
            # FIXME: The code bellow type checks because variables are registered when iterators are type checked:
            #  for (x : ["Aa", "Bb", "Cc"], y : x) {...}
            #  This should be invalid because iterators in a group do not interact with each other.
            iterator = TypedIterator.from_untyped(context, untyped_iterator)
            if expected_count is None:
                expected_count = iterator.count()
            elif iterator.count() != expected_count:
                message = f'Expected iterator of size {expected_count} but found iterator of size {iterator.count()}'
                raise CompilationException(iterator.location, message)
            iterators.append(iterator)
        return cls(iterator_group.location, iterators)

    def __init__(self, location: Location, iterators: list[TypedIterator]) -> None:
        self.location = location
        self.iterators = iterators

    def count(self) -> int:
        return self.iterators[0].count()

    def __str__(self) -> str:
        return ', '.join(iterator.__str__() for iterator in self.iterators)

    def __repr__(self) -> str:
        iterators = ', '.join(iterator.__repr__() for iterator in self.iterators)
        return f'{self.__class__.__name__}[{iterators}]'


class TypedIteratorChain:
    __slots__ = 'location', 'groups'

    @classmethod
    def from_untyped(cls, context: CodeBlockTypingContext, iterator_chain: IteratorChain) -> 'TypedIteratorChain':
        groups = [TypedIteratorGroup.from_untyped(context, group) for group in reversed(iterator_chain.groups)]
        return cls(iterator_chain.location, groups)

    def __init__(self, location: Location, groups: list[TypedIteratorGroup]) -> None:
        self.location = location
        self.groups = groups

    def count(self) -> int:
        count = 1
        for group in self.groups:
            count *= group.count()
        return count

    def __str__(self) -> str:
        return ' | '.join(group.__str__() for group in self.groups)

    def __repr__(self) -> str:
        groups = ', '.join(group.__repr__() for group in self.groups)
        return f'{self.__class__.__name__}[{groups}]'


class TypedArrayComprehension(TypedExpression):
    @classmethod
    def from_untyped(cls, context: CodeBlockTypingContext,
                     array_comprehension: ArrayComprehension) -> 'TypedArrayComprehension':
        comprehension_context = context.subscope()
        iterators = TypedIteratorChain.from_untyped(comprehension_context, array_comprehension.iterators)
        element_format = TypedExpression.from_expression(comprehension_context, array_comprehension.element_format)
        return cls(array_comprehension.location, element_format, iterators)

    def __init__(self, location: Location, element_format: TypedExpression, iterators: TypedIteratorChain) -> None:
        super().__init__(location)
        self.element_format = element_format
        self.iterators = iterators

    def type(self) -> DataType:
        return ArrayType(self.element_format.type(), self.iterators.count())

    def __str__(self) -> str:
        return f'[{self.element_format} | {self.iterators}]'

    def __repr__(self) -> str:
        return f'{self.__class__.__name__}[{self.element_format!r}, {self.iterators!r}]'


class LiteralTuple(TypedExpression):
    @classmethod
    def from_tuple_expression(cls, context: CodeBlockTypingContext, tuple_expression: Tuple) -> 'LiteralTuple':
        return cls(tuple_expression.location,
                   [TypedExpression.from_expression(context, expression) for expression in tuple_expression.elements])

    def __init__(self, location: Location, elements: list[TypedExpression]) -> None:
        super().__init__(location)
        self.elements = elements
        self.types = [element.type() for element in elements]
        if len(self.elements) < 2:
            raise CompilerException('Tuple must be contain at least two elements')

    def type(self) -> DataType:
        return ProductType.from_operands(self.types)

    def __str__(self) -> str:
        elements = ', '.join(str(element) for element in self.elements)
        return f'({elements})'

    def __repr__(self) -> str:
        return f'{self.__class__.__name__}[{self.elements!r}]'


class TypedIdentifier(TypedExpression):
    def __init__(self, context: CodeBlockTypingContext, identifier: Identifier) -> None:
        super().__init__(identifier.location)
        self.name = identifier.name
        self._type: DataType = context.get_variable_type(self.location, self.name)

    def type(self) -> DataType:
        return self._type

    def __str__(self) -> str:
        return self.name

    def __repr__(self) -> str:
        return f'{self.__class__.__name__}[{self.name}]'


class TypedArithmeticExpression(TypedExpression, ABC):
    pass


class TypedUnaryArithmeticExpression(TypedArithmeticExpression):
    def __init__(self, context: CodeBlockTypingContext, expression: UnaryArithmeticExpression) -> None:
        super().__init__(expression.location)
        self.operand = TypedExpression.from_expression(context, expression.operand)
        # Type checking
        self.operation = UnaryOperation.from_operator(self.location, expression.operator, self.operand.type())

    def type(self) -> DataType:
        return self.operation.type()

    def __str__(self) -> str:
        return f'({self.operation}{self.operand})'

    def __repr__(self) -> str:
        return f'{self.__class__.__name__}[{self.operation!r}, {self.operand!r}]'


class TypedBinaryArithmeticExpression(TypedArithmeticExpression):
    def __init__(self, context: CodeBlockTypingContext, expression: BinaryArithmeticExpression) -> None:
        super().__init__(expression.location)
        self.left = TypedExpression.from_expression(context, expression.left)
        self.right = TypedExpression.from_expression(context, expression.right)
        # Type checking
        self.operation = BinaryOperation.from_operator(self.location, expression.operator, self.left.type(),
                                                       self.right.type())

    def type(self) -> DataType:
        return self.operation.type()

    def __str__(self) -> str:
        return f'({self.left} {self.operation} {self.right})'

    def __repr__(self) -> str:
        return f'{self.__class__.__name__}[{self.operation!r}, {self.left!r}, {self.right!r}]'


class TypedArraySubscriptExpression(TypedExpression):
    def __init__(self, context: CodeBlockTypingContext, array_subscript_expression: ArraySubscriptExpression) -> None:
        super().__init__(array_subscript_expression.location)
        self.array = TypedExpression.from_expression(context, array_subscript_expression.array)
        index = evaluate(context.namespace, array_subscript_expression.index)
        if not isinstance(index, LiteralChar):
            raise CompilationException(index.location, f'Invalid expression type for array subscript: {index.type()}')
        self.index = index.value
        # Type checking
        array_type = self.array.type()
        if not isinstance(array_type, ArrayType):
            raise CompilationException(self.location, f'Cannot subscript {array_type}')
        # Negative indices start wrap around
        if self.index < 0:
            self.index = array_type.count - self.index
        if self.index < 0 or self.index >= array_type.count:
            raise CompilationException(self.location, 'Array index out of bounds')
        self._type = array_type.base_type

    def type(self) -> DataType:
        return self._type

    def __str__(self) -> str:
        return f'{self.array}[{self.index}]'

    def __repr__(self) -> str:
        return f'{self.__class__.__name__}[{self.array!r}, {self.index!r}]'


class TypedArraySlicingExpression(TypedExpression):
    def __init__(self, context: CodeBlockTypingContext, array_slicing_expression: ArraySlicingExpression) -> None:
        super().__init__(array_slicing_expression.location)
        self.array = TypedExpression.from_expression(context, array_slicing_expression.array)
        start = evaluate(context.namespace, array_slicing_expression.start)
        if not isinstance(start, LiteralChar):
            raise CompilationException(start.location, f'Invalid expression type for array index: {start.type()}')
        self.start = start.value
        stop = evaluate(context.namespace, array_slicing_expression.stop)
        if not isinstance(stop, LiteralChar):
            raise CompilationException(stop.location, f'Invalid expression type for array index: {stop.type()}')
        self.stop = stop.value
        # Type checking
        array_type = self.array.type()
        if not isinstance(array_type, ArrayType):
            raise CompilationException(self.location, f'Cannot slice {array_type}')
        if self.start > self.stop:
            raise CompilationException(self.location, f'Invalid slice (start index must be smaller than end index)')
        if self.start < 0 or self.start >= array_type.count:
            raise CompilationException(self.location, 'Start index out of bounds')
        if self.stop < 0 or self.stop > array_type.count:
            raise CompilationException(self.location, 'Stop index out of bounds')
        self._type = array_type.base_type

    def type(self) -> DataType:
        return ArrayType(self._type, self.stop - self.start)

    def __str__(self) -> str:
        return f'{self.array}[{self.start}:{self.stop}]'

    def __repr__(self) -> str:
        return f'{self.__class__.__name__}[{self.array!r}, {self.start}, {self.stop}]'


class PrintCall(TypeCheckedInstruction):
    def __init__(self, context: CodeBlockTypingContext, procedure_call: ProcedureCall, new_line: bool = False) -> None:
        super().__init__(procedure_call.location)
        self.new_line = new_line
        self.procedure = 'println' if new_line else 'print'
        # Type checking
        self.arguments: list[TypedExpression] = self._type_check_arguments(context, procedure_call.arguments)

    def _type_check_arguments(self, context: CodeBlockTypingContext,
                              arguments: list[Expression]) -> list[TypedExpression]:
        typed_arguments: list[TypedExpression] = []
        for argument in arguments:
            typed_argument = TypedExpression.from_expression(context, argument)
            argument_type = typed_argument.type()
            if not argument_type.is_string():
                message = f'Subroutine {self.procedure!r} only accepts string-like arguments, but found {argument_type}'
                raise CompilationException(argument.location, message)
            typed_arguments.append(typed_argument)
        return typed_arguments

    def __str__(self) -> str:
        arguments = ', '.join(str(argument) for argument in self.arguments)
        return f'{self.procedure}({arguments})'

    def __repr__(self) -> str:
        arguments = ', '.join(repr(argument) for argument in self.arguments)
        return f'{self.__class__.__name__}[println={self.new_line}, {arguments}]'


class InputCall(TypedExpression):
    def __init__(self, procedure_call: ProcedureCall) -> None:
        super().__init__(procedure_call.location)
        # Type checking
        arity = len(procedure_call.arguments)
        if arity > 0:
            raise CompilationException(self.location, f"Subroutine 'input' accepts 0 arguments, but found {arity}")

    def type(self) -> DataType:
        return Types.CHAR

    def __str__(self) -> str:
        return f'input()'

    def __repr__(self) -> str:
        return f'{self.__class__.__name__}'


class LogCall(TypeCheckedInstruction):
    @classmethod
    def from_procedure_call(cls, context: CodeBlockTypingContext, procedure_call: ProcedureCall) -> 'LogCall':
        if len(procedure_call.arguments) != 1:
            message = f"Subroutine 'log' expects a single argument, but found {len(procedure_call.arguments)}"
            raise CompilationException(procedure_call.location, message)
        argument = TypedExpression.from_expression(context, procedure_call.arguments[0])
        return cls(procedure_call.location, argument)

    def __init__(self, location: Location, argument: TypedExpression) -> None:
        super().__init__(location)
        self.argument = argument

    def __str__(self) -> str:
        return f'log({self.argument})'

    def __repr__(self) -> str:
        return f'{self.__class__.__name__}[{self.argument!r}]'


class TypeCheckedProcedureCall(TypeCheckedInstruction):
    def __init__(self, context: CodeBlockTypingContext, procedure_call: ProcedureCall) -> None:
        super().__init__(procedure_call.location)
        self.reference = procedure_call.reference
        self.arguments = self._type_check_arguments(context, procedure_call.arguments)

    def _type_check_arguments(self, context: CodeBlockTypingContext,
                              arguments: list[Expression]) -> list[TypedExpression]:
        expected_types = context.namespace.get_subroutine_signature(self.reference).get_argument_types()
        if len(arguments) != len(expected_types):
            argument_count = f'{len(expected_types)} argument'
            if len(expected_types) > 1:
                argument_count += 's'
            message = f'Subroutine {self.reference} accepts {argument_count}, but found {len(arguments)}'
            raise CompilationException(self.location, message)
        typed_arguments: list[TypedExpression] = []
        for i, argument in enumerate(arguments):
            typed_argument = TypedExpression.from_expression(context, argument)
            if typed_argument.type() != expected_types[i]:
                message = f'Argument {i} of subroutine {self.reference} is expected to be of type' \
                          f' {expected_types[i]}, but found {typed_argument.type()}'
                raise CompilationException(typed_argument.location, message)
            typed_arguments.append(typed_argument)
        return typed_arguments

    def __str__(self) -> str:
        arguments = ', '.join(str(argument) for argument in self.arguments)
        return f'{self.reference}({arguments})'

    def __repr__(self) -> str:
        if self.arguments:
            arguments = ', '.join(repr(argument) for argument in self.arguments)
            return f'{self.__class__.__name__}[{self.reference}, {arguments}]'
        return f'{self.__class__.__name__}[{self.reference}]'


class TypedFunctionCall(TypeCheckedProcedureCall, TypedExpression):
    def __init__(self, context: CodeBlockTypingContext, function_call: FunctionCall) -> None:
        super().__init__(context, function_call)
        self.return_type: DataType = context.namespace.get_subroutine_signature(self.reference).return_type

    def type(self) -> DataType:
        return self.return_type


class TypeCheckedIncrementation(TypeCheckedInstruction):
    def __init__(self, context: CodeBlockTypingContext, incrementation: Incrementation) -> None:
        super().__init__(incrementation.location)
        self.identifier = incrementation.identifier
        variable_type = context.get_variable_type(incrementation.location, self.identifier)
        # Type checking
        if variable_type != Types.CHAR:
            raise CompilationException(self.location, f'Cannot increment {variable_type}')

    def __str__(self) -> str:
        return f'{self.identifier}++'

    def __repr__(self) -> str:
        return f'{self.__class__.__name__}[{self.identifier}]'


class TypeCheckedDecrementation(TypeCheckedInstruction):
    def __init__(self, context: CodeBlockTypingContext, decrementation: Decrementation) -> None:
        super().__init__(decrementation.location)
        self.identifier = decrementation.identifier
        variable_type = context.get_variable_type(decrementation.location, self.identifier)
        # Type checking
        if variable_type != Types.CHAR:
            raise CompilationException(self.location, f'Cannot decrement {variable_type}')

    def __str__(self) -> str:
        return f'{self.identifier}--'

    def __repr__(self) -> str:
        return f'{self.__class__.__name__}[{self.identifier}]'


class TypeCheckedVariableDeclaration(TypeCheckedInstruction):
    @classmethod
    def from_variable_declaration(cls, context: CodeBlockTypingContext,
                                  variable_declaration: VariableDeclaration) -> 'TypeCheckedVariableDeclaration':
        if variable_declaration.value is None:
            value = None
        else:
            value = TypedExpression.from_expression(context, variable_declaration.value)
        target = TypedDeclarationTarget.from_untyped(context, variable_declaration.target, variable_declaration.type)
        return cls(variable_declaration.location, variable_declaration.type, target, value)

    def __init__(self, location: Location, data_type: DataType, target: TypedDeclarationTarget,
                 value: TypedExpression | None) -> None:
        super().__init__(location)
        self.type = data_type
        self.target = target
        self.value = value
        if self.value is not None and self.value.type() != self.type:
            raise CompilationException(self.value.location, f'Expected {self.type} but found {self.value.type()}')

    def __str__(self) -> str:
        if self.value is None:
            return f'let {self.type} {self.target}'
        return f'let {self.type} {self.target} = {self.value}'

    def __repr__(self) -> str:
        return f'{self.__class__.__name__}[{self.target!r}, {self.type!r}, {self.value!r}]'


class TypeCheckedAssignment(TypeCheckedInstruction):
    def __init__(self, context: CodeBlockTypingContext, assignment: Assignment) -> None:
        super().__init__(assignment.location)
        self.target = TypedAssignmentTarget.from_untyped(context, assignment.target)
        self.value = TypedExpression.from_expression(context, assignment.value)
        # Type checking
        if self.target.type() != self.value.type():
            raise CompilationException(self.location, f'Unable to assign {self.value.type()} to {self.target.type()}')

    def __str__(self) -> str:
        return f'{self.target} = {self.value}'

    def __repr__(self) -> str:
        return f'{self.__class__.__name__}[{self.target!r}, {self.value!r}]'


class TypeCheckedLoopStatement(TypeCheckedInstruction):
    def __init__(self, context: CodeBlockTypingContext, loop_statement: LoopStatement) -> None:
        super().__init__(loop_statement.location)
        self.count: TypedExpression = TypedExpression.from_expression(context, loop_statement.count)
        self.body: TypeCheckedInstructionBlock = TypeCheckedInstructionBlock(context, loop_statement.body)
        # Type checking
        if not self.count.type().is_integer():
            raise CompilationException(self.count.location, f'Expected char but found {self.count.type()}')

    def __str__(self) -> str:
        return f'loop ({self.count}) line {self.location.line}'

    def __repr__(self) -> str:
        return f'{self.__class__.__name__}[{self.count!r}, {self.body!r}]'


class TypeCheckedWhileLoopStatement(TypeCheckedInstruction):
    def __init__(self, context: CodeBlockTypingContext, while_loop_statement: WhileLoopStatement) -> None:
        super().__init__(while_loop_statement.location)
        self.test: TypedExpression = TypedExpression.from_expression(context, while_loop_statement.test)
        self.body: TypeCheckedInstructionBlock = TypeCheckedInstructionBlock(context, while_loop_statement.body)
        # Type checking
        if self.test.type() != Types.CHAR:
            raise CompilationException(self.test.location, f'Expected char but found {self.test.type()}')

    def __str__(self) -> str:
        return f'while ({self.test}) line {self.location.line}'

    def __repr__(self) -> str:
        return f'{self.__class__.__name__}[{self.test!r}, {self.body!r}]'


class TypeCheckedDoWhileLoopStatement(TypeCheckedInstruction):
    def __init__(self, context: CodeBlockTypingContext, do_while_loop_statement: DoWhileLoopStatement) -> None:
        super().__init__(do_while_loop_statement.location)
        self.body: TypeCheckedInstructionBlock = TypeCheckedInstructionBlock(context, do_while_loop_statement.body)
        self.test: TypedExpression = TypedExpression.from_expression(context, do_while_loop_statement.test)
        # Type checking
        if self.test.type() != Types.CHAR:
            raise CompilationException(self.test.location, f'Expected char but found {self.test.type()}')

    def __str__(self) -> str:
        return f'do while ({self.test}) line {self.location.line}'

    def __repr__(self) -> str:
        return f'{self.__class__.__name__}[{self.body!r}, {self.test!r}]'


class TypeCheckedForLoopStatement(TypeCheckedInstruction):
    @classmethod
    def from_for_loop_statement(cls, context: CodeBlockTypingContext,
                                for_loop_statement: ForLoopStatement) -> 'TypeCheckedForLoopStatement':
        loop_context = context.subscope()
        iterators = TypedIteratorGroup.from_untyped(loop_context, for_loop_statement.iterators)
        body = TypeCheckedInstructionBlock(loop_context, for_loop_statement.body)
        return cls(for_loop_statement.location, iterators, body)

    def __init__(self, location: Location, iterators: TypedIteratorGroup, body: TypeCheckedInstructionBlock) -> None:
        super().__init__(location)
        self.iterators = iterators
        self.body = body

    def __str__(self) -> str:
        return f'for ({self.iterators}) line {self.location.line}'

    def __repr__(self) -> str:
        return f'{self.__class__.__name__}[{self.iterators!r}, {self.body!r}]'


class TypeCheckedConditionalStatement(TypeCheckedInstruction):
    def __init__(self, context: CodeBlockTypingContext, statement: ConditionalStatement) -> None:
        super().__init__(statement.location)
        self.test = TypedExpression.from_expression(context, statement.test)
        self.if_body = TypeCheckedInstructionBlock(context, statement.if_body)
        if statement.has_else():
            self.else_body = TypeCheckedInstructionBlock(context, statement.else_body)
        else:
            self.else_body = None
        # Type checking
        if self.test.type() != Types.CHAR:
            raise CompilationException(self.test.location, f'Expected char but found {self.test.type()}')

    def has_else(self) -> bool:
        return self.else_body is not None

    def __str__(self) -> str:
        return f'if ({self.test}) line {self.location.line}'

    def __repr__(self) -> str:
        return f'{self.__class__.__name__}[{self.test!r}, {self.if_body!r}, {self.else_body!r}]'


class TypeCheckedReturnInstruction(TypeCheckedInstruction):
    def __init__(self, context: CodeBlockTypingContext, instruction: ReturnInstruction) -> None:
        super().__init__(instruction.location)
        self.value: TypedExpression = TypedExpression.from_expression(context, instruction.value)
        # Type checking
        if context.return_type is None:
            raise CompilationException(self.location, 'Unexpected return statement')
        if self.value.type() != context.return_type:
            message = f'Expected {context.return_type} but found {self.value.type()}'
            raise CompilationException(self.location, message)

    def __str__(self) -> str:
        return f'return {self.value}'

    def __repr__(self) -> str:
        return f'{self.__class__.__name__}[{self.value!r}]'


class TypeCheckedContextSnapshot(TypeCheckedInstruction):
    def __init__(self, context_snapshot: ContextSnapshot) -> None:
        super().__init__(context_snapshot.location)
        self.identifier = context_snapshot.identifier

    def __str__(self) -> str:
        if self.identifier is not None:
            return f'{self.identifier}?'
        return '?'

    def __repr__(self) -> str:
        return f'{self.__class__.__name__}'


__all__ = ['TypeCheckedInstruction', 'TypeCheckedInstructionBlock', 'TypedExpression', 'evaluate', 'LiteralChar',
           'LiteralArray', 'TypedIterator', 'TypedArrayIterator', 'TypedIteratorGroup', 'TypedIteratorChain',
           'TypedArrayComprehension', 'LiteralTuple', 'TypedIdentifier', 'TypedArithmeticExpression',
           'TypedUnaryArithmeticExpression', 'TypedBinaryArithmeticExpression', 'TypedArraySubscriptExpression',
           'TypedArraySlicingExpression', 'PrintCall', 'InputCall', 'LogCall', 'TypeCheckedProcedureCall',
           'TypedFunctionCall', 'TypeCheckedIncrementation', 'TypeCheckedDecrementation',
           'TypeCheckedVariableDeclaration', 'TypeCheckedAssignment', 'TypeCheckedLoopStatement',
           'TypeCheckedWhileLoopStatement', 'TypeCheckedDoWhileLoopStatement', 'TypeCheckedForLoopStatement',
           'TypeCheckedConditionalStatement', 'TypeCheckedReturnInstruction', 'TypeCheckedContextSnapshot']
