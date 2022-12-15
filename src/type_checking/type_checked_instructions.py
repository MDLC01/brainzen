from abc import ABC, abstractmethod
from typing import Generator

from type_checking.data_types import *
from exceptions import *
from intermediate_representation.instructions import *
from reference import Reference
from type_checking.operations import *
from type_checking.typed_assignment_targets import *
from type_checking.typed_declaration_target import *
from type_checking.typing_context import *


class TypeCheckedInstruction(ABC):
    @classmethod
    def from_untyped(cls, context: CodeBlockTypingContext, instruction: Instruction) -> 'TypeCheckedInstruction':
        match instruction:
            case InstructionBlock() as instruction_block:
                return TypeCheckedInstructionBlock.from_untyped(context, instruction_block)
            case Expression() as expression:
                return TypedExpression.from_untyped(context, expression)
            case ProcedureCall(reference=Reference(namespace=None, identifier='print')) as procedure_call:
                return PrintCall.from_untyped(context, procedure_call)
            case ProcedureCall(reference=Reference(namespace=None, identifier='println')) as procedure_call:
                return PrintCall.from_untyped(context, procedure_call)
            case ProcedureCall(reference=Reference(namespace=None, identifier='input')) as procedure_call:
                return InputCall.from_untyped(context, procedure_call)
            case ProcedureCall(reference=Reference(namespace=None, identifier='log')) as procedure_call:
                return LogCall.from_untyped(context, procedure_call)
            case ProcedureCall() as procedure_call:
                return TypeCheckedProcedureCall.from_untyped(context, procedure_call)
            case Incrementation() as incrementation:
                return TypeCheckedIncrementation.from_untyped(context, incrementation)
            case Decrementation() as decrementation:
                return TypeCheckedDecrementation.from_untyped(context, decrementation)
            case VariableDeclaration() as variable_declaration:
                return TypeCheckedVariableDeclaration.from_untyped(context, variable_declaration)
            case VariableCreation() as variable_creation:
                return TypeCheckedVariableCreation.from_untyped(context, variable_creation)
            case Assignment() as assignment:
                return TypeCheckedAssignment.from_untyped(context, assignment)
            case LoopStatement() as loop_statement:
                return TypeCheckedLoopStatement.from_untyped(context, loop_statement)
            case WhileLoopStatement() as while_loop_statement:
                return TypeCheckedWhileLoopStatement.from_untyped(context, while_loop_statement)
            case DoWhileLoopStatement() as do_while_loop_statement:
                return TypeCheckedDoWhileLoopStatement.from_untyped(context, do_while_loop_statement)
            case ForLoopStatement() as for_loop_statement:
                return TypeCheckedForLoopStatement.from_untyped(context, for_loop_statement)
            case ConditionalStatement() as conditional_statement:
                return TypeCheckedConditionalStatement.from_untyped(context, conditional_statement)
            case ReturnInstruction() as return_instruction:
                return TypeCheckedReturnInstruction.from_untyped(context, return_instruction)
            case ContextSnapshot() as context_snapshot:
                return TypeCheckedContextSnapshot.from_untyped(context, context_snapshot)
        raise ImpossibleException(f'Unknown instruction type: {instruction.__class__.__name__}')

    def __init__(self, location: Location) -> None:
        self.location = location

    def __repr__(self) -> str:
        return self.__class__.__name__


class TypeCheckedInstructionBlock(TypeCheckedInstruction):
    @classmethod
    def from_untyped(cls, context: CodeBlockTypingContext, instruction_block: InstructionBlock, *,
                     allow_return: bool = False) -> 'TypeCheckedInstructionBlock':
        subscope = context.subscope(allow_return=allow_return)
        instructions = [TypeCheckedInstruction.from_untyped(subscope, instruction)
                        for instruction in instruction_block.instructions]
        return cls(instruction_block.location, instructions)

    def __init__(self, location: Location, instructions: list[TypeCheckedInstruction]) -> None:
        super().__init__(location)
        self.instructions = instructions

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

    @classmethod
    def from_untyped(cls, context: CodeBlockTypingContext, expression: Expression) -> 'TypedExpression':
        match expression:
            case ConstantReference(reference=reference):
                return context.namespace.get_constant_value(reference)
            case Char() as char:
                return LiteralChar.from_untyped(context, char)
            case Array() as array:
                return LiteralArray.from_untyped(context, array)
            case ArrayComprehension() as array_comprehension:
                return TypedArrayComprehension.from_untyped(context, array_comprehension)
            case Range() as range_expression:
                return evaluate(context.namespace, range_expression)
            case Tuple() as tuple_expression:
                return LiteralTuple.from_untyped(context, tuple_expression)
            case Identifier() as identifier:
                return TypedIdentifier.from_untyped(context, identifier)
            case UnaryArithmeticExpression() as unary_arithmetic_expression:
                return TypedUnaryArithmeticExpression.from_untyped(context, unary_arithmetic_expression)
            case BinaryArithmeticExpression() as binary_arithmetic_expression:
                return TypedBinaryArithmeticExpression.from_untyped(context, binary_arithmetic_expression)
            case ArraySubscriptExpression() as array_subscript_expression:
                return TypedArraySubscriptExpression.from_untyped(context, array_subscript_expression)
            case ArraySlicingExpression() as array_slicing_expression:
                return TypedArraySlicingExpression.from_untyped(context, array_slicing_expression)
            case FunctionCall(location=location, reference=Reference(namespace=None, identifier='print')):
                raise CompilationException(location, f"Procedure 'print' does not return anything")
            case FunctionCall(location=location, reference=Reference(namespace=None, identifier='println')):
                raise CompilationException(location, f"Procedure 'println' does not return anything")
            case FunctionCall(reference=Reference(namespace=None, identifier='input')) as function_call:
                return InputCall.from_untyped(context, function_call)
            case FunctionCall(location=location, reference=Reference(namespace=None, identifier='log')):
                raise CompilationException(location, f"Procedure 'log' does not return anything")
            case FunctionCall() as function_call:
                return TypedFunctionCall.from_untyped(context, function_call)
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


def create_python_range(context: NamespaceTypingContext, range_literal: Range) -> range:
    left_bound = evaluate(context, range_literal.left_bound)
    right_bound = evaluate(context, range_literal.right_bound)
    if not isinstance(left_bound, LiteralChar):
        raise CompilationException(left_bound.location, f'Expected {Types.CHAR} evaluable at compile time')
    if not isinstance(right_bound, LiteralChar):
        raise CompilationException(right_bound.location, f'Expected {Types.CHAR} evaluable at compile time')
    step = -1 if left_bound.value > right_bound.value else 1
    start = left_bound.value + step if range_literal.left_excluded else left_bound.value
    end = right_bound.value - step if range_literal.right_excluded else right_bound.value
    return range(start, end + step, step)


def evaluate(context: NamespaceTypingContext, expression: Expression) -> TypedExpression:
    match expression:
        case ConstantReference(reference=reference):
            return context.get_constant_value(reference)
        case Char() as char:
            return LiteralChar.from_char(char)
        case Array(location=location, value=value):
            return LiteralArray(location, [evaluate(context, element) for element in value])
        case Range(location=location) as range_literal:
            return LiteralArray(location,
                                [LiteralChar(location, i) for i in create_python_range(context, range_literal)])
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
            wrapped_value = char.value % 256
            CompilationWarning.add(char.location, WarningType.OUT_OF_RANGE,
                                   f'Literal is outside of the possible range, it will be wrapped to {wrapped_value}',
                                   'Characters must be integers between 0 and 255')
        return cls(char.location, char.value)

    @classmethod
    def from_untyped(cls, context: CodeBlockTypingContext, char: Char) -> 'LiteralChar':
        return cls.from_char(char)

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
    def from_untyped(cls, context: CodeBlockTypingContext, array: Array) -> 'LiteralArray':
        return cls(array.location, [TypedExpression.from_untyped(context, expression) for expression in array.value])

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
    __slots__ = 'location'

    @classmethod
    def from_untyped(cls, context: CodeBlockTypingContext, iterator: Iterator) -> 'TypedIterator':
        if isinstance(iterator, IteratorCounter):
            return TypedIteratorCounter.from_untyped(context, iterator)
        if isinstance(iterator, ArrayIterator):
            if isinstance(iterator.array, Range):
                return TypedRangeIterator.from_untyped(context, iterator)
            return TypedArrayIterator.from_untyped(context, iterator)
        raise ImpossibleException(f'Unknown iterator type: {iterator.__class__.__name__}')

    def __init__(self, location: Location) -> None:
        self.location = location

    @abstractmethod
    def count(self) -> int | None:
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


class TypedIteratorCounter(TypedIterator):
    __slots__ = 'variable', 'initial_value'

    @classmethod
    def from_untyped(cls, context: CodeBlockTypingContext, iterator: IteratorCounter) -> 'TypedIteratorCounter':
        location = iterator.location
        target = TypedDeclarationTarget.from_untyped(context, iterator.target, Types.CHAR)
        if iterator.initial_value is None:
            initial_value = 0
        else:
            evaluated_initial_value = evaluate(context.namespace, iterator.initial_value)
            if not isinstance(evaluated_initial_value, LiteralChar):
                message = f'Expected {Types.CHAR} but found {evaluated_initial_value.type()}'
                raise CompilationException(evaluated_initial_value.location, message)
            initial_value = evaluated_initial_value.value
        return cls(location, target, initial_value)

    def __init__(self, location: Location, target: TypedDeclarationTarget, initial_value: int) -> None:
        super().__init__(location)
        if not isinstance(target, TypedIdentifierDeclarationTarget):
            raise CompilerException(f'char cannot be assigned to {target.__class__.__name__}')
        self.variable = target.identifier
        self.initial_value = initial_value

    def count(self) -> int | None:
        return None

    def type(self) -> DataType:
        return Types.CHAR

    def __str__(self) -> str:
        return f'{self.variable} counts from {self.initial_value}'

    def __repr__(self) -> str:
        return f'{self.__class__.__name__}[{self.variable!r}]'


class TypedRangeIterator(TypedIterator):
    __slots__ = 'variable', 'range'

    @classmethod
    def from_untyped(cls, context: CodeBlockTypingContext, iterator: ArrayIterator) -> 'TypedRangeIterator':
        range_literal = iterator.array
        if not isinstance(range_literal, Range):
            raise CompilerException(f'Expected range literal but got {range_literal.__class__.__name__}')
        location = iterator.location
        target = TypedDeclarationTarget.from_untyped(context, iterator.target, Types.CHAR)
        python_range = create_python_range(context.namespace, range_literal)
        return cls(location, target, python_range)

    def __init__(self, location: Location, target: TypedDeclarationTarget, python_range: range) -> None:
        super().__init__(location)
        if not isinstance(target, TypedIdentifierDeclarationTarget):
            raise CompilerException(f'char cannot be assigned to {target.__class__.__name__}')
        self.variable = target.identifier
        self.range = python_range

    def count(self) -> int | None:
        return len(self.range)

    def type(self) -> DataType:
        return Types.CHAR

    def __str__(self) -> str:
        return f'{self.variable} : [|{self.range.start}, {self.range.stop + self.range.step}|]'

    def __repr__(self) -> str:
        return f'{self.__class__.__name__}[{self.variable!r}, {self.range!r}]'


class TypedArrayIterator(TypedIterator):
    __slots__ = 'target', 'array', 'array_type'

    @classmethod
    def from_untyped(cls, context: CodeBlockTypingContext, array_iterator: ArrayIterator) -> 'TypedArrayIterator':
        array = TypedExpression.from_untyped(context, array_iterator.array)
        array_type = array.type()
        if not isinstance(array_type, ArrayType):
            raise CompilationException(array.location, f'Expected array but found {array_type}')
        target = TypedDeclarationTarget.from_untyped(context, array_iterator.target, array_type.base_type)
        return cls(array_iterator.location, target, array)

    def __init__(self, location: Location, target: TypedDeclarationTarget, array: TypedExpression) -> None:
        super().__init__(location)
        self.target = target
        self.array = array
        array_type = self.array.type()
        if not isinstance(array_type, ArrayType):
            raise CompilerException(f'Expected array but found {array_type}')
        self.array_type = array_type

    def count(self) -> int | None:
        return self.array_type.count

    def type(self) -> DataType:
        return self.array_type.base_type

    def __str__(self) -> str:
        return f'{self.target} : {self.array}'

    def __repr__(self) -> str:
        return f'{self.__class__.__name__}[{self.target!r}, {self.array!r}]'


class TypedIteratorGroup:
    __slots__ = 'location', 'iterators', '_count'

    @classmethod
    def from_untyped(cls, context: CodeBlockTypingContext, iterator_group: IteratorGroup) -> 'TypedIteratorGroup':
        iterators = []
        expected_count = None
        for untyped_iterator in iterator_group.iterators:
            # FIXME: The code bellow type checks because variables are registered when iterators are type checked:
            #  for (x : ["Aa", "Bb", "Cc"], y : x) {...}
            #  This should be invalid because iterators in a group do not interact with each other.
            iterator = TypedIterator.from_untyped(context, untyped_iterator)
            iterator_count = iterator.count()
            if expected_count is None:
                expected_count = iterator_count
            elif iterator_count is not None and iterator_count != expected_count:
                message = f'Expected iterator of size {expected_count} but found iterator of size {iterator.count()}'
                raise CompilationException(iterator.location, message)
            iterators.append(iterator)
        if expected_count is None:
            raise CompilationException(iterator_group.location, 'An iterator group cannot contain only counters')
        return cls(iterator_group.location, iterators, expected_count)

    def __init__(self, location: Location, iterators: list[TypedIterator], count: int) -> None:
        self.location = location
        self.iterators = iterators
        self._count = count

    def count(self) -> int:
        return self._count

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
        element_format = TypedExpression.from_untyped(comprehension_context, array_comprehension.element_format)
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
    def from_untyped(cls, context: CodeBlockTypingContext, tuple_expression: Tuple) -> 'LiteralTuple':
        return cls(tuple_expression.location,
                   [TypedExpression.from_untyped(context, expression) for expression in tuple_expression.elements])

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
    @classmethod
    def from_untyped(cls, context: CodeBlockTypingContext, identifier: Identifier) -> 'TypedIdentifier':
        location = identifier.location
        name = identifier.name
        variable_type = context.get_variable_type(location, name)
        return cls(location, name, variable_type)

    def __init__(self, location: Location, name: str, variable_type: DataType) -> None:
        super().__init__(location)
        self.name = name
        self._type = variable_type

    def type(self) -> DataType:
        return self._type

    def __str__(self) -> str:
        return self.name

    def __repr__(self) -> str:
        return f'{self.__class__.__name__}[{self.name}]'


class TypedArithmeticExpression(TypedExpression, ABC):
    pass


class TypedUnaryArithmeticExpression(TypedArithmeticExpression):
    @classmethod
    def from_untyped(cls, context: CodeBlockTypingContext,
                     expression: UnaryArithmeticExpression) -> 'TypedUnaryArithmeticExpression':
        location = expression.location
        operand = TypedExpression.from_untyped(context, expression.operand)
        operation = UnaryOperation.from_operator(location, expression.operator, operand.type())
        return cls(location, operand, operation)

    def __init__(self, location: Location, operand: TypedExpression, operation: UnaryOperation) -> None:
        super().__init__(location)
        self.operand = operand
        self.operation = operation

    def type(self) -> DataType:
        return self.operation.type()

    def __str__(self) -> str:
        return f'({self.operation}{self.operand})'

    def __repr__(self) -> str:
        return f'{self.__class__.__name__}[{self.operation!r}, {self.operand!r}]'


class TypedBinaryArithmeticExpression(TypedArithmeticExpression):
    @classmethod
    def from_untyped(cls, context: CodeBlockTypingContext,
                     expression: BinaryArithmeticExpression) -> 'TypedBinaryArithmeticExpression':
        location = expression.location
        left = TypedExpression.from_untyped(context, expression.left)
        right = TypedExpression.from_untyped(context, expression.right)
        operation = BinaryOperation.from_operator(location, expression.operator, left.type(), right.type())
        return cls(location, left, right, operation)

    def __init__(self, location: Location, left: TypedExpression, right: TypedExpression,
                 operation: BinaryOperation) -> None:
        super().__init__(location)
        self.left = left
        self.right = right
        self.operation = operation

    def type(self) -> DataType:
        return self.operation.type()

    def __str__(self) -> str:
        return f'({self.left} {self.operation} {self.right})'

    def __repr__(self) -> str:
        return f'{self.__class__.__name__}[{self.operation!r}, {self.left!r}, {self.right!r}]'


class TypedArraySubscriptExpression(TypedExpression):
    @classmethod
    def from_untyped(cls, context: CodeBlockTypingContext,
                     array_subscript_expression: ArraySubscriptExpression) -> 'TypedArraySubscriptExpression':
        location = array_subscript_expression.location
        array = TypedExpression.from_untyped(context, array_subscript_expression.array)
        subscript = evaluate(context.namespace, array_subscript_expression.index)
        if not isinstance(subscript, LiteralChar):
            raise CompilationException(subscript.location, f'Expected {Types.CHAR} but found {subscript.type()}')
        index = subscript.value
        # Assert the array is indeed an array
        array_type = array.type()
        if not isinstance(array_type, ArrayType):
            raise CompilationException(location, f'Cannot subscript {array_type}')
        # Negative indices start at the end
        if index < 0:
            index = array_type.count - index
        if index < 0 or index >= array_type.count:
            raise CompilationException(location, f'Index out of bounds for {array_type}')
        base_type = array_type.base_type
        return cls(location, array, index, base_type)

    def __init__(self, location: Location, array: TypedExpression, index: int, base_type: DataType) -> None:
        super().__init__(location)
        self.array = array
        self.index = index
        self.base_type = base_type

    def type(self) -> DataType:
        return self.base_type

    def __str__(self) -> str:
        return f'{self.array}[{self.index}]'

    def __repr__(self) -> str:
        return f'{self.__class__.__name__}[{self.array!r}, {self.index!r}]'


class TypedArraySlicingExpression(TypedExpression):
    @classmethod
    def from_untyped(cls, context: CodeBlockTypingContext,
                     array_slicing_expression: ArraySlicingExpression) -> 'TypedArraySlicingExpression':
        location = array_slicing_expression.location
        array = TypedExpression.from_untyped(context, array_slicing_expression.array)
        slice_start = evaluate(context.namespace, array_slicing_expression.start)
        if not isinstance(slice_start, LiteralChar):
            raise CompilationException(slice_start.location, f'Expected {Types.CHAR} but found {slice_start.type()}')
        start = slice_start.value
        slice_stop = evaluate(context.namespace, array_slicing_expression.stop)
        if not isinstance(slice_stop, LiteralChar):
            raise CompilationException(slice_stop.location, f'Expected {Types.CHAR} but found {slice_stop.type()}')
        stop = slice_stop.value
        # Assert the array is in fact an array
        array_type = array.type()
        if not isinstance(array_type, ArrayType):
            raise CompilationException(location, f'Cannot slice {array_type}')
        # Compute positive start (negative indices start at the end)
        if start < 0:
            start = array_type.count - start
        if start < 0 or start >= array_type.count:
            raise CompilationException(location, f'Index out of bounds for {array_type}')
        # Compute positive stop (negative indices start at the end)
        if stop < 0:
            stop = array_type.count - stop
        if stop < 0 or stop > array_type.count:
            raise CompilationException(location, f'Index out of bounds for {array_type}')
        # Assert the slice is valid
        if start > stop:
            raise CompilationException(location, 'Invalid slice (start index must be smaller than stop index)')
        base_type = array_type.base_type
        return cls(location, array, start, stop, base_type)

    def __init__(self, location: Location, array: TypedExpression, start: int, stop: int, base_type: DataType) -> None:
        super().__init__(location)
        self.array = array
        self.start = start
        self.stop = stop
        self.base_type = base_type

    def type(self) -> DataType:
        return ArrayType(self.base_type, self.stop - self.start)

    def __str__(self) -> str:
        return f'{self.array}[{self.start}:{self.stop}]'

    def __repr__(self) -> str:
        return f'{self.__class__.__name__}[{self.array!r}, {self.start}, {self.stop}]'


class TypeCheckedProcedureCall(TypeCheckedInstruction):
    @classmethod
    def from_untyped(cls, context: CodeBlockTypingContext, procedure_call: ProcedureCall) -> 'TypeCheckedProcedureCall':
        location = procedure_call.location
        reference = procedure_call.reference
        arguments = procedure_call.arguments
        # Get subroutine signature
        signature = context.namespace.get_subroutine_signature(reference)
        if signature.return_type is not None:
            CompilationWarning.add(location, WarningType.IGNORED_RESULT,
                                   f'Return value of function {reference!r} is ignored')
        expected_types = signature.get_argument_types()
        # Test arity
        if len(arguments) != len(expected_types):
            argument_count = f'{len(expected_types)} argument'
            if len(expected_types) > 1:
                argument_count += 's'
            message = f'Procedure {reference!r} accepts {argument_count}, but found {len(arguments)}'
            raise CompilationException(location, message)
        # Test if types match
        typed_arguments = []
        for i, argument in enumerate(arguments):
            typed_argument = TypedExpression.from_untyped(context, argument)
            if typed_argument.type() != expected_types[i]:
                message = f'Expected {expected_types[i]} but found {typed_argument.type()}'
                raise CompilationException(typed_argument.location, message)
            typed_arguments.append(typed_argument)
        return cls(location, reference, typed_arguments)

    def __init__(self, location: Location, reference: Reference, arguments: list[TypedExpression]) -> None:
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


class TypedFunctionCall(TypedExpression):
    @classmethod
    def from_untyped(cls, context: CodeBlockTypingContext, function_call: FunctionCall) -> 'TypedFunctionCall':
        location = function_call.location
        reference = function_call.reference
        arguments = function_call.arguments
        # Get subroutine signature
        signature = context.namespace.get_subroutine_signature(reference)
        expected_types = signature.get_argument_types()
        # Test arity
        if len(arguments) != len(expected_types):
            argument_count = f'{len(expected_types)} argument'
            if len(expected_types) > 1:
                argument_count += 's'
            message = f'Function {reference!r} accepts {argument_count}, but found {len(arguments)}'
            raise CompilationException(location, message)
        # Test if types match
        typed_arguments = []
        for i, argument in enumerate(arguments):
            typed_argument = TypedExpression.from_untyped(context, argument)
            if typed_argument.type() != expected_types[i]:
                message = f'Expected {expected_types[i]} but found {typed_argument.type()}'
                raise CompilationException(typed_argument.location, message)
            typed_arguments.append(typed_argument)
        return cls(location, reference, typed_arguments, signature.return_type)

    def __init__(self, location: Location, reference: Reference, arguments: list[TypedExpression],
                 return_type: DataType) -> None:
        super().__init__(location)
        self.reference = reference
        self.arguments = arguments
        self.return_type = return_type

    def type(self) -> DataType:
        return self.return_type

    def __str__(self) -> str:
        arguments = ', '.join(str(argument) for argument in self.arguments)
        return f'{self.reference}({arguments})'

    def __repr__(self) -> str:
        if self.arguments:
            arguments = ', '.join(repr(argument) for argument in self.arguments)
            return f'{self.__class__.__name__}[{self.reference}, {arguments}]'
        return f'{self.__class__.__name__}[{self.reference}]'


class PrintCall(TypeCheckedProcedureCall):
    @classmethod
    def from_untyped(cls, context: CodeBlockTypingContext, procedure_call: ProcedureCall) -> 'PrintCall':
        procedure = procedure_call.reference
        new_line = procedure.identifier.endswith('ln')
        arguments = []
        for argument in procedure_call.arguments:
            typed_argument = TypedExpression.from_untyped(context, argument)
            argument_type = typed_argument.type()
            if not argument_type.is_string():
                raise CompilationException(argument.location, f'Expected string-like but found {argument_type}')
            arguments.append(typed_argument)
        return cls(procedure_call.location, procedure, arguments, new_line)

    def __init__(self, location: Location, procedure: Reference, arguments: list[TypedExpression],
                 new_line: bool = False) -> None:
        super().__init__(location, procedure, arguments)
        self.new_line = new_line

    def __repr__(self) -> str:
        if self.arguments:
            arguments = ', '.join(repr(argument) for argument in self.arguments)
            return f'{self.__class__.__name__}[{self.reference}, {arguments}, new_line={self.new_line}]'
        return f'{self.__class__.__name__}[{self.reference}, new_line={self.new_line}]'


class InputCall(TypedFunctionCall):
    @classmethod
    def from_untyped(cls, context: CodeBlockTypingContext, function_call: FunctionCall) -> 'InputCall':
        location = function_call.location
        reference = function_call.reference
        # Test arity
        if len(function_call.arguments) != 0:
            message = f'Function {reference!r} accepts no argument, but found {len(function_call.arguments)}'
            raise CompilationException(location, message)
        return cls(location, reference)

    def __init__(self, location: Location, reference: Reference) -> None:
        super().__init__(location, reference, [], Types.CHAR)

    def type(self) -> DataType:
        return Types.CHAR

    def __repr__(self) -> str:
        return f'{self.__class__.__name__}'


class LogCall(TypeCheckedProcedureCall):
    @classmethod
    def from_untyped(cls, context: CodeBlockTypingContext, procedure_call: ProcedureCall) -> 'LogCall':
        location = procedure_call.location
        reference = procedure_call.reference
        # Test arity
        if len(procedure_call.arguments) != 1:
            message = f'Procedure {reference!r} accepts a single argument, but found {len(procedure_call.arguments)}'
            raise CompilationException(location, message)
        # Type check argument
        argument = TypedExpression.from_untyped(context, procedure_call.arguments[0])
        return cls(location, reference, argument)

    def __init__(self, location: Location, reference: Reference, argument: TypedExpression) -> None:
        super().__init__(location, reference, [argument])
        self.argument = argument

    def __repr__(self) -> str:
        return f'{self.__class__.__name__}[{self.argument!r}]'


class TypeCheckedIncrementation(TypeCheckedInstruction):
    @classmethod
    def from_untyped(cls, context: CodeBlockTypingContext,
                     incrementation: Incrementation) -> 'TypeCheckedIncrementation':
        location = incrementation.location
        identifier = incrementation.identifier
        variable_type = context.get_variable_type(incrementation.location, incrementation.identifier)
        if variable_type != Types.CHAR:
            raise CompilationException(location, f'Cannot increment {variable_type}')
        return cls(location, identifier)

    def __init__(self, location: Location, identifier: str) -> None:
        super().__init__(location)
        self.identifier = identifier

    def __str__(self) -> str:
        return f'{self.identifier}++'

    def __repr__(self) -> str:
        return f'{self.__class__.__name__}[{self.identifier}]'


class TypeCheckedDecrementation(TypeCheckedInstruction):
    @classmethod
    def from_untyped(cls, context: CodeBlockTypingContext,
                     decrementation: Decrementation) -> 'TypeCheckedDecrementation':
        location = decrementation.location
        identifier = decrementation.identifier
        variable_type = context.get_variable_type(decrementation.location, decrementation.identifier)
        if variable_type != Types.CHAR:
            raise CompilationException(location, f'Cannot decrement {variable_type}')
        return cls(location, identifier)

    def __init__(self, location: Location, identifier: str) -> None:
        super().__init__(location)
        self.identifier = identifier

    def __str__(self) -> str:
        return f'{self.identifier}--'

    def __repr__(self) -> str:
        return f'{self.__class__.__name__}[{self.identifier}]'


class TypeCheckedVariableDeclaration(TypeCheckedInstruction):
    @classmethod
    def from_untyped(cls, context: CodeBlockTypingContext,
                     variable_declaration: VariableDeclaration) -> 'TypeCheckedVariableDeclaration':
        variable_type = context.namespace.build_type(variable_declaration.type)
        target = TypedDeclarationTarget.from_untyped(context, variable_declaration.target, variable_type)
        return cls(variable_declaration.location, target)

    def __init__(self, location: Location, target: TypedDeclarationTarget) -> None:
        super().__init__(location)
        self.target = target

    def type(self) -> DataType:
        return self.target.type

    def __str__(self) -> str:
        return f'let {self.target}: {self.type()}'

    def __repr__(self) -> str:
        return f'{self.__class__.__name__}[{self.target!r}, {self.type()!r}]'


class TypeCheckedVariableCreation(TypeCheckedInstruction):
    @classmethod
    def from_untyped(cls, context: CodeBlockTypingContext,
                     variable_declaration: VariableCreation) -> 'TypeCheckedVariableCreation':
        value = TypedExpression.from_untyped(context, variable_declaration.value)
        target = TypedDeclarationTarget.from_untyped(context, variable_declaration.target, value.type())
        return cls(variable_declaration.location, target, value)

    def __init__(self, location: Location, target: TypedDeclarationTarget, value: TypedExpression | None) -> None:
        super().__init__(location)
        self.target = target
        self.value = value

    def type(self) -> DataType:
        return self.value.type()

    def __str__(self) -> str:
        return f'let {self.target} = {self.value}'

    def __repr__(self) -> str:
        return f'{self.__class__.__name__}[{self.target!r}, {self.value!r}]'


class TypeCheckedAssignment(TypeCheckedInstruction):
    @classmethod
    def from_untyped(cls, context: CodeBlockTypingContext, assignment: Assignment) -> 'TypeCheckedAssignment':
        location = assignment.location
        target = TypedAssignmentTarget.from_untyped(context, assignment.target)
        value = TypedExpression.from_untyped(context, assignment.value)
        if target.type() != value.type():
            raise CompilationException(location, f'Unable to assign {value.type()} to {target.type()}')
        return cls(location, target, value)

    def __init__(self, location: Location, target: TypedAssignmentTarget, value: TypedExpression) -> None:
        super().__init__(location)
        self.target = target
        self.value = value

    def __str__(self) -> str:
        return f'{self.target} = {self.value}'

    def __repr__(self) -> str:
        return f'{self.__class__.__name__}[{self.target!r}, {self.value!r}]'


class TypeCheckedLoopStatement(TypeCheckedInstruction):
    @classmethod
    def from_untyped(cls, context: CodeBlockTypingContext, loop_statement: LoopStatement) -> 'TypeCheckedLoopStatement':
        count = TypedExpression.from_untyped(context, loop_statement.count)
        body = TypeCheckedInstructionBlock.from_untyped(context, loop_statement.body)
        if not count.type().is_integer():
            raise CompilationException(count.location, f'Expected {Types.CHAR} but found {count.type()}')
        return cls(loop_statement.location, count, body)

    def __init__(self, location: Location, count: TypedExpression, body: TypeCheckedInstructionBlock) -> None:
        super().__init__(location)
        self.count = count
        self.body = body

    def __str__(self) -> str:
        return f'loop ({self.count}) line {self.location.line}'

    def __repr__(self) -> str:
        return f'{self.__class__.__name__}[{self.count!r}, {self.body!r}]'


class TypeCheckedWhileLoopStatement(TypeCheckedInstruction):
    @classmethod
    def from_untyped(cls, context: CodeBlockTypingContext,
                     while_loop_statement: WhileLoopStatement) -> 'TypeCheckedWhileLoopStatement':
        test = TypedExpression.from_untyped(context, while_loop_statement.test)
        if test.type() != Types.CHAR:
            raise CompilationException(test.location, f'Expected {Types.CHAR} but found {test.type()}')
        body = TypeCheckedInstructionBlock.from_untyped(context, while_loop_statement.body)
        return cls(while_loop_statement.location, test, body)

    def __init__(self, location: Location, test: TypedExpression, body: TypeCheckedInstructionBlock) -> None:
        super().__init__(location)
        self.test = test
        self.body = body

    def __str__(self) -> str:
        return f'while ({self.test}) line {self.location.line}'

    def __repr__(self) -> str:
        return f'{self.__class__.__name__}[{self.test!r}, {self.body!r}]'


class TypeCheckedDoWhileLoopStatement(TypeCheckedInstruction):
    @classmethod
    def from_untyped(cls, context: CodeBlockTypingContext,
                     do_while_loop_statement: DoWhileLoopStatement) -> 'TypeCheckedDoWhileLoopStatement':
        body = TypeCheckedInstructionBlock.from_untyped(context, do_while_loop_statement.body)
        test = TypedExpression.from_untyped(context, do_while_loop_statement.test)
        if test.type() != Types.CHAR:
            raise CompilationException(test.location, f'Expected {Types.CHAR} but found {test.type()}')
        return cls(do_while_loop_statement.location, body, test)

    def __init__(self, location: Location, body: TypeCheckedInstructionBlock, test: TypedExpression) -> None:
        super().__init__(location)
        self.body = body
        self.test = test

    def __str__(self) -> str:
        return f'do while ({self.test}) line {self.location.line}'

    def __repr__(self) -> str:
        return f'{self.__class__.__name__}[{self.body!r}, {self.test!r}]'


class TypeCheckedForLoopStatement(TypeCheckedInstruction):
    @classmethod
    def from_untyped(cls, context: CodeBlockTypingContext,
                     for_loop_statement: ForLoopStatement) -> 'TypeCheckedForLoopStatement':
        loop_context = context.subscope()
        iterators = TypedIteratorGroup.from_untyped(loop_context, for_loop_statement.iterators)
        body = TypeCheckedInstructionBlock.from_untyped(loop_context, for_loop_statement.body)
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
    @classmethod
    def from_untyped(cls, context: CodeBlockTypingContext,
                     conditional_statement: ConditionalStatement) -> 'TypeCheckedConditionalStatement':
        location = conditional_statement.location
        test = TypedExpression.from_untyped(context, conditional_statement.test)
        if test.type() != Types.CHAR:
            raise CompilationException(test.location, f'Expected {Types.CHAR} but found {test.type()}')
        if_body = TypeCheckedInstructionBlock.from_untyped(context, conditional_statement.if_body)
        if conditional_statement.has_else():
            else_body = TypeCheckedInstructionBlock.from_untyped(context, conditional_statement.else_body)
            return cls(location, test, if_body, else_body)
        return cls(location, test, if_body)

    def __init__(self, location: Location, test: TypedExpression, if_body: TypeCheckedInstructionBlock,
                 else_body: TypeCheckedInstructionBlock | None = None) -> None:
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


class TypeCheckedReturnInstruction(TypeCheckedInstruction):
    @classmethod
    def from_untyped(cls, context: CodeBlockTypingContext,
                     return_instruction: ReturnInstruction) -> 'TypeCheckedReturnInstruction':
        location = return_instruction.location
        value = TypedExpression.from_untyped(context, return_instruction.value)
        if context.return_type is None:
            raise CompilationException(location, 'Unexpected return statement')
        if value.type() != context.return_type:
            raise CompilationException(value.location, f'Expected {context.return_type} but found {value.type()}')
        return cls(location, value)

    def __init__(self, location: Location, value: TypedExpression) -> None:
        super().__init__(location)
        self.value = value

    def __str__(self) -> str:
        return f'return {self.value}'

    def __repr__(self) -> str:
        return f'{self.__class__.__name__}[{self.value!r}]'


class TypeCheckedContextSnapshot(TypeCheckedInstruction):
    @classmethod
    def from_untyped(cls, context: CodeBlockTypingContext,
                     context_snapshot: ContextSnapshot) -> 'TypeCheckedContextSnapshot':
        location = context_snapshot.location
        identifier = context_snapshot.identifier
        if identifier not in context:
            raise CompilationException(location, f'Unknown identifier: {identifier!r}')
        return cls(location, identifier)

    def __init__(self, location: Location, identifier: str) -> None:
        super().__init__(location)
        self.identifier = identifier

    def __str__(self) -> str:
        if self.identifier is not None:
            return f'{self.identifier}?'
        return '?'

    def __repr__(self) -> str:
        return f'{self.__class__.__name__}'


__all__ = ['TypeCheckedInstruction', 'TypeCheckedInstructionBlock', 'TypedExpression', 'evaluate', 'LiteralChar',
           'LiteralArray', 'TypedIterator', 'TypedIteratorCounter', 'TypedRangeIterator', 'TypedArrayIterator',
           'TypedIteratorGroup', 'TypedIteratorChain', 'TypedArrayComprehension', 'LiteralTuple', 'TypedIdentifier',
           'TypedArithmeticExpression', 'TypedUnaryArithmeticExpression', 'TypedBinaryArithmeticExpression',
           'TypedArraySubscriptExpression', 'TypedArraySlicingExpression', 'PrintCall', 'InputCall', 'LogCall',
           'TypeCheckedProcedureCall', 'TypedFunctionCall', 'TypeCheckedIncrementation', 'TypeCheckedDecrementation',
           'TypeCheckedVariableDeclaration', 'TypeCheckedVariableCreation', 'TypeCheckedAssignment',
           'TypeCheckedLoopStatement', 'TypeCheckedWhileLoopStatement', 'TypeCheckedDoWhileLoopStatement',
           'TypeCheckedForLoopStatement', 'TypeCheckedConditionalStatement', 'TypeCheckedReturnInstruction',
           'TypeCheckedContextSnapshot']
