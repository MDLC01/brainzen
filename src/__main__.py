import sys
import time

from exceptions import *
from generation.generator import CommentLevel, generate_program
from intermediate_representation import ASTGenerator
from reference import Reference
from tokenization import Tokenizer
from type_checking.type_checker import TypeCheckedNamespace


def compile_source_code(source_code: str, file_name: str, *, main_procedure: str = 'main',
                        verbose_level: int = CommentLevel.BZ_CODE) -> str:
    tokens = Tokenizer(file_name, source_code).tokenize()
    ast = ASTGenerator(Location.in_file(file_name), tokens).generate()
    typed_ast = TypeCheckedNamespace.from_file(ast)
    main_procedure_reference = Reference.from_string(main_procedure)
    return generate_program(typed_ast, main_procedure_reference, comment_level=verbose_level)


def main(argv: list[str]) -> int:
    if len(argv) < 2:
        message = f'Usage: {argv[0]} <source file> [destination file [main procedure name [verbose level]]]'
        print(message, file=sys.stderr)
        sys.exit(1)

    source_file_name = argv[1]
    destination_file_name = argv[2] if len(argv) > 2 else 'out.bf'

    kwargs = {}
    if len(argv) > 3:
        kwargs['main_procedure'] = argv[3]
    if len(argv) > 4:
        kwargs['verbose_level'] = int(argv[4])

    with open(source_file_name, 'r', encoding='UTF-8') as file:
        code = file.read()

    start_time = time.time()
    try:
        program = compile_source_code(code, source_file_name, **kwargs)
    except CompilationException as e:
        print(e, file=sys.stderr)
        e.location.print_position(code)
        return 1
    end_time = time.time()

    with open(destination_file_name, 'w', encoding='UTF-8') as file:
        file.write(program)

    CompilationWarning.print_warnings(code)

    print(f'Compiled {source_file_name!r} in {end_time - start_time:0.2f} s')

    return 0


if __name__ == '__main__':
    sys.exit(main(sys.argv))
