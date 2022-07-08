import sys
import time
from argparse import ArgumentParser

from exceptions import *
from generation import CommentLevel, generate_program
from intermediate_representation import ASTGenerator
from reference import Reference
from tokenization import Tokenizer
from type_checking.type_checker import TypeCheckedNamespace


def compile_source_code(source_code: str, source_file_name: str, output_file_name: str, main_procedure: str,
                        verbose_level: int) -> float:
    start_time = time.time()
    tokens = Tokenizer(source_file_name, source_code).tokenize()
    ast = ASTGenerator(Location.in_file(source_file_name), tokens).generate()
    typed_ast = TypeCheckedNamespace.from_file(ast)
    main_procedure_reference = Reference.from_string(main_procedure)
    brainfuck_code = generate_program(typed_ast, main_procedure_reference, comment_level=verbose_level)
    end_time = time.time()
    with open(output_file_name, 'w', encoding='UTF-8') as file:
        brainfuck_code.write_file(file)
    return end_time - start_time


def main(argv: list[str]) -> int:
    parser = ArgumentParser(description='Compile a Brainzen program.')
    parser.add_argument('source_file_name', help='the Brainzen file to compile')
    parser.add_argument('-o', '--out', default='out.bf', dest='output_file_name', help='the path to the output file')
    parser.add_argument('-m', '--main', default='main', dest='main_procedure_identifier',
                        help='the identifier of the procedure to compile')
    parser.add_argument('-v', '--verbose', type=int, default=CommentLevel.BZ_CODE, dest='verbose_level',
                        help='the verbose level for comments in generated Brainfuck file')
    parser.add_argument('-s', '--silent', dest='silent', default=False, action='store_true',
                        help='redirect stdout to /dev/null')
    parser.add_argument('-w', '--warn', type=WarningType.from_string, default=WarningType.default(),
                        dest='allowed_warning_types', help='a list of allowed warnings (comma separated)')

    arguments = parser.parse_args(argv[1:])

    with open(arguments.source_file_name, 'r', encoding='UTF-8') as file:
        code = file.read()

    try:
        compilation_time = compile_source_code(code, arguments.source_file_name, arguments.output_file_name,
                                               arguments.main_procedure_identifier, arguments.verbose_level)
    except CompilationException as e:
        print(e, file=sys.stderr)
        e.location.print_position(code)
        return 1

    CompilationWarning.print_warnings(code, arguments.allowed_warning_types)

    if not arguments.silent:
        print(f'Compiled {arguments.source_file_name!r} in {compilation_time:0.2f} s')

    return 0


if __name__ == '__main__':
    sys.exit(main(sys.argv))
