from typing import TextIO


class BrainfuckCode:
    __slots__ = 'instructions'

    def __init__(self) -> None:
        self.instructions: list[tuple[str, int]] = []

    def remove_up_to(self, count: int, command: str) -> int:
        """
        Remove up to `count` times the passed command at the end of the Brainfuck code
        and return the number of commands that were NOT where removed.
        """
        if len(self.instructions) == 0:
            return count
        last_command, last_count = self.instructions[-1]
        if last_command == command:
            if last_count < count:
                self.instructions.pop()
                return count - last_count
            if last_count >= count:
                self.instructions[-1] = command, last_count - count
            if self.instructions[-1][1] == 0:
                self.instructions.pop()
            return 0
        return count

    def append_raw(self, code: str, count: int = 1) -> None:
        """
        Append the passed Brainfuck code without performing any simplification.
        This method is faster than append, but it generates less optimised code.
        """
        self.instructions.append((code, count))

    def append(self, command: str, count: int = 1) -> None:
        """Append the passed Brainfuck command, optionally simplifying the resulted code."""
        if command and count:
            if len(self.instructions) > 0 and command == self.instructions[-1][0]:
                self.instructions[-1] = command, self.instructions[-1][1] + count
            else:
                self.append_raw(command, count)

    def extend(self, brainfuck_code: 'BrainfuckCode') -> None:
        """Extend this Brainfuck code with the passed Brainfuck code."""
        for i, (command, count) in enumerate(brainfuck_code.instructions):
            if i == 0:
                self.append(command, count)
            else:
                self.append_raw(command, count)

    def comment(self, comment: str, *, prefix: str = '\t', end: str = '\n') -> None:
        """
        Add a comment to the Brainfuck code.
        Special characters in the comment will be replaced
        """
        serialized_comment = (comment
                              .replace('<', '(less than)')
                              .replace('>', '(greater than)')
                              .replace('+', '(plus)')
                              .replace('-', '(minus)')
                              .replace('[', '(')
                              .replace(']', ')')
                              .replace('.', '(period)')
                              .replace(',', ';')
                              .replace('#', '(hash)'))
        self.append_raw(f'{prefix}{serialized_comment}{end}')

    def write_file(self, file: TextIO) -> None:
        for command, count in self.instructions:
            file.write(command * count)
        file.write('\n')


__all__ = ['BrainfuckCode']
