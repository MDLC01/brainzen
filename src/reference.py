from typing import Optional, Self

from exceptions import Location


class Reference:
    @classmethod
    def from_string(cls, string: str) -> Self:
        parts = string.split('::')
        reference = cls(Location.unknown(), parts[0])
        for part in parts[1:]:
            reference = cls(Location.unknown(), part, reference)
        return reference

    def __init__(self, location: Location, identifier: str, namespace: Optional['Reference'] = None) -> None:
        self.location = location
        self.identifier = identifier
        self.namespace = namespace

    def __str__(self) -> str:
        if self.namespace is not None:
            return f'{self.namespace}::{self.identifier}'
        return self.identifier

    def __repr__(self) -> str:
        return f"'{self}'"


__all__ = ['Reference']
