from typing import Optional

from exceptions import Location


class Reference:
    @classmethod
    def from_string(cls, string: str) -> 'Reference':
        parts = string.split('::')
        reference = Reference(Location.unknown(), parts[0])
        for part in parts[1:]:
            reference = Reference(Location.unknown(), part, reference)
        return reference

    def __init__(self, location: Location, identifier: str, namespace: Optional['Reference'] = None) -> None:
        self.location = location
        self.identifier = identifier
        self.namespace = namespace

    def __str__(self) -> str:
        if self.namespace is not None:
            return f'{self.namespace}::{self.identifier}'
        return self.identifier


__all__ = ['Reference']
