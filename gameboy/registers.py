from typing import Union


class Registers:
    def __init__(self):
        self.A: int = 0x00
        self.B: int = 0x00
        self.C: int = 0x00
        self.D: int = 0x00
        self.E: int = 0x00
        self.F: int = 0x00
        self.H: int = 0x00
        self.L: int = 0x00

        self.PC: int = 0x0000
        self.SP: int = 0x0000

    @property
    def AF(self) -> int:
        return (self.A << 8) | self.F

    @AF.setter
    def AF(self, value: int):
        self.A = (value & 0xff00) >> 8
        self.F = value & 0x00ff

    @property
    def BC(self) -> int:
        return (self.B << 8) | self.C

    @BC.setter
    def BC(self, value: int):
        self.B = (value & 0xff00) >> 8
        self.C = value & 0x00ff

    @property
    def DE(self) -> int:
        return (self.D << 8) | self.E

    @DE.setter
    def DE(self, value: int):
        self.D = (value & 0xff00) >> 8
        self.E = value & 0x00ff

    @property
    def HL(self) -> int:
        return (self.H << 8) | self.L

    @HL.setter
    def HL(self, value: int):
        self.H = (value & 0xff00) >> 8
        self.L = value & 0x00ff

    @property
    def zero_flag(self) -> bool:
        return (self.F & 0b10000000) != 0

    @zero_flag.setter
    def zero_flag(self, bit: bool):
        if bit:
            self.F |= 0b10000000
        else:
            self.F &= 0b01111111

    @property
    def subtract_flag(self) -> bool:
        return (self.F & 0b01000000) != 0

    @subtract_flag.setter
    def subtract_flag(self, bit: bool):
        if bit:
            self.F |= 0b01000000
        else:
            self.F &= 0b10111111

    @property
    def half_carry_flag(self) -> bool:
        return (self.F & 0b00100000) != 0

    @half_carry_flag.setter
    def half_carry_flag(self, bit: bool):
        if bit:
            self.F |= 0b00100000
        else:
            self.F &= 0b11011111

    @property
    def carry_flag(self) -> bool:
        return (self.F & 0b00010000) != 0

    @carry_flag.setter
    def carry_flag(self, bit: bool):
        if bit:
            self.F |= 0b00010000
        else:
            self.F &= 0b11101111

    def flags(self,
              Z: Union[bool, int] = None, N: Union[bool, int] = None,
              H: Union[bool, int] = None, C: Union[bool, int] = None):
        if Z is not None:
            self.F = (self.F & 0b01110000) | (Z << 7)
        if N is not None:
            self.F = (self.F & 0b10110000) | (N << 6)
        if H is not None:
            self.F = (self.F & 0b11010000) | (H << 5)
        if C is not None:
            self.F = (self.F & 0b11100000) | (C << 4)
