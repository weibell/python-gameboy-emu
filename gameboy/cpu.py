from typing import Callable, Dict, Tuple, Optional

from gameboy.memory import Memory
from gameboy.registers import Registers


class CPU:
    """
    Opcodes and notation source based on https://gbdev.io/gb-opcodes/optables/
    """

    def __init__(self, memory: Memory):
        self.mem = memory
        self.reg = Registers()
        self.opcode_table = self._build_opcode_table()

        self.register_order = ["B", "C", "D", "E", "H", "L", "HL", "A"]
        self.opcode_handlers_0x80_to_0xbf = [
            # (handler, mnemonic prefix)
            (self._ADD, "ADD A, "), (self._ADC, "ADC A, "),
            (self._SUB, "SUB "), (self._SBC, "SBC A, "),
            (self._AND, "AND "), (self._XOR, "XOR "),
            (self._OR, "OR "), (self._CP, "CP "),
        ]
        self.opcode_handlers_0xcb00_to_0xcb3f = [
            # (handler, mnemonic prefix)
            (self._RLC, "RLC "), (self._RRC, "RRC "),
            (self._RL, "RL "), (self._RR, "RR "),
            (self._SLA, "SLA "), (self._SRA, "SRA "),
            (self._SWAP, "SWAP "), (self._SRL, "SRL "),
        ]

        self.instruction = b'' # TODO type
        self.opcode = 0x00
        self.mnemonic = ""

        self.interrupts_enabled = False
        self.is_halted = False
        self.is_stopped = False

    def step(self) -> int:
        self.opcode = self.mem[self.reg.PC]

        if 0x40 <= self.opcode <= 0xbf and self.opcode != 0x76:
            m_cycles = self._handle_0x40_to_0xbf()
        else:
            opcode_handler, instruction_length, m_cycles, self.mnemonic = self.opcode_table[self.opcode]
            self.instruction = self.mem[self.reg.PC:self.reg.PC + instruction_length]
            self.reg.PC = (self.reg.PC + instruction_length) & 0xffff
            additional_cycles = opcode_handler()
            if additional_cycles is not None:
                m_cycles += additional_cycles

        return m_cycles

    @property
    def last_pc(self):
        return self.reg.PC - len(self.last_instruction)

    @property
    def last_instruction(self) -> bytes:
        return self.instruction

    @property
    def last_mnemonic(self) -> str:
        mnemonic = self.mnemonic
        for placeholder in ["d8", "d16", "a8", "a16", "r8"]:
            if placeholder in mnemonic:
                value = getattr(self, placeholder)
                return f"{mnemonic.replace(placeholder, hex(value))}"
        return mnemonic

    def _handle_0x40_to_0xbf(self) -> int:
        source: str = self.register_order[self.opcode & 0b111]
        is_source_HL = source == "HL"

        if self.opcode <= 0x7f:  # 0x40 to 0x7f: LD
            target: str = self.register_order[(self.opcode >> 3) & 0b111]
            is_target_HL = target == "HL"

            if target == source:
                pass
            elif target == "HL":
                self.mem[self.reg.HL] = getattr(self.reg, source)
                self.mnemonic = f"LD (HL), {source}"
            elif source == "HL":
                setattr(self.reg, target, self.mem[self.reg.HL])
                self.mnemonic = f"LD {target}, (HL)"
            else:
                setattr(self.reg, target, getattr(self.reg, source))

            m_cycles = 2 if is_target_HL or is_source_HL else 1
            self.mnemonic = f"LD {'(HL)' if is_target_HL else target}, {'(HL)' if is_source_HL else source}"

        else:  # 0x80 to 0xbf: ADD, ADC, SUB, SBC, AND, XOR, OR, CP
            opcode_handler, mnemonic_prefix = self.opcode_handlers_0x80_to_0xbf[(self.opcode >> 3) & 0b111]
            value: int = self.mem[self.reg.HL] if is_source_HL else getattr(self.reg, source)
            opcode_handler(value)

            m_cycles = 2 if is_source_HL else 1
            self.mnemonic = f"{mnemonic_prefix} {'(HL)' if is_source_HL else source}"

        instruction_length = 1
        self.instruction = self.mem[self.reg.PC:self.reg.PC + instruction_length]
        self.reg.PC = (self.reg.PC + instruction_length) & 0xffff
        return m_cycles

    @property
    def d8(self) -> int:  # immediate 8-bit data
        return self.instruction[1]

    @property
    def d16(self) -> int:  # immediate 16-bit data
        return (self.instruction[2] << 8) + self.instruction[1]

    @property
    def a8(self) -> int:  # 8-bit unsigned data, which is added to $FF00 in certain instructions
        return self.instruction[1]

    @property
    def a16(self) -> int:  # 16-bit address
        return (self.instruction[2] << 8) + self.instruction[1]

    @property
    def r8(self) -> int:  # 8-bit signed data
        return (self.instruction[1] ^ 0x80) - 0x80

    def _build_opcode_table(self) -> Dict[int, Tuple[Callable[[], Optional[int]], int, int, str]]:
        return {
            # opcode: (handler, length, minimum m-cycles, mnemonic)
            0x00: (self._0x00, 1, 1, "NOP"),
            0x01: (self._0x01, 3, 3, "LD BC, d16"),
            0x02: (self._0x02, 1, 2, "LD (BC), A"),
            0x03: (self._0x03, 1, 2, "INC BC"),
            0x04: (self._0x04, 1, 1, "INC B"),
            0x05: (self._0x05, 1, 1, "DEC B"),
            0x06: (self._0x06, 2, 2, "LD B, d8"),
            0x07: (self._0x07, 1, 1, "RLCA"),
            0x08: (self._0x08, 3, 5, "LD (a16), SP"),
            0x09: (self._0x09, 1, 2, "ADD HL, BC"),
            0x0a: (self._0x0a, 1, 2, "LD A, (BC)"),
            0x0b: (self._0x0b, 1, 2, "DEC BC"),
            0x0c: (self._0x0c, 1, 1, "INC C"),
            0x0d: (self._0x0d, 1, 1, "DEC C"),
            0x0e: (self._0x0e, 2, 2, "LD C, d8"),
            0x0f: (self._0x0f, 1, 1, "RRCA"),
            0x10: (self._0x10, 1, 1, "STOP"),
            0x11: (self._0x11, 3, 3, "LD DE, d16"),
            0x12: (self._0x12, 1, 2, "LD (DE), A"),
            0x13: (self._0x13, 1, 2, "INC DE"),
            0x14: (self._0x14, 1, 1, "INC D"),
            0x15: (self._0x15, 1, 1, "DEC D"),
            0x16: (self._0x16, 2, 2, "LD D, d8"),
            0x17: (self._0x17, 1, 1, "RLA"),
            0x18: (self._0x18, 2, 3, "JR r8"),
            0x19: (self._0x19, 1, 2, "ADD HL, DE"),
            0x1a: (self._0x1a, 1, 2, "LD A, (DE)"),
            0x1b: (self._0x1b, 1, 2, "DEC DE"),
            0x1c: (self._0x1c, 1, 1, "INC E"),
            0x1d: (self._0x1d, 1, 1, "DEC E"),
            0x1e: (self._0x1e, 2, 2, "LD E, d8"),
            0x1f: (self._0x1f, 1, 1, "RRA"),
            0x20: (self._0x20, 2, 2, "JR NZ, r8"),
            0x21: (self._0x21, 3, 3, "LD HL, d16"),
            0x22: (self._0x22, 1, 2, "LD (HL+), A"),
            0x23: (self._0x23, 1, 2, "INC HL"),
            0x24: (self._0x24, 1, 1, "INC H"),
            0x25: (self._0x25, 1, 1, "DEC H"),
            0x26: (self._0x26, 2, 2, "LD H, d8"),
            0x27: (self._0x27, 1, 1, "DAA"),
            0x28: (self._0x28, 2, 2, "JR Z, r8"),
            0x29: (self._0x29, 1, 2, "ADD HL, HL"),
            0x2a: (self._0x2a, 1, 2, "LD A, (HL+)"),
            0x2b: (self._0x2b, 1, 2, "DEC HL"),
            0x2c: (self._0x2c, 1, 1, "INC L"),
            0x2d: (self._0x2d, 1, 1, "DEC L"),
            0x2e: (self._0x2e, 2, 2, "LD L, d8"),
            0x2f: (self._0x2f, 1, 1, "CPL"),
            0x30: (self._0x30, 2, 2, "JR NC, r8"),
            0x31: (self._0x31, 3, 3, "LD SP, d16"),
            0x32: (self._0x32, 1, 2, "LD (HL-), A"),
            0x33: (self._0x33, 1, 2, "INC SP"),
            0x34: (self._0x34, 1, 3, "INC (HL)"),
            0x35: (self._0x35, 1, 3, "DEC (HL)"),
            0x36: (self._0x36, 2, 3, "LD (HL), d8"),
            0x37: (self._0x37, 1, 1, "SCF"),
            0x38: (self._0x38, 2, 2, "JR C, r8"),
            0x39: (self._0x39, 1, 2, "ADD HL, SP"),
            0x3a: (self._0x3a, 1, 2, "LD A, (HL-)"),
            0x3b: (self._0x3b, 1, 2, "DEC SP"),
            0x3c: (self._0x3c, 1, 1, "INC A"),
            0x3d: (self._0x3d, 1, 1, "DEC A"),
            0x3e: (self._0x3e, 2, 2, "LD A, d8"),
            0x3f: (self._0x3f, 1, 1, "CCF"),
            # _0x40_to_0xbf()
            0x76: (self._0x76, 1, 1, "HALT"),
            0xc0: (self._0xc0, 1, 2, "RET NZ"),
            0xc1: (self._0xc1, 1, 3, "POP BC"),
            0xc2: (self._0xc2, 3, 3, "JP NZ, a16"),
            0xc3: (self._0xc3, 3, 4, "JP a16"),
            0xc4: (self._0xc4, 3, 3, "CALL NZ, a16"),
            0xc5: (self._0xc5, 1, 4, "PUSH BC"),
            0xc6: (self._0xc6, 2, 2, "ADD A, d8"),
            0xc7: (self._0xc7, 1, 4, "RST 00H"),
            0xc8: (self._0xc8, 1, 2, "RET Z"),
            0xc9: (self._0xc9, 1, 4, "RET"),
            0xca: (self._0xca, 3, 3, "JP Z, a16"),
            0xcb: (self._0xcb, 2, 2, "PREFIX"),
            0xcc: (self._0xcc, 3, 3, "CALL Z, a16"),
            0xcd: (self._0xcd, 3, 6, "CALL a16"),
            0xce: (self._0xce, 2, 2, "ADC A, d8"),
            0xcf: (self._0xcf, 1, 4, "RST 08H"),
            0xd0: (self._0xd0, 1, 2, "RET NC"),
            0xd1: (self._0xd1, 1, 3, "POP DE"),
            0xd2: (self._0xd2, 3, 3, "JP NC, a16"),
            # 0xd3
            0xd4: (self._0xd4, 3, 3, "CALL NC, a16"),
            0xd5: (self._0xd5, 1, 4, "PUSH DE"),
            0xd6: (self._0xd6, 2, 2, "SUB d8"),
            0xd7: (self._0xd7, 1, 4, "RST 10H"),
            0xd8: (self._0xd8, 1, 2, "RET C"),
            0xd9: (self._0xd9, 1, 4, "RETI"),
            0xda: (self._0xda, 3, 3, "JP C, a16"),
            # 0xdb
            0xdc: (self._0xdc, 3, 3, "CALL C, a16"),
            # 0xdd
            0xde: (self._0xde, 2, 2, "SBC A, d8"),
            0xdf: (self._0xdf, 1, 4, "RST 18H"),
            0xe0: (self._0xe0, 2, 3, "LDH (a8), A"),
            0xe1: (self._0xe1, 1, 3, "POP HL"),
            0xe2: (self._0xe2, 1, 2, "LD (C), A"),
            # 0xe3
            # 0xe4
            0xe5: (self._0xe5, 1, 4, "PUSH HL"),
            0xe6: (self._0xe6, 2, 2, "AND d8"),
            0xe7: (self._0xe7, 1, 4, "RST 20H"),
            0xe8: (self._0xe8, 2, 4, "ADD SP, r8"),
            0xe9: (self._0xe9, 1, 1, "JP HL"),
            0xea: (self._0xea, 3, 4, "LD (a16), A"),
            # 0xeb
            # 0xec
            # 0xed
            0xee: (self._0xee, 2, 2, "XOR d8"),
            0xef: (self._0xef, 1, 4, "RST 28H"),
            0xf0: (self._0xf0, 2, 3, "LDH A, (a8)"),
            0xf1: (self._0xf1, 1, 3, "POP AF"),
            0xf2: (self._0xf2, 1, 2, "LD A, (C)"),
            0xf3: (self._0xf3, 1, 1, "DI"),
            # 0xf4
            0xf5: (self._0xf5, 1, 4, "PUSH AF"),
            0xf6: (self._0xf6, 2, 2, "OR d8"),
            0xf7: (self._0xf7, 1, 4, "RST 30H"),
            0xf8: (self._0xf8, 2, 4, "LD HL, SP + r8"),
            0xf9: (self._0xf9, 1, 1, "LD SP, HL"),
            0xfa: (self._0xfa, 3, 4, "LD A, (a16)"),
            0xfb: (self._0xfb, 1, 1, "EI"),
            # 0xfc
            # 0xfd
            0xfe: (self._0xfe, 2, 2, "CP d8"),
            0xff: (self._0xff, 1, 4, "RST 38H")
        }

    """Helper methods"""

    def _ADD(self, value: int):
        sum = self.reg.A + value
        result = sum & 0xff
        self.reg.A = result
        self.reg.flags(Z=result == 0, N=0, H=(self.reg.A & 0x0f) + (value & 0x0f) > 0x0f, C=sum > 0xff)

    def _ADC(self, value: int):
        sum = self.reg.A + value + self.reg.carry_flag
        result = sum & 0xff
        self.reg.A = result
        self.reg.flags(Z=result == 0, N=0,
                       H=(self.reg.A & 0x0f) + (value & 0x0f) + self.reg.carry_flag > 0x0f, C=sum > 0xff)

    def _SUB(self, value: int):
        difference = self.reg.A - value
        result = difference & 0xff
        self.reg.A = result
        self.reg.flags(Z=result == 0, N=1, H=(self.reg.A & 0x0f) - (value & 0x0f) < 0, C=difference < 0)

    def _SBC(self, value: int):
        difference = self.reg.A - value - self.reg.carry_flag
        result = difference & 0xff
        self.reg.A = result
        self.reg.flags(Z=result == 0, N=1,
                       H=(self.reg.A & 0x0f) - (value & 0x0f) - self.reg.carry_flag < 0, C=difference < 0)

    def _AND(self, value: int):
        self.reg.A &= value
        self.reg.flags(Z=self.reg.A == 0, N=0, H=1, C=0)

    def _XOR(self, value: int):
        self.reg.A ^= value
        self.reg.flags(Z=self.reg.A == 0, N=0, H=0, C=0)

    def _OR(self, value: int):
        self.reg.A |= value
        self.reg.flags(Z=self.reg.A == 0, N=0, H=0, C=0)

    def _CP(self, value: int):
        self.reg.flags(Z=self.reg.A == value, N=1, H=(self.reg.A & 0x0f) - (value & 0x0f) < 0, C=self.reg.A < value)

    def _PUSH(self, value: int):
        self.reg.SP = (self.reg.SP - 2) & 0xffff
        self.mem.write_word(self.reg.SP, value)

    def _POP(self) -> int:
        value = self.mem.read_word(self.reg.SP)
        self.reg.SP = (self.reg.SP + 2) & 0xffff
        return value

    def _RLC(self, value: int) -> int:
        result = ((value << 1) | (value >> 7)) & 0xff
        self.reg.flags(Z=result == 0, N=0, H=0, C=result & 0b1)
        return result

    def _RRC(self, value: int) -> int:
        result = ((value << 7) | (value >> 1)) & 0xff
        self.reg.flags(Z=result == 0, N=0, H=0, C=(result & 0x80) >> 7)
        return result

    def _RL(self, value: int) -> int:
        result = ((value << 1) | self.reg.carry_flag) & 0xff
        self.reg.flags(Z=result == 0, N=0, H=0, C=(value & 0x80) >> 7)
        return result

    def _RR(self, value: int) -> int:
        result = ((self.reg.carry_flag << 7) | (value >> 1)) & 0xff
        self.reg.flags(Z=result == 0, N=0, H=0, C=value & 0b1)
        return result

    def _SLA(self, value: int) -> int:
        result = (value << 1) & 0xff
        self.reg.flags(Z=result == 0, N=0, H=0, C=(value & 0x80) >> 7)
        return result

    def _SRA(self, value: int) -> int:
        result = (value & 0x80) | (value >> 1)
        self.reg.flags(Z=result == 0, N=0, H=0, C=value & 0b1)
        return result

    def _SWAP(self, value: int) -> int:
        result = ((value & 0x0f) << 4) | (value >> 4)
        self.reg.flags(Z=result == 0, N=0, H=0, C=0)
        return result

    def _SRL(self, value: int) -> int:
        result = value >> 1
        self.reg.flags(Z=result == 0, N=0, H=0, C=value & 0b1)
        return result

    """Misc / control instructions"""

    def _0x00(self):  # NOP
        pass

    def _0x10(self):  # STOP
        self.is_stopped = True  # TODO

    def _0x76(self):  # HALT
        self.is_halted = True

    def _0xcb(self):  # PREFIX
        prefixed_opcode: int = self.instruction[1]
        register: str = self.register_order[prefixed_opcode & 0b111]
        is_HL: bool = register == "HL"
        mnemonic_source: str = "(HL)" if is_HL else register
        value: int = self.mem[self.reg.HL] if is_HL else getattr(self.reg, register)
        result = None

        if prefixed_opcode <= 0x3f:
            opcode_handler, mnemonic_prefix = self.opcode_handlers_0xcb00_to_0xcb3f[prefixed_opcode >> 3]
            self.mnemonic = mnemonic_prefix + mnemonic_source
            result = opcode_handler(value)
        else:
            bit = (prefixed_opcode >> 3) & 0b111
            if prefixed_opcode <= 0x7f:
                self.reg.flags(Z=(value & (1 << bit)) == 0, N=0, H=1)  # BIT
                self.mnemonic = f"BIT {bit}, {mnemonic_source}"
            elif prefixed_opcode <= 0xbf:
                result = value & (~(1 << bit))  # RES
                self.mnemonic = f"RES {bit}, {mnemonic_source}"
            else:
                result = value | (1 << bit)  # SET
                self.mnemonic = f"SET {bit}, {mnemonic_source}"

        if result is not None:
            if is_HL:
                self.mem[self.reg.HL] = result
            else:
                setattr(self.reg, register, result)

        if is_HL:
            additional_cycles = 1 if (0x40 <= prefixed_opcode <= 0x7f) else 2
            return additional_cycles

    def _0xf3(self):  # DI
        self.interrupts_enabled = False  # TODO

    def _0xfb(self):  # EI
        self.interrupts_enabled = True  # TODO

    """Jumps / calls"""

    def _0x18(self):  # JR r8
        self.reg.PC = (self.reg.PC + self.r8) & 0xffff

    def _0x20(self):  # JR NZ, r8
        if not self.reg.zero_flag:
            self.reg.PC = (self.reg.PC + self.r8) & 0xffff
            return 1

    def _0x28(self):  # JR Z, r8
        if self.reg.zero_flag:
            self.reg.PC = (self.reg.PC + self.r8) & 0xffff
            return 1

    def _0x30(self):  # JR NC, r8
        if not self.reg.carry_flag:
            self.reg.PC = (self.reg.PC + self.r8) & 0xffff
            return 1

    def _0x38(self):  # JR C, r8
        if self.reg.carry_flag:
            self.reg.PC = (self.reg.PC + self.r8) & 0xffff
            return 1

    def _0xc0(self):  # RET NZ
        if not self.reg.zero_flag:
            self.reg.PC = self._POP()
            return 3

    def _0xc2(self):  # JP NZ, a16
        if not self.reg.zero_flag:
            self.reg.PC = self.a16
            return 1

    def _0xc3(self):  # JP a16
        self.reg.PC = self.a16

    def _0xc4(self):  # CALL NZ, a16
        if not self.reg.zero_flag:
            self._PUSH(self.reg.PC)
            self.reg.PC = self.a16
            return 3

    def _0xc7(self):  # RST 00H
        self._PUSH((self.reg.PC - 1) & 0xffff)
        self.reg.PC = 0x0000

    def _0xc8(self):  # RET Z
        if self.reg.zero_flag:
            self.reg.PC = self._POP()
            return 3

    def _0xc9(self):  # RET
        self.reg.PC = self._POP()

    def _0xca(self):  # JP Z, a16
        if self.reg.zero_flag:
            self.reg.PC = self.a16
            return 1

    def _0xcc(self):  # CALL Z, a16
        if self.reg.zero_flag:
            self._PUSH(self.reg.PC)
            self.reg.PC = self.a16
            return 3

    def _0xcd(self):  # CALL a16
        self._PUSH(self.reg.PC)
        self.reg.PC = self.a16
        return 3

    def _0xcf(self):  # RST 08H
        self._PUSH((self.reg.PC - 1) & 0xffff)
        self.reg.PC = 0x0008

    def _0xd0(self):  # RET NC
        if not self.reg.carry_flag:
            self.reg.PC = self._POP()
            return 3

    def _0xd2(self):  # JP NC, a16
        if not self.reg.carry_flag:
            self.reg.PC = self.a16
            return 1

    def _0xd4(self):  # CALL NC, a16
        if not self.reg.carry_flag:
            self._PUSH(self.reg.PC)
            self.reg.PC = self.a16
            return 3

    def _0xd7(self):  # RST 10H
        self._PUSH((self.reg.PC - 1) & 0xffff)
        self.reg.PC = 0x0010

    def _0xd8(self):  # RET C
        if self.reg.carry_flag:
            self.reg.PC = self._POP()
            return 3

    def _0xd9(self):  # RETI
        self.reg.PC = self._POP()
        self.interrupts_enabled = True  # TODO

    def _0xda(self):  # JP C, a16
        if self.reg.carry_flag:
            self.reg.PC = self.a16
            return 1

    def _0xdc(self):  # CALL C, a16
        if self.reg.carry_flag:
            self._PUSH(self.reg.PC)
            self.reg.PC = self.a16
            return 3

    def _0xdf(self):  # RST 18H
        self._PUSH((self.reg.PC - 1) & 0xffff)
        self.reg.PC = 0x0018

    def _0xe7(self):  # RST 20H
        self._PUSH((self.reg.PC - 1) & 0xffff)
        self.reg.PC = 0x0020

    def _0xe9(self):  # JP HL
        self.reg.PC = self.reg.HL

    def _0xef(self):  # RST 28H
        self._PUSH((self.reg.PC - 1) & 0xffff)
        self.reg.PC = 0x0028

    def _0xf7(self):  # RST 30H
        self._PUSH((self.reg.PC - 1) & 0xffff)
        self.reg.PC = 0x0030

    def _0xff(self):  # RST 38H
        self._PUSH((self.reg.PC - 1) & 0xffff)
        self.reg.PC = 0x0038

    """8-bit load instructions"""

    def _0x02(self):  # LD (BC), A
        self.mem[self.reg.BC] = self.reg.A

    def _0x06(self):  # LD B, d8
        self.reg.B = self.d8

    def _0x0a(self):  # LD A, (BC)
        self.reg.A = self.mem[self.reg.BC]

    def _0x0e(self):  # LD C, d8
        self.reg.C = self.d8

    def _0x12(self):  # LD (DE), A
        self.mem[self.reg.DE] = self.reg.A

    def _0x16(self):  # LD D, d8
        self.reg.D = self.d8

    def _0x1a(self):  # LD A, (DE)
        self.reg.A = self.mem[self.reg.DE]

    def _0x1e(self):  # LD E, d8
        self.reg.E = self.d8

    def _0x22(self):  # LD (HL+), A
        self.mem[self.reg.HL] = self.reg.A
        self.reg.HL = (self.reg.HL + 1) & 0xffff

    def _0x26(self):  # LD H, d8
        self.reg.H = self.d8

    def _0x2a(self):  # LD A, (HL+)
        self.reg.A = self.mem[self.reg.HL]
        self.reg.HL = (self.reg.HL + 1) & 0xffff

    def _0x2e(self):  # LD L, d8
        self.reg.L = self.d8

    def _0x32(self):  # LD (HL-), A
        self.mem[self.reg.HL] = self.reg.A
        self.reg.HL = (self.reg.HL - 1) & 0xffff

    def _0x36(self):  # LD (HL), d8
        self.mem[self.reg.HL] = self.d8

    def _0x3a(self):  # LD A, (HL-)
        self.reg.A = self.mem[self.reg.HL]
        self.reg.HL = (self.reg.HL - 1) & 0xffff

    def _0x3e(self):  # LD A, d8
        self.reg.A = self.d8

    def _0xe0(self):  # LDH (a8), A
        self.mem[0xff00 + self.a8] = self.reg.A

    def _0xe2(self):  # LD (C), A
        self.mem[0xff00 + self.reg.C] = self.reg.A

    def _0xea(self):  # LD (a16), A
        self.mem[self.a16] = self.reg.A

    def _0xf0(self):  # LDH A, (a8)
        self.reg.A = self.mem[0xff00 + self.a8]

    def _0xf2(self):  # LD A, (C)
        self.reg.A = self.mem[0xff00 + self.reg.C]

    def _0xfa(self):  # LD A, (a16)
        self.reg.A = self.mem[self.a16]

    """16-bit load instructions"""

    def _0x01(self):  # LD BC, d16
        self.reg.BC = self.d16

    def _0x08(self):  # LD (a16), SP
        self.mem.write_word(self.a16, self.reg.SP)

    def _0x11(self):  # LD DE, d16
        self.reg.DE = self.d16

    def _0x21(self):  # LD HL, d16
        self.reg.HL = self.d16

    def _0x31(self):  # LD SP, d16
        self.reg.SP = self.d16

    def _0xc1(self):  # POP BC
        self.reg.BC = self._POP()

    def _0xc5(self):  # PUSH BC
        self._PUSH(self.reg.BC)

    def _0xd1(self):  # POP DE
        self.reg.DE = self._POP()

    def _0xd5(self):  # PUSH DE
        self._PUSH(self.reg.DE)

    def _0xe1(self):  # POP HL
        self.reg.HL = self._POP()

    def _0xe5(self):  # PUSH HL
        self._PUSH(self.reg.HL)

    def _0xf1(self):  # POP AF
        self.reg.AF = self._POP()

    def _0xf5(self):  # PUSH AF
        self._PUSH(self.reg.AF)

    def _0xf8(self):  # LD HL, SP + r8
        self.HL = self.reg.SP + self.r8
        self.reg.flags(Z=0, N=0, H=(self.reg.SP & 0x0f) + (self.r8 & 0x0f) > 0xff,
                       C=(self.reg.SP & 0xff) + (self.r8 & 0xff) > 0xff)

    def _0xf9(self):  # LD SP, HL
        self.reg.SP = self.reg.HL

    """8-bit arithmetic / logical instructions"""

    def _0x04(self):  # INC B
        self.reg.B = (self.reg.B + 1) & 0xff
        self.reg.flags(Z=self.reg.B == 0, N=0, H=(self.reg.B & 0x0f) == 0)

    def _0x05(self):  # DEC B
        self.reg.B = (self.reg.B - 1) & 0xff
        self.reg.flags(Z=self.reg.B == 0, N=1, H=(self.reg.B & 0x0f) == 0x0f)

    def _0x0c(self):  # INC C
        self.reg.C = (self.reg.C + 1) & 0xff
        self.reg.flags(Z=self.reg.C == 0, N=0, H=(self.reg.C & 0x0f) == 0)

    def _0x0d(self):  # DEC C
        self.reg.C = (self.reg.C - 1) & 0xff
        self.reg.flags(Z=self.reg.C == 0, N=1, H=(self.reg.C & 0x0f) == 0x0f)

    def _0x14(self):  # INC D
        self.reg.D = (self.reg.D + 1) & 0xff
        self.reg.flags(Z=self.reg.D == 0, N=0, H=(self.reg.D & 0x0f) == 0)

    def _0x15(self):  # DEC D
        self.reg.D = (self.reg.D - 1) & 0xff
        self.reg.flags(Z=self.reg.D == 0, N=1, H=(self.reg.D & 0x0f) == 0x0f)

    def _0x1c(self):  # INC E
        self.reg.E = (self.reg.E + 1) & 0xff
        self.reg.flags(Z=self.reg.E == 0, N=0, H=(self.reg.E & 0x0f) == 0)

    def _0x1d(self):  # DEC E
        self.reg.E = (self.reg.E - 1) & 0xff
        self.reg.flags(Z=self.reg.E == 0, N=1, H=(self.reg.E & 0x0f) == 0x0f)

    def _0x24(self):  # INC H
        self.reg.H = (self.reg.H + 1) & 0xff
        self.reg.flags(Z=self.reg.H == 0, N=0, H=(self.reg.H & 0x0f) == 0)

    def _0x25(self):  # DEC H
        self.reg.H = (self.reg.H - 1) & 0xff
        self.reg.flags(Z=self.reg.H == 0, N=1, H=(self.reg.H & 0x0f) == 0x0f)

    def _0x27(self):  # DAA
        # https://forums.nesdev.com/viewtopic.php?t=15944
        if self.reg.subtract_flag:
            if self.reg.carry_flag:
                self.reg.A = (self.reg.A - 0x60) & 0xff
            if self.reg.half_carry_flag:
                self.reg.A = (self.reg.A - 0x06) & 0xff
        else:
            if self.reg.carry_flag or self.reg.A > 0x99:
                self.reg.A = (self.reg.A + 0x60) & 0xff
                self.reg.carry_flag = True
            if self.reg.half_carry_flag or (self.reg.A & 0x0f) > 0x09:
                self.reg.A = (self.reg.A + 0x06) & 0xff
        self.reg.flags(Z=self.reg.A == 0, H=0)

    def _0x2c(self):  # INC L
        self.reg.L = (self.reg.L + 1) & 0xff
        self.reg.flags(Z=self.reg.L == 0, N=0, H=(self.reg.L & 0x0f) == 0)

    def _0x2d(self):  # DEC L
        self.reg.L = (self.reg.L - 1) & 0xff
        self.reg.flags(Z=self.reg.L == 0, N=1, H=(self.reg.L & 0x0f) == 0x0f)

    def _0x2f(self):  # CPL
        self.reg.A ^= 0xff
        self.reg.flags(N=1, H=1)

    def _0x34(self):  # INC (HL)
        result = (self.mem[self.reg.HL] + 1) & 0xff
        self.mem[self.reg.HL] = result
        self.reg.flags(Z=result == 0, N=0, H=(result & 0x0f) == 0)

    def _0x35(self):  # DEC (HL)
        result = (self.mem[self.reg.HL] - 1) & 0xff
        self.mem[self.reg.HL] = result
        self.reg.flags(Z=result == 0, N=1, H=(result & 0x0f) == 0x0f)

    def _0x37(self):  # SCF
        self.reg.flags(N=0, H=0, C=1)

    def _0x3c(self):  # INC A
        self.reg.A = (self.reg.A + 1) & 0xff
        self.reg.flags(Z=self.reg.A == 0, N=0, H=(self.reg.A & 0x0f) == 0)

    def _0x3d(self):  # DEC A
        self.reg.A = (self.reg.A - 1) & 0xff
        self.reg.flags(Z=self.reg.A == 0, N=1, H=(self.reg.A & 0x0f) == 0x0f)

    def _0x3f(self):  # CCF
        self.reg.flags(N=0, H=0, C=not self.reg.carry_flag)

    def _0xc6(self):  # ADD A, d8
        self._ADD(self.d8)

    def _0xce(self):  # ADC A, d8
        self._ADC(self.d8)

    def _0xd6(self):  # SUB d8
        self._SUB(self.d8)

    def _0xde(self):  # SBC A, d8
        self._SBC(self.d8)

    def _0xe6(self):  # AND d8
        self._AND(self.d8)

    def _0xee(self):  # XOR d8
        self._XOR(self.d8)

    def _0xf6(self):  # OR d8
        self._OR(self.d8)

    def _0xfe(self):  # CP d8
        self._CP(self.d8)

    """16-bit arithmetic / logical instructions"""

    def _0x03(self):  # INC BC
        self.reg.BC = (self.reg.BC + 1) & 0xffff

    def _0x09(self):  # ADD HL, BC
        result = self.reg.HL + self.reg.BC
        self.reg.flags(N=0, H=(self.reg.HL & 0xfff) + (self.reg.BC & 0xfff) > 0xfff, C=result > 0xffff)
        self.reg.HL = result & 0xffff

    def _0x0b(self):  # DEC BC
        self.reg.BC = (self.reg.BC - 1) & 0xffff

    def _0x13(self):  # INC DE
        self.reg.DE = (self.reg.DE + 1) & 0xffff

    def _0x19(self):  # ADD HL, DE
        result = self.reg.HL + self.reg.DE
        self.reg.flags(N=0, H=(self.reg.HL & 0xfff) + (self.reg.DE & 0xfff) > 0xfff, C=result > 0xffff)
        self.reg.HL = result & 0xffff

    def _0x1b(self):  # DEC DE
        self.reg.DE = (self.reg.DE - 1) & 0xffff

    def _0x23(self):  # INC HL
        self.reg.HL = (self.reg.HL + 1) & 0xffff

    def _0x29(self):  # ADD HL, HL
        self.reg.flags(N=0, H=(self.reg.HL & 0xfff) > 0x7ff, C=self.reg.HL > 0x7fff)
        self.reg.HL = (self.reg.HL << 1) & 0xffff

    def _0x2b(self):  # DEC HL
        self.reg.HL = (self.reg.HL - 1) & 0xffff

    def _0x33(self):  # INC SP
        self.reg.SP = (self.reg.SP + 1) & 0xffff

    def _0x39(self):  # ADD HL, SP
        result = self.reg.HL + self.reg.SP
        self.reg.flags(N=0, H=(self.reg.HL & 0xfff) + (self.reg.SP & 0xfff) > 0xfff, C=result > 0xffff)
        self.reg.HL = result & 0xffff

    def _0x3b(self):  # DEC SP
        self.reg.SP = (self.reg.SP - 1) & 0xffff

    def _0xe8(self):  # ADD SP, r8
        self.reg.flags(Z=0, N=0, H=(self.reg.SP & 0x0f) + (self.reg.SP & 0x0f) > 0x0f,
                       C=(self.reg.SP & 0xff) + (self.reg.SP & 0xff) > 0xff)
        self.reg.SP = (self.reg.SP + self.r8) & 0xffff

    """8-bit shift, rotate and bit instructions"""

    def _0x07(self):  # RLCA
        self.reg.A = ((self.reg.A << 1) | self.reg.A >> 7) & 0xff
        self.reg.flags(Z=0, N=0, H=0, C=self.reg.A & 0b1)

    def _0x0f(self):  # RRCA
        self.reg.A = ((self.reg.A << 7) | (self.reg.A >> 1)) & 0xff
        self.reg.flags(Z=0, N=0, H=0, C=(self.reg.A & 0x80) >> 7)

    def _0x17(self):  # RLA
        self.reg.flags(Z=0, N=0, H=0, C=self.reg.A >> 7)
        self.reg.A = ((self.reg.A << 1) | self.reg.carry_flag) & 0xff

    def _0x1f(self):  # RRA
        self.reg.flags(Z=0, N=0, H=0, C=self.reg.A & 0b1)
        self.reg.A = (self.reg.carry_flag << 7) | (self.reg.A >> 1)
