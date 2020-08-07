import unittest

from gameboy.registers import Registers


class TestRegisters(unittest.TestCase):
    def test_af(self):
        registers = Registers()
        registers.AF = 0xabcd
        self.assertEqual(0xab, registers.A)
        self.assertEqual(0xcd, registers.F)
        registers.A = 0x12
        registers.F = 0x34
        self.assertEqual(0x1234, registers.AF)

    def test_bc(self):
        registers = Registers()
        registers.BC = 0xabcd
        self.assertEqual(0xab, registers.B)
        self.assertEqual(0xcd, registers.C)
        registers.B = 0x12
        registers.C = 0x34
        self.assertEqual(0x1234, registers.BC)

    def test_de(self):
        registers = Registers()
        registers.DE = 0xabcd
        self.assertEqual(0xab, registers.D)
        self.assertEqual(0xcd, registers.E)
        registers.D = 0x12
        registers.E = 0x34
        self.assertEqual(0x1234, registers.DE)
