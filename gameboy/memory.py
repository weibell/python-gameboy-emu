from typing import Union


class Memory:
    def __init__(self, boot_rom: bytes):
        self.boot_rom = boot_rom
        self.rom = bytearray(32 * 1024)  # TODO: bytes?
        self.vram = bytearray(8 * 1024)
        self.eram = bytearray(8 * 1024)  # TODO: offset
        self.wram = bytearray(8 * 1024)
        # TODO: OAM etc.

        self.has_booted = False
        # self.cartridge = None
        self.rom_offset = 0

    def set_cartridge(self, rom: bytes):
        # self.cartridge = cartridge
        self.rom[0:len(rom)] = rom

    def __getitem__(self, address: int) -> Union[int, bytes]:
        if isinstance(address, slice):
            return bytes([self[addr] for addr in range(address.start, address.stop)])

        if address <= 0x7fff:
            if self.has_booted:
                return self.rom[address]
            else:
                # TODO: set has_booted to False somewhere
                return self.boot_rom[address]
        elif 0x8000 <= address <= 0x9fff:
            return self.vram[address & 0x1fff]
        elif 0xa000 <= address <= 0xbfff:
            return self.eram[address & 0x1fff]
        elif 0xc000 <= address <= 0xdfff:
            return self.wram[address & 0x1fff]
        # print(f"tried to read from {hex(address)}")

    def __setitem__(self, address: int, value: int):
        if address <= 0x7fff:
            self.rom[address] = value
        elif address <= 0x9fff:
            self.vram[address & 0x1fff] = value
        elif address <= 0xbfff:
            self.eram[address & 0x1fff] = value
        elif address <= 0xdfff:
            self.wram[address & 0x1fff] = value
        # print(f"tried to write to {hex(address)}")

    def read_word(self, address: int) -> int:
        return (self[(address + 1) & 0xffff] << 8) | self[address]

    def write_word(self, address: int, value: int):
        self[address] = value & 0x00ff
        self[(address + 1) & 0xffff] = value >> 8

    @property
    def lcd_status(self) -> int:
        return self[0xff41]

    @lcd_status.setter
    def lcd_status(self, value: int):
        self[0xff41] = value
