from time import sleep

import pygame

from gameboy.cpu import CPU
from gameboy.lcd import LCD
from gameboy.memory import Memory

BLACK = (0, 0, 0)
DARK = (85, 85, 85)
LIGHT = (170, 170, 170)
WHITE = (255, 255, 255)


class GameBoy:

    def __init__(self, debug=False):
        self.debug = debug
        self.mem = Memory(dmg_boot_rom)

        self.cpu = CPU(self.mem)
        self.lcd = LCD(self.mem)

        self.scanline_counter = 0

        self.surface = self._init_screen()

    def insert_cartridge(self, rom: bytes):
        self.mem.set_cartridge(rom)

    def power_on(self):
        for _ in range(1000000):
            self._next_frame()

        sleep(5)

    def _next_frame(self):
        # cycle_count = 0
        # while True:
        m_cycles = self.cpu.step()

        if self.debug:
            print(f"{self.cpu.last_pc:0{4}x}  {self.cpu.last_instruction.hex():6}  {self.cpu.last_mnemonic}")

        self._display_tileset()

        # update_timers(m_cycles)
        # self._update_scanlines(m_cycles)
        # do_interrupts()

    def _update_scanlines(self, m_cycles: int):
        self.scanline_counter -= m_cycles
        if self.scanline_counter <= 0:
            self.scanline_counter = 0
            current_line = self.mem[0xff44] + 1
            self.mem[0xff44] = current_line
            if current_line == 144:
                # TODO: request interrupt
                pass

            elif current_line > 153:
                self.mem[0xff44] = 0

            else:
                self._draw_scanline()

    def _draw_scanline(self):
        pass

    def _init_screen(self):
        surface = pygame.display.set_mode((256, 256))
        surface.fill(BLACK)
        return surface

    def _display_tileset(self):
        colors = (WHITE, DARK, LIGHT, BLACK)
        for tile_num in range(16):
            base_address = 0x8000 + 16 * tile_num
            for row in range(8):
                tile_byte_1 = self.mem[base_address + 2 * row]
                tile_byte_2 = self.mem[base_address + 2 * row + 1]
                for bit in range(8):
                    x = 8 * tile_num + bit
                    y = 8 * tile_num + row
                    bit_1 = (tile_byte_1 & (1 << bit)) > 0
                    bit_2 = (tile_byte_1 & (1 << bit)) > 0
                    color = colors[(bit_1 << 1) | bit_2]
                    rect = (x, y, 1, 1)
                    pygame.draw.rect(self.surface, color, rect)


dmg_boot_rom = b'\x31\xfe\xff\xaf\x21\xff\x9f\x32\xcb\x7c\x20\xfb\x21\x26\xff\x0e' \
               b'\x11\x3e\x80\x32\xe2\x0c\x3e\xf3\xe2\x32\x3e\x77\x77\x3e\xfc\xe0' \
               b'\x47\x11\x04\x01\x21\x10\x80\x1a\xcd\x95\x00\xcd\x96\x00\x13\x7b' \
               b'\xfe\x34\x20\xf3\x11\xd8\x00\x06\x08\x1a\x13\x22\x23\x05\x20\xf9' \
               b'\x3e\x19\xea\x10\x99\x21\x2f\x99\x0e\x0c\x3d\x28\x08\x32\x0d\x20' \
               b'\xf9\x2e\x0f\x18\xf3\x67\x3e\x64\x57\xe0\x42\x3e\x91\xe0\x40\x04' \
               b'\x1e\x02\x0e\x0c\xf0\x44\xfe\x90\x20\xfa\x0d\x20\xf7\x1d\x20\xf2' \
               b'\x0e\x13\x24\x7c\x1e\x83\xfe\x62\x28\x06\x1e\xc1\xfe\x64\x20\x06' \
               b'\x7b\xe2\x0c\x3e\x87\xe2\xf0\x42\x90\xe0\x42\x15\x20\xd2\x05\x20' \
               b'\x4f\x16\x20\x18\xcb\x4f\x06\x04\xc5\xcb\x11\x17\xc1\xcb\x11\x17' \
               b'\x05\x20\xf5\x22\x23\x22\x23\xc9\xce\xed\x66\x66\xcc\x0d\x00\x0b' \
               b'\x03\x73\x00\x83\x00\x0c\x00\x0d\x00\x08\x11\x1f\x88\x89\x00\x0e' \
               b'\xdc\xcc\x6e\xe6\xdd\xdd\xd9\x99\xbb\xbb\x67\x63\x6e\x0e\xec\xcc' \
               b'\xdd\xdc\x99\x9f\xbb\xb9\x33\x3e\x3c\x42\xb9\xa5\xb9\xa5\x42\x3c' \
               b'\x21\x04\x01\x11\xa8\x00\x1a\x13\xbe\x00\x00\x23\x7d\xfe\x34\x20' \
               b'\xf5\x06\x19\x78\x86\x23\x05\x20\xfb\x86\x00\x00\x3e\x01\xe0\x50'
