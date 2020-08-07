from gameboy.memory import Memory


class LCD:
    def __init__(self, mem: Memory):
        self.mem = mem

    def set_status(self):

        current_mode = self.LCDC & 0x3

        new_mode = 0
        new_status = self.STAT

        require_interrupt = False

        current_line = self.LY
        if current_line >= 144: # VBLANK
            self.set_STAT_bit(0, True)
            self.set_STAT_bit(1, False)
            require_interrupt = self.get_STAT_bit(4)
        else:
            pass



    @property
    def LCDC(self) -> int:
        return self.mem[0xff40]

    def get_LCDC_bit(self, bit: int) -> bool:
        return (self.mem[0xff40] & (1 << bit)) != 0

    def set_LCDC_bit(self, bit: int, value: bool):
        self.mem[0xff40] = (self.mem[0xff40] & (~(1 << bit))) | (value << bit)

    @property
    def STAT(self) -> int:
        return self.mem[0xff41]

    def get_STAT_bit(self, bit: int) -> bool:
        return (self.mem[0xff41] & (1 << bit)) != 0

    def set_STAT_bit(self, bit: int, value: bool):
        self.mem[0xff41] = (self.mem[0xff41] & (~(1 << bit))) | (value << bit)

    @property
    def SCY(self) -> int:
        return self.mem[0xff42]

    @property
    def SCX(self) -> int:
        return self.mem[0xff43]

    @property
    def LY(self) -> int:
        return self.mem[0xff44]

    @property
    def LYC(self) -> int:
        return self.mem[0xff45]
