from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter

from gameboy.gameboy import GameBoy


def main():
    # noinspection PyTypeChecker
    parser = ArgumentParser(description="Python Game Boy emulator", formatter_class=ArgumentDefaultsHelpFormatter)
    parser.add_argument("rom", type=str, help="ROM file")
    args = parser.parse_args()

    with open(args.rom, "rb") as f:
        rom = f.read()
    gameboy = GameBoy(debug=False)
    gameboy.insert_cartridge(rom)
    gameboy.power_on()


if __name__ == "__main__":
    main()
