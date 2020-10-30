

# CHIP-8 Emulator

CHIP-8 is an interpreted programming language, developed by Joseph Weisbecker. It was initially used on the COSMAC VIP and Telmac 1800 8-bit microcomputers in the mid-1970s. CHIP-8 programs are run on a CHIP-8 virtual machine.

Read more at: [https://en.wikipedia.org/wiki/CHIP-8](https://en.wikipedia.org/wiki/CHIP-8)
## Getting Started

In order to download any dependencies, building and running the emulator you need to have `cabal` and `ghc` installed. On windows you can download [Haskell Platform](https://www.haskell.org/downloads/#haskell-platform), which comes bundled with GHC and Cabal among other utilities.

Start by cloning the repository with: `git clone https://github.com/joelsiks/chip8-hs`

There are a couple of rom files already present in the repository in the `roms/` folder which you can use when running the emulator.

## Running

### Installing dependencies
You might want to download any dependencies if they are not already installed. Use:
```
cabal update
```

### Running

To build and run the emulator, use:
```
cabal run
```

## Usage
When starting the emulator the program will look for a folder named `roms`. It will then ask you what rom located in the `roms` folder you want to run.

When the emulator window pops up, you start it by clicking on the window.

### Input combinations
The 16 available keyboard input keys for the emulator are the following:
| KEY|KEY|KEY|KEY|
|---|---|---|---|
| 1 | 2 | 3 | 4 |
| Q | W | E | R |
| A | S | D | F |
| Z | X | C | V |

After launching a ROM it might not be immediately clear what input the program expects (if anything). The input combinations for  some of the most common ROMs that are played on the CHIP-8 can be seen in the table below.

| ROM      | Instructions                                                                                                                                 |
|----------|----------------------------------------------------------------------------------------------------------------------------------------------|
| CONNECT4 | Use Q & E to move to the left or to the right and W to drop disc.                                                                            |
| PONG     | Use 1 & Q to move the left paddle up or down and 4 & R to move the rightpaddle up or down.                                                   |
| TICTAC   | Hold down every key until your X or O has been placed. Use 1, 2, 3 for thefirst row, Q, W, E for the second row & A, S, D for the third row. |
| HIDDEN   | Press any key to start the game. Use 2, Q, E & S to move around and W tochoose a card.                                                       |

### Exiting the emualtor
If you want to exit the program at any time, press the escape key (ESC).
