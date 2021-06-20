# Virtual Light Machine (1995) by Jeff Minter
<img src="https://user-images.githubusercontent.com/58846/122686598-c82f1700-d209-11eb-89b5-7f965b1cbec6.jpg" height=250><img src="https://user-images.githubusercontent.com/58846/122686679-6622e180-d20a-11eb-93db-3bb82dfb53a0.gif" height=250>

[<img src="https://img.shields.io/badge/Lastest%20Release-Jaguar-blue.svg">](https://github.com/mwenge/vlm/releases/download/v0.1/VirtualLightMachine.jag)

This is the reconstructed source code for the Virtual Light Machine by Jeff Minter originally published in 1995 for the ill-fated Atari Jaguar.

It is part of the [llamasource project](https://mwenge.github.io/llamaSource/).

The source code can be compiled into an executable that you can run in `t2k.exe`, a Jaguar emulator included in the `utils` folder.


<!-- vim-markdown-toc GFM -->

* [Build Instructions](#build-instructions)
  * [Build Requirements](#build-requirements)
  * [Build the assembler toolchain](#build-the-assembler-toolchain)
  * [Build the Virtual Light Machine](#build-the-virtual-light-machine)
* [Play the Virtual Light Machine](#play-the-virtual-light-machine)
  * [Getting to know the controls of the Virtual Light Machine](#getting-to-know-the-controls-of-the-virtual-light-machine)
* [Notes on the Source Code](#notes-on-the-source-code)

<!-- vim-markdown-toc -->
## Build Instructions

### Build Requirements
```sh
sudo apt install build-essentials wine
```

### Build the assembler toolchain

We use two tools to build the source code: `rmac` and `rln`. If you already have these installed you may have some
luck using them, if not you can build the versions included in this repository as they are known to work. 

First you must run the following to set up the git submodules containing the assembler toolchain:

```sh
git submodule init
git submodule update
```

Now you can build the toolchain, as follows:

```sh
cd rmac
make
cd ../rln
make 
cd ..
```

### Build the Virtual Light Machine

To build the rom image `VirtualLightMachine.jag`:
```sh
make virtuallightmachine.jag
```

## Play the Virtual Light Machine

You can run the VLM as follows using `t2k.exe`:
```sh
wine ./utils/t2k.exe
```

Now open `VirtualLightMachine.jag` as follows:

<img src="https://user-images.githubusercontent.com/58846/122686992-fd3c6900-d20b-11eb-8513-6357e686d609.png" height=300>

<img src="https://user-images.githubusercontent.com/58846/122687003-0f1e0c00-d20c-11eb-8383-6ac3226e7698.png" height=300>

To launch the Virtual Light Machine, press 'Q'. 

With the VLM activated, play some music on your computer and press `F3` to start visualizing the music:

<img src="https://user-images.githubusercontent.com/58846/122687014-1ba26480-d20c-11eb-9192-d6c86d83d144.png" height=300>

### Getting to know the controls of the Virtual Light Machine

Here are some of the useful keys for playing with the VLM:

```
F1 - Fullscreen toggle
F3 - Enable Music device capture
F4 - Show Music Input
F6 - Load State
F7 - Save State
F11 - Throttle toggle
F12 - Screenshot
O - option
P - pause
Z/Ctrl - A
X/ALT - B
C/Space - C
Arrow keys
Numeric keypad (. is #)
Q - three-fingered salute (used on the flashing CD with ?) to engage VLM
Esc - Quit
```

VLM: press Z (button A) twice to enter program mode, press O to engage
user programmable mode, then the numeric pad or numbers: first press is
bank, second press is effect in bank.

You can find the original manual for the Jaguar CD (and the VLM) in the [docs folder](docs).

For convenience, here is the short section covering use of the VLM:
![vlm1](https://user-images.githubusercontent.com/58846/122687733-dc761280-d20f-11eb-89e7-63c1f729cf41.png)
![vlm2](https://user-images.githubusercontent.com/58846/122687737-e0a23000-d20f-11eb-9c08-601d58d1f3fa.png)

## Notes on the Source Code

The content of this repository was originally retrieved from https://github.com/jaguar64/samples/tree/master/VLM.

Not all of the files in that dump are needed to build VLM and the core of the VLM itself is available only as an
executable `vlm.abs` file. In order to reconstruct the source of the VLM core I reversed engineered 'vlm.abs` with the help
of the symbols file 'vlm.syms' - an extremely useful thing to have as it means the routine and variable names can be retained from
the original. The reconstructed VLM source and accompanying images and binary objects are available in the [VLM folder](src/vlm).

It was possible to get everything to build with only minor modifications. Most of these were to make the source
comaptible with `rmac` since most of the code would originally have been written for the native jaguar assembler
`gasm`.

