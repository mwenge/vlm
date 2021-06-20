# Virtual Light Machine (1995) by Jeff Minter
<img src="https://user-images.githubusercontent.com/58846/122686598-c82f1700-d209-11eb-89b5-7f965b1cbec6.jpg" height=200><img src="https://user-images.githubusercontent.com/58846/122686679-6622e180-d20a-11eb-93db-3bb82dfb53a0.gif" height=200>

This is the reconstructed source code for the Virtual Light Machine by Jeff Minter originally published in 1995 for the ill-fated Atari Jaguar.

The source code can be compiled into an executable that you can run in `t2k.exe`.

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

