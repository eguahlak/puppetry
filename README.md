# Puppetry


## Developing

Currently, the working solution is to use the nix packages manager, with flakes. 
Follow the guides [here](https://nixos.org/download.html#nix-install-macos) and [here](https://nixos.wiki/wiki/Flakes)

Now we can setup a development environment using

```bash
nix develop
```

Building is as simple as 

```bash
cargo build
```

And if you have elf2u2 installed, then we put our code directly on the Raspberry Pi Pico, if it 
is in boot model (hold bootsel for 3 seconds on start)

```bash
cargo run --release
```

## Log 

- Followed the advice from [rp2040-project-template-nix](https://github.com/polygon/rp2040-project-template-nix/)

## References

More information about the [UF2](https://microsoft.github.io/uf2/)


