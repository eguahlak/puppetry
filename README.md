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

## WS2812B Protocol

0 is 0.40 μs high + 0.85 μs low = 1.25 μs
1 is 0.80 μs high + 0.45 μs low = 1.25 μs

10 ticks = 1.25 μs = 8Mhz

Clock 125 Mhz, scale is 15



https://pdf1.alldatasheet.com/datasheet-pdf/view/1179113/WORLDSEMI/WS2812B.html

## Log 

- Followed the advice from [rp2040-project-template-nix](https://github.com/polygon/rp2040-project-template-nix/)
- Install a CMSIS-DAP prope: You can use a second Pico as a CMSIS-DAP debug probe by installing the following firmware on it: https://github.com/majbthrd/DapperMime/releases/download/20210225/raspberry_pi_pico-DapperMime.uf2

## References

More information about the [UF2](https://microsoft.github.io/uf2/)

### WS2812
How to get the WS2812 chip running with the pico:
[here](https://core-electronics.com.au/guides/how-to-use-ws2812b-rgb-leds-with-raspberry-pi-pico/)
Sadly, it's in micropython. But maybe we can improve

```python
@rp2.asm_pio(sideset_init=rp2.PIO.OUT_LOW, out_shiftdir=rp2.PIO.SHIFT_LEFT, autopull=True, pull_thresh=24)
def ws2812():
    T1 = 2
    T2 = 5
    T3 = 3
    wrap_target()
    label("bit_loop")
    out(x, 1)               .side(0)    [T3 - 1]
    jmp(not_x, "do_zero")   .side(1)    [T1 - 1]
    jmp("bit_loop")          .side(1)    [T2 - 1]
    label("do_zero")
    nop()                   .side(0)    [T2 - 1]
    wrap()
```



