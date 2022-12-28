#![no_std]
#![no_main]

use bsp::{
    entry,
    hal::{pio::ShiftDirection, Clock},
};
use defmt::*;
use defmt_rtt as _;
// use embedded_hal::digital::v2::OutputPin;
// use embedded_time::fixed_point::FixedPoint;
use panic_probe as _;

// Provide an alias for our BSP so we can switch targets quickly.
// Uncomment the BSP you included in Cargo.toml, the rest of the code does not need to change.
use rp_pico as bsp;
// use sparkfun_pro_micro_rp2040 as bsp;

use bsp::hal::{
    clocks::init_clocks_and_plls,
    gpio::{FunctionPio0, Pin},
    pac,
    pio::PIOBuilder,
    pio::PIOExt,
    pio::PinDir,
    sio::Sio,
    watchdog::Watchdog,
};

#[entry]
fn main() -> ! {
    info!("Program start");
    let mut pac = pac::Peripherals::take().unwrap();
    let core = pac::CorePeripherals::take().unwrap();
    let mut watchdog = Watchdog::new(pac.WATCHDOG);
    let sio = Sio::new(pac.SIO);

    // External high-speed crystal on the pico board is 12Mhz
    let external_xtal_freq_hz = 12_000_000u32;
    let clocks = init_clocks_and_plls(
        external_xtal_freq_hz,
        pac.XOSC,
        pac.CLOCKS,
        pac.PLL_SYS,
        pac.PLL_USB,
        &mut pac.RESETS,
        &mut watchdog,
    )
    .ok()
    .unwrap();

    let mut delay = cortex_m::delay::Delay::new(
        core.SYST,
        clocks.system_clock.freq().to_Hz(),
    );

    let pins = bsp::Pins::new(
        pac.IO_BANK0,
        pac.PADS_BANK0,
        sio.gpio_bank0,
        &mut pac.RESETS,
    );

    // setup

    // configure LED pin for Pio0.
    let _led: Pin<_, FunctionPio0> = pins.gpio16.into_mode();
    // PIN id for use inside of PIO
    let led_pin_id = 16;

    // Define some simple PIO program.
    let program = pio_proc::pio_asm!(
        ".side_set 1",
        ".wrap_target",
        "bit_loop:",
        "out x, 1         side 0 [2]", // T3 - 1
        "jmp !x do_zero   side 1 [1]", // T1 - 1
        "jmp bit_loop     side 1 [4]", // T2 - 1
        "do_zero:",
        "nop              side 0 [4]", // T2 - 1
        ".wrap"
    );

    // Initialize and start PIO
    let (mut pio, sm0, _, _, _) = pac.PIO0.split(&mut pac.RESETS);
    let installed = pio.install(&program.program).unwrap();
    let (mut sm, _, mut tx) = PIOBuilder::from_program(installed)
        .autopull(true)
        .pull_threshold(24)
        .out_shift_direction(ShiftDirection::Left)
        .side_set_pin_base(led_pin_id)
        .set_pins(led_pin_id, 0)
        .buffers(bsp::hal::pio::Buffers::OnlyTx)
        // .clock_divisor_fixed_point(0, 0)
        .clock_divisor_fixed_point(15, 160)
        .build(sm0);
    sm.set_pindirs([(led_pin_id, PinDir::Output)]);
    info!(
        "Starting the machine: {}",
        clocks.system_clock.freq().to_Hz()
    );
    info!("Starting the machine");
    sm.start();
    info!("Machine started");

    let mut red: u32 = 0;
    let mut blue: u32 = 0;
    let mut green: u32 = 0;
    let max_power: u32 = 32;
    // PIO runs in background, independently from CPU
    loop {
        red = (red + 4) % max_power;
        blue = (blue + 2) % max_power;
        green = (green + 1) % max_power;
        for i in 0..6 {
            tx.write(
                green << 24 | (red + 10 * i % max_power) << 16 | blue << 8,
            );
        }
        delay.delay_ms(55);
    }
}

// End of file
