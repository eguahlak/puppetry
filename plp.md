# PLP: Puppetry Light Protocol

This protocol describes how two entities can convey information about
lighting of a puppet theater. The sender will be referred as the
PUPPET-MASTER, or MASTER, the receiver will be referred to as the BASE
UTILITY LUMEN BEAM - provider, or BULB.

## Utilities

### Color

A color is 4 bytes of information, a red, a green and a blue component, 
the fourth byte is reserved for future information like face.

Example; 0xfffff00 is white.

### Target

The target represent the pixels being target of the operation. A target
is a 4 byte segment. The first 5 bytes represents the light arrays
targeted, each light array has it's own bit, except *Sidelys* which 
only have one bit for the left and right bit.

| Bit | Name      | Pixels |
|----:|:----------|-------:|
| 0   | Baglys    | 27     |
| 1   | Mellemlys | 27     |
| 2   | Forlys    | 27     |
| 3   | Scenelys  | 23     |
| 4   | Sidelys   | 24     |

The remaining 27 bit represent individual pixels in the array. The
first pixel colors the left most pixel and the second the right most
pixel, so on moving towards the center.

An array of 27 pixels is therefore enumerated like so.

L_5 L_7 L_9 ... L_17 C_18 R_16 ... R_10 R_8 R_6

The reason to use this system that a directional command, e.i. color the
left lights green, makes semantically sense no matter the number of
pixels in an array.


A light only on the left side of the scene, but on all arrays, except
the *Scenelys* can be targeted using this bit array.
```
-- arrays ---  ------------------  pixels  -------------------
1  1  1  0  1  1  0  1  0  1  0  1  0  1  0  1  0  1  0
B  M  F  P  S  L0 R0 L1 R1 L2 R2 L3 R3 L4 R4 L5 R5 L6 R6 .... 
```

Notice that the protocol only lits the left *sidelys*.

Targeting all pixels is simply the max integer 0xffffffff

## Protocol 
On beginning of the protocol BULB will setup the system and send
a single integer:
```
0x4F4B2100 -- OK!\0
```

After this MASTER is able to send commands to BULB. A command is 
a byte, from the command-table below. 

### Set 0x00
```
0x00 <target> <color> 
```
Instantly sets the <target> with the color <color> 

### Gradient 0x01
```
0x01 <time> <target> <color> 
```
Over `<time>` ms move color of all `<target>` from the previous color to the
new `<color>`.

### Test 0x02
```
0x02
```
BULB should return
```
0x4F4B2100 -- OK!\0
```




