#include <Adafruit_DotStar.h>
#include <SPI.h>         // COMMENT OUT THIS LINE FOR GEMMA OR TRINKET

#define NUMPIXELS_B 27 // Number of LEDs in strip_B
#define NUMPIXELS_M 27 // Number of LEDs in strip_M
#define NUMPIXELS_F 27 // Number of LEDs in strip_F
#define NUMPIXELS_P 23 // Number of LEDs in strip_P
#define NUMPIXELS_S 12 // Number of LEDs in strip_S

// Here's how to control the LEDs from any two pins:
#define CLOCKPIN   7

#define DATAPIN_B  6
#define DATAPIN_M  5
#define DATAPIN_F  4
#define DATAPIN_P  3
#define DATAPIN_S  2

#define NOP 0
#define SET 1
#define GRADE 2
#define TEST 255

Adafruit_DotStar strip_B = Adafruit_DotStar(NUMPIXELS_B, DATAPIN_B, CLOCKPIN, DOTSTAR_BRG);
Adafruit_DotStar strip_M = Adafruit_DotStar(NUMPIXELS_M, DATAPIN_M, CLOCKPIN, DOTSTAR_BRG);
Adafruit_DotStar strip_F = Adafruit_DotStar(NUMPIXELS_F, DATAPIN_F, CLOCKPIN, DOTSTAR_BRG);
Adafruit_DotStar strip_P = Adafruit_DotStar(NUMPIXELS_P, DATAPIN_P, CLOCKPIN, DOTSTAR_BRG);
Adafruit_DotStar strip_S = Adafruit_DotStar(NUMPIXELS_S, DATAPIN_S, CLOCKPIN, DOTSTAR_BRG);

void setup() {
  Serial.begin(9600);
  strip_B.begin(); // Initialize pins for output
  strip_M.begin();
  strip_F.begin();
  strip_P.begin();
  strip_S.begin();
  strip_B.show();  // Turn all LEDs off ASAP
  strip_M.show();
  strip_F.show();
  strip_P.show();
  strip_S.show();
  }

int waitMillis = 100;
int command = 0;
int maskLeft = 0;
int colorLeft = 0;
uint32_t mask = 0;
uint32_t color = 0;

void setMaskedPixelsColor(uint32_t mask, uint32_t color, Adafruit_DotStar strip, int rightPixel) {
  int leftPixel = 0;
  while (leftPixel < rightPixel) {
    if (mask & 0x1) strip.setPixelColor(leftPixel++, color);
    mask = mask >> 1;
    if (mask & 0x1) strip.setPixelColor(--rightPixel, color);
    mask = mask >> 1;
    }
  }

void parseCommand(uint32_t mask, uint32_t color) {
  uint32_t pixelMask = mask >> 5;
  if (mask & 0x01) setMaskedPixelsColor(pixelMask, color, strip_B, NUMPIXELS_B);
  if (mask & 0x02) setMaskedPixelsColor(pixelMask, color, strip_M, NUMPIXELS_M);
  if (mask & 0x04) setMaskedPixelsColor(pixelMask, color, strip_F, NUMPIXELS_F);
  if (mask & 0x08) setMaskedPixelsColor(pixelMask, color, strip_P, NUMPIXELS_P);
  if (mask & 0x10) setMaskedPixelsColor(pixelMask, color, strip_S, NUMPIXELS_S);
  }


void loop() {
  if (maskLeft == 0 && colorLeft == 0) {
    switch (command) {
      case NOP:
        break;
      case SET:
        parseCommand(mask, color);
        break;
      case TEST:
        Serial.write(7);
        break;
      }    
    mask = 0;
    color = 0;
    command = 0;
    }
  delay(waitMillis);
  while (Serial.available()) {
    int c = Serial.read();
    if (command == 0) {
      command = c;
      switch (command) {
        case NOP:
          maskLeft = 0;
          colorLeft = 0;
          break;
        case SET:
          maskLeft = 4;
          colorLeft = 4;
          break;
        case TEST:
          maskLeft = 0;
          colorLeft = 0;
          break;
        }
      }
    else if (maskLeft > 0) {
      maskLeft--;
      mask = (mask << 8) | c;
      }
    else if (colorLeft > 0) {
      colorLeft--;
      color = color << 8 | c;
      }
    if (maskLeft == 0 && colorLeft == 0) break;
    }
  }



