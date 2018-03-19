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
  Serial.println("OK!");
  }

void showStripColor_B(uint32_t color) {
  for (int index = 0; index < NUMPIXELS_B; index++)
      strip_B.setPixelColor(index, color);
  strip_B.show();
  }

void showStripColor_M(uint32_t color) {
  for (int index = 0; index < NUMPIXELS_M; index++)
      strip_M.setPixelColor(index, color);
  strip_M.show();
  }

void showStripColor_F(uint32_t color) {
  for (int index = 0; index < NUMPIXELS_F; index++)
      strip_F.setPixelColor(index, color);
  strip_F.show();
  }

void showStripColor_P(uint32_t kolor) {
  for (int index = 0; index < NUMPIXELS_P; index++)
      strip_P.setPixelColor(index, kolor);
  strip_P.show();
  }

void showStripColor_S(uint32_t color) {
  for (int index = 0; index < NUMPIXELS_S; index++)
      strip_S.setPixelColor(index, color);
  strip_S.show();
  }

void showStripColor_L(uint32_t color) {
  for (int index = 0; index < 6; index++)
      strip_S.setPixelColor(index, color);
  strip_S.show();
  }

void showStripColor_R(uint32_t color) {
  for (int index = 6; index < 12; index++)
      strip_S.setPixelColor(index, color);
  strip_S.show();
  }


int count = 0;
int strip = 'X';
uint32_t color = 0;
int waitMillis = 100;

void loop() {
  if (count == 6) {
    if (strip == 'X' || strip == 'B') showStripColor_B(color);
    if (strip == 'X' || strip == 'M') showStripColor_M(color);
    if (strip == 'X' || strip == 'F') showStripColor_F(color);
    if (strip == 'X' || strip == 'P') showStripColor_P(color);
    if (strip == 'X' || strip == 'S' || strip == 'L') showStripColor_L(color);
    if (strip == 'X' || strip == 'S' || strip == 'R') showStripColor_R(color);
    count = 0;
    }
  delay(waitMillis);
  while (Serial.available()) {
    int c = Serial.read();
    Serial.println(c);
    if ('A' <= c && c <= 'Z') {
      strip = c;
      color = 0;
      }
    else if ('0' <= c && c <= '9') {
      color = 16*color + (c - '0');
      count++; 
      }
    else if ('a' <= c && c <= 'f') {
      color = 16*color + (10 + c - 'a');
      count++;
      }
    else break;
    }
  }



