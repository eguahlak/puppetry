#include "Strip.h"
#include <Arduino.h>
#include <SPI.h>

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

Strip strip_B(Adafruit_DotStar(NUMPIXELS_B, DATAPIN_B, CLOCKPIN, DOTSTAR_BRG));
Strip strip_M(Adafruit_DotStar(NUMPIXELS_M, DATAPIN_M, CLOCKPIN, DOTSTAR_BRG));
Strip strip_F(Adafruit_DotStar(NUMPIXELS_F, DATAPIN_F, CLOCKPIN, DOTSTAR_BRG));
Strip strip_P(Adafruit_DotStar(NUMPIXELS_P, DATAPIN_P, CLOCKPIN, DOTSTAR_BRG));
Strip strip_S(Adafruit_DotStar(NUMPIXELS_S, DATAPIN_S, CLOCKPIN, DOTSTAR_BRG));

int count = 0;
int strip = 'X';
uint32_t lamp = 0;
int waitMillis = 10;

void setup() {
  Serial.begin(9600);
  Serial.println("OK!");
  }

void loop() {
  if (count >= 8) { //lamp index and color read
    if (strip == 'B') strip_B.setLamp(lamp);
    if (strip == 'M') strip_M.setLamp(lamp);
    if (strip == 'F') strip_F.setLamp(lamp);
    // if (strip == 'L') strip_L.setLamp(lamp);
    // if (strip == 'R') strip_R.setLamp(lamp);
    if (strip == 'P') strip_P.setLamp(lamp);
    count = 0;
    }
  delay(waitMillis);

  while (Serial.available()) {
    int c = Serial.read();
    // Serial.println(c);
    if ('A' <= c && c <= 'Z') {
      strip = c;
      lamp = 0;
      count = 0;
      }
    else if ('0' <= c && c <= '9') {
      lamp = 16*lamp + (c - '0');
      count++;
      }
    else if ('a' <= c && c <= 'f') {
      lamp = 16*lamp + (10 + c - 'a');
      count++;
      }
    else if (c == '!') {
      strip_B.show();
      strip_M.show();
      strip_F.show();
      // strip_L.show();
      // strip_R.show();
      strip_P.show();
      count = 0;
      }
    else break;
    }
  }
