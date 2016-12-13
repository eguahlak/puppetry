#include "PLP.hpp"

#define NOP    0
#define SET   'S'
#define GRADE 'G'
#define TEST  'T'
#define OK    '!'
#define KO    '?'

#define BINARY      'B'
#define HEXADECIMAL 'H'

PuppetLight light_B = PuppetLight(0, 6, 7, 27);
PuppetLight light_M = PuppetLight(1, 5, 7, 27);
PuppetLight light_F = PuppetLight(2, 4, 7, 27);
PuppetLight light_P = PuppetLight(3, 3, 7, 23);
PuppetLight light_S = PuppetLight(4, 2, 7, 12);

void setup() {
  Serial.begin(9600);
  }

int waitMillis = 100;
int command = 0;
int maskLeft = 0;
int colorLeft = 0;
uint32_t mask = 0;
uint32_t color = 0;

bool isHex = false;

void runSetCommand(uint32_t mask, uint32_t color) {
  light_B.doSet(mask, color);
  light_M.doSet(mask, color);
  light_F.doSet(mask, color);
  light_P.doSet(mask, color);
  light_S.doSet(mask, color);
  }

int hexValueOf(int c) {
  if ('0' <= c && c <= '9')  return c - '0';
  if ('a' <= c && c <= 'f')  return 10 + c - 'a';
  return 0;
  }

void loop() {
  if (maskLeft == 0 && colorLeft == 0) {
    switch (command) {
      case NOP:
        break;
      case SET:
        runSetCommand(mask, color);
        Serial.write(OK);
        Serial.flush();
        break;
      case TEST:
        Serial.write(OK);
        Serial.flush();
        break;
      case BINARY:
        isHex = false;
        Serial.write(OK);
        Serial.flush();
        break;
      case HEXADECIMAL:
        isHex = true;
        Serial.write(OK);
        Serial.flush();
        break;
      default:
        Serial.write(KO);
        Serial.flush();
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
          maskLeft = isHex ? 8 : 4;
          colorLeft = isHex ? 8 : 4;
          break;
        case TEST:
          maskLeft = 0;
          colorLeft = 0;
          break;
        }
      }
    else if (maskLeft > 0) {
      maskLeft--;
      if (isHex) mask = (mask << 4) | hexValueOf(c);
      else mask = (mask << 8) | c;
      }
    else if (colorLeft > 0) {
      colorLeft--;
      if (isHex) color = (color << 4) | hexValueOf(c);
      else color = color << 8 | c;
      }
    if (maskLeft == 0 && colorLeft == 0) break;
    }
  }
