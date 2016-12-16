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
  Serial.write(OK);
  }


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


#define DONE
#define WAITING
#define SKIPPED

bool isHex = false;
// reads a byte a byte at a time into a value.
// returns 1 if the byte was read, and 2 if the
void readInt(byte c, uint32_t & value, int & left) {
  if (left > 0) {
    if (isHex)
      value = (value << 4) | hexValueOf(c);
    else
      value = (value << 8) | c;
    left = left - 1;
    }
  }

int command = NOP;

int maskLeft = 0;
int colorLeft = 0;
uint32_t mask = 0;
uint32_t color = 0;

// action takes a byte and tries to perform an action. If the action
// is still waiting for inputs it return 0, else it returns a return type
// byte.
byte action (byte b) {
  switch (command) {
    case NOP:
      command = b;
      switch (command) {
        case TEST:
          return OK;
        case BINARY:
          isHex = false;
          return OK;
        case HEXADECIMAL:
          isHex = true;
          return OK;
        case SET:
          maskLeft = isHex ? 8 : 4;
          colorLeft = isHex ? 8 : 4;
          return 0;
      }
      return 0;
    case SET:
      if (maskLeft != 0) {
        readInt(b, mask, maskLeft);
        return 0;
      }
      if (colorLeft) {
        readInt(b, color, colorLeft);
        if (colorLeft) return 0;
        runSetCommand(mask, color);
        return OK;
        } else {
        return KO;
        }
    default:
      return KO;
    }
  }

void loop() {
  // Read command.
  while (Serial.available() > 0) {
    byte b = Serial.read();
    byte resp = action(b);
    if (resp) {
      // Indicated that an action is taken.
      Serial.write(resp);
      Serial.flush();
      command = NOP;
    }
  }
}
