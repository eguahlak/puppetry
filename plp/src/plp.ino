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
  // Do nothing, maybe out comment completely?
  }

bool isHex = false;
// reads a byte a byte at a time into a value.
// returns true if the byte was read.
boolean readInt(byte c, uint32_t & value, int & read) {
  if (isHex)
    if (read++ < 8) {
      value = (value << 4) | hexValueOf(c);
      return true;
    } else return false;
  else
    if (read++ < 4) {
      value = (value << 8) | c;
      return true;
    } else return false;
  }

int command = NOP;

int maskRead = 0;
int colorRead = 0;
uint32_t mask = 0;
uint32_t color = 0;


// action takes a byte and tries to perform an action. If the action
// is still waiting for inputs it return 0, else it returns a return type
// byte.
byte action (byte b) {
  switch (command) {
    case NOP:
      command = b;
      return 0;
    case SET:
      if (readInt(b, mask, maskRead)) return 0;
      if (readInt(b, color, colorRead)) return 0;
      // At this point both mask and color should be filled.
      runSetCommand(mask, color);
      maskRead = colorRead = 0;
      return OK;
    case TEST:
      return OK;
    case BINARY:
      isHex = false;
      return OK;
    case HEXADECIMAL:
      isHex = true;
      return OK;
    default:
      return KO;
    }
}

void serialEvent () {
  // Read command.
  while (Serial.available() > 0) {
    byte resp = action(Serial.read());
    if (resp) {
      // Indicated that an action is taken.
      Serial.write(resp);
      Serial.flush();
      command = NOP;
    }
  }
}
