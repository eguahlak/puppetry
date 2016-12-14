#ifndef PLP
#define PLP

#include <Arduino.h>
#include <Adafruit_DotStar.h>
#include <SPI.h>

class PuppetLight {
public:
  PuppetLight(int index, int dataPin, int clockPin, int pixelCount);
  ~PuppetLight();
  int getCount();
  void doSet(int32_t, int32_t);
  // grade(int time, int32_t mask, int32_t color);
  // tick();
private:
  int flag, count;
  Adafruit_DotStar* strip;
  };
/*
class Protocol {
public:
  Protocol();
  ~Protocol();
  bool hasCommand();
  bool isHex();
private:

  };
*/
#endif
