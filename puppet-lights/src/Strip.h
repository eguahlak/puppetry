#ifndef Strip_h
#define Strip_h
#include <Arduino.h>
#include <Adafruit_DotStar.h>
#include <SPI.h>         // COMMENT OUT THIS LINE FOR GEMMA OR TRINKET

class Strip {
public:
  Strip(Adafruit_DotStar dotStar);
  ~Strip();
  void setLamp(uint32_t number, uint32_t color);
  void show();
private:
  int _lampCount;
  bool _shown;
  Adafruit_DotStar* _dotStar;
  uint32_t _activeLamps[32];
  int _activeLampCount;
  };

#endif
