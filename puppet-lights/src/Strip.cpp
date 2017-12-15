#include "Strip.h"
#include <Arduino.h>

Strip::Strip(Adafruit_DotStar dotStar) {
  _dotStar = &dotStar;
  _lampCount = _dotStar->numPixels();
  _shown = true;
  }

Strip::~Strip() { }

void Strip::setLamp(uint32_t number, uint32_t color) {
  if (_shown) {
    _activeLampCount = 0;
    _shown = false;
    }
  uint32_t lamp = (number << 24) + color;
  _activeLamps[_activeLampCount++] = lamp;
  }

int numberOf(uint32_t lamp) {
  return (color >> 24) & 255;
  }

void Strip::show() {
  uint32
  _shown = true;
  }
