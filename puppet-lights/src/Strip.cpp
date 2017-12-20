#include "Strip.h"
#include <Arduino.h>

Strip::Strip(Adafruit_DotStar dotStar) {
  _dotStar = &dotStar;
  _dotStar->begin();
  _dotStar->show();
  _lampCount = _dotStar->numPixels();
  _shown = true;
  }

Strip::~Strip() { }

void Strip::setLamp(uint32_t lamp) {
  if (_shown) {
    _activeLampCount = 0;
    _shown = false;
    }
  if (_activeLampCount < _lampCount) _activeLamps[_activeLampCount++] = lamp;
  }

uint32_t indexOf(uint32_t lamp) {
  return (lamp >> 24) & 255;
  }

uint32_t indexOn(uint32_t lamp, uint32_t index) {
  return (lamp & 0xffffff) | (index << 24);
  }

uint32_t redOf(uint32_t lamp) {
  return (lamp >> 16) & 255;
  }

uint32_t greenOf(uint32_t lamp) {
  return (lamp >> 8) & 255;
  }

uint32_t blueOf(uint32_t lamp) {
  return lamp & 255;
  }

void Strip::show() {
  if (_activeLampCount == 0) {
    for (int i = 0; i < _activeLampCount; i++)
        _dotStar->setPixelColor(indexOf(_activeLamps[i]), _activeLamps[i] & 0xffffff);
    }
  uint32_t firstColor = _activeLamps[0] & 0xffffff;
  uint32_t firstIndex = indexOf(_activeLamps[0]);
  for (uint32_t left = 0; left <= firstIndex; left++) {
    _dotStar->setPixelColor(left, firstColor);
    }
  uint32_t lastColor = _activeLamps[_activeLampCount - 1] & 0xffffff;
  uint32_t lastIndex = indexOf(_activeLamps[_activeLampCount - 1]);
  for (uint32_t right = lastIndex; right >= 0; right--) {
    _dotStar->setPixelColor(right, lastColor);
    }
  for (int number = 0; number <= _activeLampCount -2; number++) {
    uint32_t lamp1 = _activeLamps[number];
    uint32_t lamp2 = _activeLamps[number + 1];
    uint32_t index1 = indexOf(lamp1);
    uint32_t index2 = indexOf(lamp2);

    uint32_t r1 = redOf(lamp1);
    uint32_t r2 = redOf(lamp2);
    uint32_t g1 = greenOf(lamp1);
    uint32_t g2 = greenOf(lamp2);
    uint32_t b1 = blueOf(lamp1);
    uint32_t b2 = blueOf(lamp2);

    for (uint32_t index = index1; index < index2; index++) {
      uint32_t r = (r1 + (index - index1)*(r2 - r1)/(index2 - index1)) & 255;
      uint32_t g = (g1 + (index - index1)*(g2 - g1)/(index2 - index1)) & 255;
      uint32_t b = (b1 + (index - index1)*(b2 - b1)/(index2 - index1)) & 255;
      _dotStar->setPixelColor(index, (r << 16) | (g << 8) | b);
      }
    _shown = true;
    }
  }
