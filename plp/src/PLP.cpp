#include "PLP.hpp"

PuppetLight::PuppetLight(int index, int dataPin, int clockPin, int pixelCount) {
  flag = 1 << index;
  count = pixelCount;
  strip = new Adafruit_DotStar(pixelCount, dataPin, clockPin, DOTSTAR_BRG);
  strip->begin();
  strip->show();
  }

PuppetLight::~PuppetLight() {
  delete strip;
  }

void PuppetLight::doSet(int32_t mask, int32_t color) {
  if ((mask & flag) == 0) return;
  int leftPixel = 0;
  int rightPixel = count - 1;
  mask = mask >> 5;
  while (leftPixel < rightPixel) {
    if (mask & 1) strip->setPixelColor(leftPixel, color);
    leftPixel++;
    mask = mask >> 1;
    if (mask & 1) strip->setPixelColor(rightPixel, color);
    rightPixel--;
    mask = mask >> 1;
    }
  strip->show();
  }
