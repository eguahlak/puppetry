#include "PLP.hpp"

#define RED   0x00ff0000
#define GREEN 0x0000ff00
#define BLUE  0x000000ff

// RED   = 0x0000ff00
// BLUE  = 0x000000ff
// GREEN = 0x00ff0000

int32_t fixColor (int32_t color) {
  // Because colors are mixed up, we fix it here
  return ((color & RED) >> 8) | ((color & GREEN) << 8) | (color & BLUE);
  }

PuppetLight::PuppetLight(int index, int dataPin, int clockPin, int pixelCount) {
  flag = 1 << index;
  count = pixelCount;
  strip = new Adafruit_DotStar(pixelCount, dataPin, clockPin, DOTSTAR_BRG);
  strip->begin();
  }

PuppetLight::~PuppetLight() {
  delete strip;
  }

void PuppetLight::doSet(int32_t mask, int32_t color) {
  if ((mask & flag) == 0) return;
  int leftPixel = 0;
  int rightPixel = count - 1;
  mask = mask >> 5;
  while (leftPixel <= rightPixel) {
    if (mask & 1) strip->setPixelColor(leftPixel, fixColor(color));
    leftPixel++;
    mask = mask >> 1;
    if (mask & 1) strip->setPixelColor(rightPixel, fixColor(color));
    rightPixel--;
    mask = mask >> 1;
    }
  strip->show();
  }

