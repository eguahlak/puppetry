#ifndef Strip_h
#define Strip_h

class Strip {
public:
  Strip(uint16_t lampCount, uint8_t pin);
  ~Strip();
  void setLamp(uint16_t number, int color);
  void show();
private:
  int _lampCount;
  };

#endif
