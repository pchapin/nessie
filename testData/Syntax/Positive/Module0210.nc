# 1 "./ServerC.nc"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "./ServerC.nc"

# 1 "./Server.h" 1




enum {
  AM_BENCHMARK = 6
};

typedef nx_struct BenchmarkMsg {
  nx_uint16_t nodeid;
  nx_uint16_t counter;
} BenchmarkMsg;
# 3 "./ServerC.nc" 2

module ServerC {
  uses interface Boot;
  uses interface Leds;
  uses interface Packet;
  uses interface AMPacket;
  uses interface Receive;
  uses interface SplitControl as AMControl;
}
implementation {

  uint16_t counter;
  message_t pkt;
  bool busy = FALSE;

  void setLeds(uint16_t val) {







    if (val & 0x01)
      call Leds.led0On();
    else
      call Leds.led0Off();
    if (val & 0x02)
      call Leds.led1On();
    else
      call Leds.led1Off();
    if (val & 0x04)
      call Leds.led2On();
    else
      call Leds.led2Off();
  }

  event void Boot.booted() {
    call AMControl.start();
  }

  event void AMControl.startDone(error_t err) {
    if (err != SUCCESS) {
      call AMControl.start();
    }
  }

  event void AMControl.stopDone(error_t err) {
  }

  event message_t *Receive.receive(message_t *msg, void *payload, uint8_t len){
    if (len == sizeof(BenchmarkMsg)) {
      BenchmarkMsg *btrpkt = (BenchmarkMsg *)payload;
      setLeds(btrpkt->counter);
    }
    return msg;
  }
}
