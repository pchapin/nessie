# 1 "./ClientC.nc"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "./ClientC.nc"

# 1 "/opt/tinyos-2.x/tos/lib/timer/Timer.h" 1
# 29 "/opt/tinyos-2.x/tos/lib/timer/Timer.h"
typedef struct { int notUsed; } TMilli;
typedef struct { int notUsed; } T32khz;
typedef struct { int notUsed; } TMicro;
# 3 "./ClientC.nc" 2
# 1 "./Client.h" 1




enum {
  AM_BENCHMARK = 6,
  TIMER_PERIOD_MILLI = 50
};

typedef nx_struct BenchmarkMsg {
  nx_uint16_t nodeid;
  nx_uint16_t counter;
} BenchmarkMsg;
# 4 "./ClientC.nc" 2

module ClientC {
  uses interface Boot;
  uses interface Timer<TMilli> as Timer0;
  uses interface Packet;
  uses interface AMPacket;
  uses interface AMSend;
  uses interface SplitControl as AMControl;
}
implementation {

  uint16_t counter;
  message_t pkt;
  bool busy = FALSE;

  event void Boot.booted() {
    call AMControl.start();
  }

  event void AMControl.startDone(error_t err) {
    if (err == SUCCESS) {
      call Timer0.startPeriodic(TIMER_PERIOD_MILLI);
    }
    else {
      call AMControl.start();
    }
  }

  event void AMControl.stopDone(error_t err) {
  }

  event void Timer0.fired() {
    counter++;
    if (!busy) {
      BenchmarkMsg* btrpkt = (BenchmarkMsg*)(call Packet.getPayload(&pkt, sizeof(BenchmarkMsg)));
      if (btrpkt == NULL) {
 return;
      }
      btrpkt->nodeid = TOS_NODE_ID;
      btrpkt->counter = counter;
      if (call AMSend.send(AM_BROADCAST_ADDR, &pkt, sizeof(BenchmarkMsg)) == SUCCESS) {
        busy = TRUE;
      }
    }
  }

  event void AMSend.sendDone(message_t* msg, error_t err) {
    if (&pkt == msg) {
      busy = FALSE;
    }
  }

}
