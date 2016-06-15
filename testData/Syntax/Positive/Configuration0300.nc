# 1 ".\\ServerAppC.nc"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 ".\\ServerAppC.nc"

# 1 ".\\/Server.h" 1

// This is the preprocessed output of an actual (simple) program.

enum {
  AM_BENCHMARK = 6
};

typedef nx_struct BenchmarkMsg {
  nx_uint16_t nodeid;
  nx_uint16_t counter;
} BenchmarkMsg;
# 3 ".\\ServerAppC.nc" 2

configuration ServerAppC {
}
implementation {
  components MainC;
  components LedsC;
  components ServerC as App;
  components ActiveMessageC;
  components new AMReceiverC(AM_BENCHMARK);

  App.Boot -> MainC;
  App.Leds -> LedsC;
  App.AMControl -> ActiveMessageC;
  App.Receive -> AMReceiverC;
}

