
// Make sure we can use a previously defined typedef name in another typedef declaration.
typedef uint32_t  counter_t;
typedef counter_t buffer_counter_t;

// And now try to use the second typedef name in an object declaration.
buffer_counter_t count;

