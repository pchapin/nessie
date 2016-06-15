
// Generic modules

generic module X(/* int n, typedef t */) {
  uses interface I;
}
implementation {
    
    struct stuff {
        int a;
        int b, c;
        // t   d;
    };
 
    void do_something()
    {
        struct stuff object;
        object.a = 1;
        call I.handle_stuff(&object);
    }
}
