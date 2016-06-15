module X {
  uses interface I;
}
implementation {
    
    struct stuff {
        int a;
        int b, c;
    };
 
    void do_something()
    {
        struct stuff object;
        object.a = 1;
        call I.handle_stuff(&object);
    }
}
