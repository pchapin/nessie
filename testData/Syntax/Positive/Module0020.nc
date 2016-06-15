module X {
    provides interface I;
}
implementation {
    int a;
    
    task void do_something()
    {
        a++;
    }
}
