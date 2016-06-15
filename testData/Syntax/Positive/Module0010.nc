module X {
    provides interface I;
}
implementation {
    int a;
    
    command void I.some_command(int b)
    {
        a = b;
    }
}

