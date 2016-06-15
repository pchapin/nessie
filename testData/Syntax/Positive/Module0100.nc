module X {
}
implementation {
    typedef int counter_t;
    typedef struct xyzzy {
        int a, b, c;
    } xyzzy_t;

    // Make sure names introduced by typedef are recognized as type names.    
    counter_t var1;
    xyzzy_t   var2;
    
    void f()
    {
        var1++;
        var2.a++;
    }
}
