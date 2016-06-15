module X {
}
implementation {
    // Make sure typedef names introduced in an outer scope are recognized.
    typedef int counter1_t;
    
    void f()
    {
        counter1_t var;
        
        if (x == y) {
            counter1_t var1;
        }
        var++;
    }
}
