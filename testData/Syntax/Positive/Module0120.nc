module X {
}
implementation {
    // Make sure typedef names and ordinary names can hide each other properly.
    int outer;
    typedef int counter_t;
    void f()
    {
        int x;

        // The grammar regards this as a declaration without an init-declarator-list (which is
        // optional) and multiple type specifiers (which must be supported for things like
        // "unsigned int" and "short int" etc). As a result it does not add counter_t to the
        // list of ordinary identifiers for this scope.
        //
        int counter_t;       // No longer a type name.
//        ++counter_t;

        typedef char outer;  // Now a type name.
        outer ch;
        if (x == y) {
            typedef float counter_t;  // Once again a type name.
            outer var1;
            if (x == y) {
                counter_t var;
            }
        }
        var++;
    }
}
