
// This test explores the handling of line directives in implementations.

module X {
}
implementation {
    // For some reason the parser fails if a line directive is the first item here (2013-04-16)
    //# 42 "SomeFile.nc"
    int x;
    # 42 "SomeFile.nc"
    int y;
}
