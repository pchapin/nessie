configuration X {
    provides interface I;
}
implementation {
    components A;
    
    I = A.J.I;
}
