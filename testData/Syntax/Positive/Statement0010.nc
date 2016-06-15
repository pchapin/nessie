{
    if (X == Y) A = B + C;
    if (X == Y) {
        X;
        Y = B + C;
        Z;
    }
    
    if (X < Y) A++; else B++;
      
    if (X > Y) {
        A = B + C;
    }
    else B++;
    
    if (X != Y) {
        A = B + C;
    }
    else {
        A = B - C;
    }
    
    if (X == Y)
        if (A == B)
            C = A + B;
        else
            C = A - B;
}

