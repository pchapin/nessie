// If one comments out the 'SIZEOF unary_expression' in the grammar, this test case still
// passes. It appears that the syntactic predict on assignment_expression is failing and that
// stops the parsing process. No attempt is made to digest the remaining tokens. This seems
// wrong to me and I'm wondering if I'm missing something.
//
// In any case, use of the comma operator might be reducing the effectiveness of these tests. A
// failure to parse one of the subexpressions does not appear to cause the test to fail!

++X,
--X,
 &X,
 *X,
 +X,
 -X,
 ~X,
 !X,
sizeof( int ),
sizeof X,
sizeof( X + 1 )
