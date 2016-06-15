grammar NesC;

options {
    output = AST;
}

tokens {
    // C99 reserved words except _Bool, _Complex, and _Imaginary.
    AUTO           = 'auto';
    BREAK          = 'break';
    CASE           = 'case';
    CHAR           = 'char';
    CONST          = 'const';
    CONTINUE       = 'continue';
    DEFAULT        = 'default';
    DO             = 'do';
    DOUBLE         = 'double';
    ELSE           = 'else';
    ENUM           = 'enum';
    EXTERN         = 'extern';
    FLOAT          = 'float';
    FOR            = 'for';
    GOTO           = 'goto';
    IF             = 'if';
    INLINE         = 'inline';
    INT            = 'int';
    LONG           = 'long';
    REGISTER       = 'register';
    RESTRICT       = 'restrict';
    RETURN         = 'return';
    SHORT          = 'short';
    SIGNED         = 'signed';
    SIZEOF         = 'sizeof';
    STATIC         = 'static';
    STRUCT         = 'struct';
    SWITCH         = 'switch';
    TYPEDEF        = 'typedef';
    UNION          = 'union';
    UNSIGNED       = 'unsigned';
    VOID           = 'void';
    VOLATILE       = 'volatile';
    WHILE          = 'while';

    // gcc extensions needed by some programs.
    GCCATTRIBUTE    = '__attribute__';
    BUILTIN_VA_LIST = '__builtin_va_list';
    BUILTIN_VA_ARG  = '__builtin_va_arg';

    // The 'nx' types are built into the nesC compiler (they require special handling by the
    // compiler). I'm not clear if the exact width types are built-in or not. However, the nesC
    // compiler appears to be fine seeing 'typedef signed char int8_t;' Nessie treats the
    // exact width types as built-in, but has special handling of typedef to deal with the case
    // above (otherwise it can't parse it because there are only type specifiers and no declared
    // name).
    //
    // An alternative approach would be to not build in the exact width types and require the
    // programmer to include <stdint.h> whenever they are needed. This appears to be contrary to
    // normal nesC practice.
    //
    // If you change here, be sure to update the 'type_specifier' production.
    //
    INT8_T         = 'int8_t';
    INT16_T        = 'int16_t';
    INT32_T        = 'int32_t';
    INT64_T        = 'int64_t';

    UINT8_T        = 'uint8_t';
    UINT16_T       = 'uint16_t';
    UINT32_T       = 'uint32_t';
    UINT64_T       = 'uint64_t';

    NX_INT8_T      = 'nx_int8_t';
    NX_INT16_T     = 'nx_int16_t';
    NX_INT32_T     = 'nx_int32_t';
    NX_INT64_T     = 'nx_int64_t';

    NX_UINT8_T     = 'nx_uint8_t';
    NX_UINT16_T    = 'nx_uint16_t';
    NX_UINT32_T    = 'nx_uint32_t';
    NX_UINT64_T    = 'nx_uint64_t';

    NXLE_INT8_T    = 'nxle_int8_t';
    NXLE_INT16_T   = 'nxle_int16_t';
    NXLE_INT32_T   = 'nxle_int32_t';
    NXLE_INT64_T   = 'nxle_int64_t';
    
    NXLE_UINT8_T   = 'nxle_uint8_t';
    NXLE_UINT16_T  = 'nxle_uint16_t';
    NXLE_UINT32_T  = 'nxle_uint32_t';
    NXLE_UINT64_T  = 'nxle_uint64_t';
    
    // nesC extensions to Standard C
    ABSTRACT       = 'abstract';       // Reserved for future use.
    AS             = 'as';
    ASYNC          = 'async';
    ATOMIC         = 'atomic';
    CALL           = 'call';
    COMMAND        = 'command';
    COMPONENT      = 'component';
    COMPONENTS     = 'components';
    CONFIGURATION  = 'configuration';
    EVENT          = 'event';
    GENERIC        = 'generic';
    IMPLEMENTATION = 'implementation';
    INTERFACE      = 'interface';
    MODULE         = 'module';
    NEW            = 'new';
    NORACE         = 'norace';
    NX_STRUCT      = 'nx_struct';
    NX_UNION       = 'nx_union';
    POST           = 'post';
    PROVIDES       = 'provides';
    SIGNAL         = 'signal';
    TASK           = 'task';
    USES           = 'uses';
        
    // Punctuators
    AMP            = '&';    // This token has multiple semantic purposes.
    AND            = '&&';
    ARROW          = '->';
    ASSIGN         = '=';
    ATTRIBUTE      = '@';
    BITANDASSIGN   = '&=';
    BITCOMPLEMENT  = '~';
    BITOR          = '|';
    BITORASSIGN    = '|=';
    BITXOR         = '^';
    BITXORASSIGN   = '^=';
    COMMA          = ',';
    DIVASSIGN      = '/=';
    DIVIDE         = '/';
    DOT            = '.';
    ELLIPSIS       = '...';
    EQUAL          = '==';
    GREATER        = '>';    // This token has multiple semantic purposes.
    GREATEREQUAL   = '>=';
    HASH           = '#';
    LBRACE         = '{';
    LBRACKET       = '[';
    LESS           = '<';    // This token has multiple semantic purposes.
    LESSEQUAL      = '<=';
    LPARENS        = '(';
    LSHIFT         = '<<';
    LSHIFTASSIGN   = '<<=';
    MINUS          = '-';
    MINUSASSIGN    = '-=';
    MINUSMINUS     = '--';
    MODASSIGN      = '%=';
    MODULUS        = '%';
    MULASSIGN      = '*=';
    NOT            = '!';
    NOTEQUAL       = '!=';
    OR             = '||';
    PLUS           = '+';
    PLUSASSIGN     = '+=';
    PLUSPLUS       = '++';
    RBRACE         = '}';
    RBRACKET       = ']';
    RPARENS        = ')';
    RSHIFT         = '>>';
    RSHIFTASSIGN   = '>>=';
    STAR           = '*';    // This token has multiple semantic purposes.
    
    // Pseudo-tokens used during AST construction.
    ADDRESS_OF;
    ARGUMENT_LIST;
    ARRAY_ELEMENT_SELECTION;
    CAST;
    COMPOUND_STATEMENT;
    COMPONENT_ARGUMENTS;
    COMPONENT_DECLARATION;
    COMPONENT_DEFINITION;
    COMPONENT_INSTANTIATION;
    COMPONENT_KIND;
    COMPONENT_PARAMETER_LIST;
    CONNECTION;
    DECLARATION;
    DECLARATOR;
    DECLARATOR_ARRAY_MODIFIER;
    DECLARATOR_LIST;
    DECLARATOR_PARAMETER_LIST_MODIFIER;
    DEREFERENCE;
    DYNAMIC_IDENTIFIER_PATH;
    ENUMERATOR;
    FILE;
    FOR_INITIALIZE;
    FOR_CONDITION;
    FOR_ITERATION;
    FUNCTION_DEFINITION;
    IDENTIFIER_PATH;
    INTERFACE_TYPE;
    INIT_DECLARATOR;
    INITIALIZER_LIST;
    LABELED_STATEMENT;
    LINE_DIRECTIVE;
    NULL;             // Used as a placeholder during tree manipulations by others.
    PARAMETER;
    PARAMETER_LIST;
    POINTER_QUALIFIER;
    POST_DECREMENT;
    POST_INCREMENT;
    POSTFIX_EXPRESSION;
    PRE_DECREMENT;
    PRE_INCREMENT;
    SIZEOF_TYPE;
    SIZEOF_EXPRESSION;
    SPECIFICATION;
    STATEMENT;
    TYPE_NAME;
    UNARY_PLUS;
    UNARY_MINUS;
}

@parser::header {
    package edu.vtc.nesc.parser;
    import java.util.LinkedList;
}

@lexer::header {
    package edu.vtc.nesc.parser;
}

@parser::members {
    // This is mostly just a placeholder.
    private final int VERSION = 1;

    private ParserSymbolsManager symbols;
    
    // The global symbol table contains information about global symbols defined in other files.
    public void setSymbols(ParserSymbolsManager globalSymbols)
    {
        symbols = globalSymbols;
    }

//    // The following two magic methods, together with the @rulecatch section below cause the
//    // parser to exit immediately with an exception when an error is encountered. This is useful
//    // for testing but is probably not desired in a production system. I'm not sure right now
//    // how to provide both behaviors in the same executable. I'll figure that out later!
//    //
//    protected Object recoverFromMismatchedToken(IntStream input, int ttype, BitSet follow)
//        throws RecognitionException
//    {
//        throw new MismatchedTokenException(ttype, input);
//    }
//    
//    public Object recoverFromMismatchedSet(IntStream input, RecognitionException e, BitSet follow)
//        throws RecognitionException
//    {
//        throw e;
//    }

    // The following two overrides provide enhanced error messages that are useful for debugging
    // grammar problems. The messages produced are not very suitable for end users so these
    // methods should probably be removed before a "production" version of Nessie is released.
    //
    // If you run ANTLR with the -dfa option, it will generate DOT files showing decision state
    // diagrams. You can use these files to look up a particular decision number to get more
    // information about what the parser was attempting to do when it encountered the error.
    // See Section 10.2 on page 245 of the Definitive ANTLR book (for ANTLR v3).
    //
    @Override
    public String getErrorMessage(RecognitionException e, String[] tokenNames)
    {
        List stack = getRuleInvocationStack(e, this.getClass().getName());
        String msg = null;
        if (e instanceof NoViableAltException) {
            NoViableAltException nvae = (NoViableAltException)e;
            msg = " no viable alt; token=" + e.token +
                  " (decision=" + nvae.decisionNumber +
                  " state " + nvae.stateNumber + ")" +
                  " decision=<<" + nvae.grammarDecisionDescription + ">>";
        }
        else {
            msg = super.getErrorMessage(e, tokenNames);
        }
        return stack + " " + msg;
    }

    @Override
    public String getTokenErrorDisplay(Token t)
    {
        return t.toString();
    }
}

//@parser::rulecatch {
//    catch (RecognitionException e) {
//        throw e;
//    }
//}

@lexer::members {
    // This is mostly just a placeholder.
    private final int VERSION = 1;
}

/* ================== */
/* Expression grammar */
/* ================== */

primary_expression
    :    identifier
    |    CONSTANT
    |    STRING_LITERAL
    |    CHARACTER_LITERAL
    |    '(' expression ')' -> expression;

// NOTE: This grammar is rather permissive about where call_kind can occur.
//
// The POSTFIX_EXPRESSION pseudo-token is needed to group all the postfix_expression_modifiers
// together with their primary_expression. This is required so that one postfix_expression can
// be distinguished from another in the ASTs for the various binary operators.
//
postfix_expression
    :    call_kind? primary_expression postfix_expression_modifier*
            -> ^(POSTFIX_EXPRESSION call_kind? primary_expression postfix_expression_modifier*)
    |    BUILTIN_VA_ARG '(' expression ',' type_name ')'  // Gcc extension.
            -> ^(BUILTIN_VA_ARG expression type_name);

postfix_expression_modifier
    :   '[' expression ']' -> ^(ARRAY_ELEMENT_SELECTION expression)
    |   '(' argument_expression_list? ')' -> ^(ARGUMENT_LIST argument_expression_list?)
    |   '.' identifier -> ^('.' identifier)
    |   '->' identifier -> ^('->' identifier)
    |   '++'
    |   '--';

call_kind
    :    CALL
    |    POST
    |    SIGNAL;
        
argument_expression_list
    :    assignment_expression (','! assignment_expression)*;
    
unary_expression
    :    '++' unary_expression -> ^(PRE_INCREMENT unary_expression)
    |    '--' unary_expression -> ^(PRE_DECREMENT unary_expression)
    |    '&'  cast_expression  -> ^(ADDRESS_OF    cast_expression )
    |    '*'  cast_expression  -> ^(DEREFERENCE   cast_expression )
    |    '+'  cast_expression  -> ^(UNARY_PLUS    cast_expression )
    |    '-'  cast_expression  -> ^(UNARY_MINUS   cast_expression )
    |    ('~'^ | '!'^) cast_expression
    |    (SIZEOF '(' type_name ')') => SIZEOF '(' type_name ')' -> ^(SIZEOF_TYPE type_name)
    |    SIZEOF unary_expression -> ^(SIZEOF_EXPRESSION unary_expression)
    |    postfix_expression;

// It is convenient to put the cast_expression first in the AST because that way the various
// tokens composing the type_name do not have to be wrapped in a pseudo-token. Note that the
// expression should be entirely contained inside its own tree.
//
cast_expression
    :    ('(' type_name ')') =>
          '(' type_name ')' cast_expression -> ^(CAST cast_expression type_name)
    |    unary_expression;
    
multiplicative_expression
    :    cast_expression ( ('*'^ | '/'^ | '%'^) cast_expression)*;
    
additive_expression
    :    multiplicative_expression ( ('+'^ | '-'^) multiplicative_expression)*;
    
shift_expression
    :    additive_expression ( ('<<'^ | '>>'^) additive_expression)*;
    
relational_expression
    :    shift_expression ( ('<'^ | '>'^ | '<='^ | '>='^) shift_expression)*;
    
equality_expression
    :    relational_expression ( ('=='^ | '!='^) relational_expression)*;
    
and_expression
    :    equality_expression ( '&'^ equality_expression)*;
    
xor_expression
    :    and_expression ( '^'^ and_expression)*;
    
or_expression
    :    xor_expression ( '|'^ xor_expression)*;
    
logical_and_expression
    :    or_expression ( '&&'^ or_expression)*;
    
logical_or_expression
    :    logical_and_expression ( '||'^ logical_and_expression)*;
    
conditional_expression
    :    logical_or_expression ('?'^ expression ':'! conditional_expression)?;
    
assignment_expression
    :    (unary_expression assignment_operator) =>
          unary_expression assignment_operator assignment_expression
             -> ^(assignment_operator unary_expression assignment_expression)
    |    conditional_expression;

assignment_operator
    :    '=' | '*=' | '/=' | '%=' | '+=' | '-=' | '<<=' | '>>=' | '&=' | '^=' | '|=';

expression
    :    assignment_expression ((',' assignment_expression) => ','^ assignment_expression)*;
    
constant_expression
    :    conditional_expression;
    
/* =================== */
/* Declaration grammar */
/* =================== */

// This grammar allows nested function definitions. They need to be semantically excluded.
// This was done so that all declarations are produced by a single rule where the declaredNames
// variable is defined. Thus all declarators encountered will be able to use declaredNames
// without issue.
//
// The DECLARATION pseudo-token is needed to clearly distinguish declarations from expression
// statements (which are marked with the STATEMENT pseudo-token).
//    
// The FUNCTION_DEFINITION pseudo-token is needed to distinguish function definitions from other
// kinds of declarations. This is necessary because, during rewriting, function definitions are
// not terminated with a ';' while other kinds of declarations are so terminated.
//
// TODO: Currently the gcc_attributes are not part of the AST.
// TODO: How are names introduced in parameter lists handled? Are they going into symbol tables?
declaration
scope { LinkedList<String> declaredNames;
        boolean inStructDeclaration; }
    :    (declaration_specifiers gcc_attributes? init_declarator_list? ';') =>
         { $declaration::declaredNames = new LinkedList<String>();
           $declaration::inStructDeclaration = false;
         }
         // The init_declarator_list is optional because of, for example, structure definitions.
         declaration_specifiers gcc_attributes? init_declarator_list? ';'
             {
               // Inefficient, but how many declarators will be in a declaration, honestly?
               for (int i = 0; i < $declaration::declaredNames.size(); ++i) {
                   symbols.addIdentifier($declaration::declaredNames.get(i));
               }
             }
             -> ^(DECLARATION declaration_specifiers init_declarator_list?)

    |    { $declaration::declaredNames = new LinkedList<String>();
           $declaration::inStructDeclaration = false;
         }
         function_definition
             {
               // Inefficient, but how many declarators will be in a declaration, honestly?
               for (int i = 0; i < $declaration::declaredNames.size(); ++i) {
                   symbols.addIdentifier($declaration::declaredNames.get(i));
               }
             }
             -> ^(DECLARATION ^(FUNCTION_DEFINITION function_definition))

    |    // The init_declarator_list is optional so 'typedef signed char int8_t;' will parse.
         // See the comment in the 'tokens' section of this grammar near the INT8_T token.
         //
         (TYPEDEF declaration_specifiers ';') => TYPEDEF declaration_specifiers ';'
             -> ^(DECLARATION TYPEDEF declaration_specifiers)

    |    { $declaration::declaredNames = new LinkedList<String>();
           $declaration::inStructDeclaration = false;
         }
         TYPEDEF declaration_specifiers gcc_attributes? init_declarator_list gcc_attributes? ';'
             {
               // Inefficient, but how many declarators will be in a declaration, honestly?
               for (int i = 0; i < $declaration::declaredNames.size(); ++i) {
                   symbols.addType($declaration::declaredNames.get(i));
               }
             }
             -> ^(DECLARATION TYPEDEF declaration_specifiers init_declarator_list);
    
// nesC allows declarations to be marked as "default" in certain situations. Currently doing so
// causes problems in blocks where declarations have to be distinguished from statements. A
// labeled_statement can start with DEFAULT ':'. I don't understand why this is a problem,
// though, since no c_style_declaration_specifier can start with ':'. Why isn't the second
// lookahead token sufficient to disambiguate?
//
declaration_specifiers
    :    /* DEFAULT? */ c_style_declaration_specifier+;

c_style_declaration_specifier
    :    storage_class_specifier
    |    type_specifier
    |    type_qualifier
    |    function_specifier;
  
// The DECLARATOR_LIST pseudo-token is needed as a container for a list of declarators. This
// is needed to distinguish the declarators in a declaration from the various declaration
// specifiers that preceed them.
//  
init_declarator_list
    :    init_declarator (',' init_declarator)* -> ^(DECLARATOR_LIST init_declarator+);
    
// The INIT_DECLARATOR pseudo-token is needed to package (declarator, initializer) pairs so
// that commas can be inserted into rewritten declarations properly. They also help to
// distinguish initializers from declarators.
//
init_declarator
    :    declarator attributes? ('=' initializer)? -> ^(INIT_DECLARATOR declarator initializer?);
    
storage_class_specifier
    :    EXTERN
    |    STATIC
    |    AUTO
    |    REGISTER
    |    ASYNC
    |    COMMAND
    |    EVENT
    |    NORACE
    |    TASK;
    
type_specifier
    :   VOID
    |   CHAR
    |   SHORT
    |   INT
    |   LONG
    |   SIGNED
    |   UNSIGNED
    |   FLOAT
    |   DOUBLE

    |   INT8_T
    |   INT16_T
    |   INT32_T
    |   INT64_T

    |   UINT8_T
    |   UINT16_T
    |   UINT32_T
    |   UINT64_T

    |   NX_INT8_T
    |   NX_INT16_T
    |   NX_INT32_T
    |   NX_INT64_T

    |   NX_UINT8_T
    |   NX_UINT16_T
    |   NX_UINT32_T
    |   NX_UINT64_T

    |   NXLE_INT8_T
    |   NXLE_INT16_T
    |   NXLE_INT32_T
    |   NXLE_INT64_T

    |   NXLE_UINT8_T
    |   NXLE_UINT16_T
    |   NXLE_UINT32_T
    |   NXLE_UINT64_T

    |   BUILTIN_VA_LIST    // Gcc extension.

    |   struct_or_union_specifier
    |   enum_specifier
    |   typedef_name;

// Really structure tags should be in their own name space (maybe tag_identifier?) that is
// distinguished from other identifiers using a semantic predicate. In any case they can be type
// names (for example) so using the 'identifier' production isn't appropriate.
//
struct_or_union_specifier
    :    struct_or_union '{' struct_declaration_list? '}'
             -> ^(struct_or_union struct_declaration_list?)
    |    struct_or_union RAW_IDENTIFIER (/* attributes? */ '{' struct_declaration_list? '}')?
             -> ^(struct_or_union RAW_IDENTIFIER /* attributes? */ struct_declaration_list?)
    |    STRUCT '@' RAW_IDENTIFIER '{' struct_declaration_list? '}'
             -> ^(STRUCT '@' RAW_IDENTIFIER struct_declaration_list?);
    
struct_or_union
    :    STRUCT | UNION | NX_STRUCT | NX_UNION;

// BUG: If structure declarations are nested the value of inStructDeclaration is mishandled when
// the inner declaration is left.
//
struct_declaration_list
    :    { $declaration::inStructDeclaration = true; }
         (line_directive_mini | struct_declaration)+
         { $declaration::inStructDeclaration = false; };
    
struct_declaration
    :    specifier_qualifier_list struct_declarator_list ';'
            -> ^(DECLARATION specifier_qualifier_list struct_declarator_list);
// I'm not sure what language features these productions are intended to support.
//    |    declarator attributes ';'
//            -> ^(DECLARATION declarator attributes)
//    |    declarator ':' constant_expression attributes ';'
//            -> ^(DECLARATION declarator constant_expression attributes);

specifier_qualifier_list
    :    (type_specifier | type_qualifier)+;
    
// Reuse the DECLARATOR_LIST pseudo-token here to wrap declarator lists in structure
// definitions. In this case the declarators are directly beneath the DECLARATOR_LIST token
// rather than being wrapped inside INIT_DECLARATOR tokens as is done with ordinary
// declarations.
//
struct_declarator_list
    :    struct_declarator (',' struct_declarator)* -> ^(DECLARATOR_LIST struct_declarator+);
    
struct_declarator
    :    declarator (':' constant_expression)?
    |    ':' constant_expression;
    
enum_specifier
    :    ENUM '{' enumerator_list ','? '}'
             -> ^(ENUM enumerator_list)
    |    ENUM identifier (/* attributes? */ '{' enumerator_list ','? '}')?
             -> ^(ENUM identifier /* attributes? */ enumerator_list?);
    
enumerator_list
    :    enumerator (','! enumerator)*;
    
// The ENUMERATOR pseudo-token is needed to wrap up an enumerator name with its value (if one
// is provided. Otherwise the names and values are not easy to distinguish when they appear in
// an enumerator_list.
//
enumerator
    :    identifier ('=' constant_expression)? -> ^(ENUMERATOR identifier constant_expression?);    
type_qualifier
    :    CONST
    |    RESTRICT
    |    VOLATILE;
    
function_specifier
    :    INLINE;

declarator
    :    pointer? direct_declarator -> ^(DECLARATOR pointer? direct_declarator);

direct_declarator
    :   direct_declarator_identifier direct_declarator_modifier*;

// The IDENTIFIER_PATH pseudo-token is needed to group the identifiers that comprise a qualified
// name. This keeps them nicely separated from the other entities in the declarations where they
// are used. Strictly speaking this pseudo-token is probably not necessary, but it makes things
// a little easier.
//
// BUG? The code here avoids adding direct_declarator_identifiers for structure members to the
// declaredNames list. This is fine, but shouldn't direct_declarator_identifiers inside function
// parameter lists also be skipped?
//
direct_declarator_identifier
    :   myIdName=identifier ('.' identifier)?
            { if (!$declaration::inStructDeclaration) {
                  $declaration::declaredNames.add($myIdName.text);
              }
            } -> ^(IDENTIFIER_PATH identifier+)
    |   '(' declarator ')' -> declarator;

// Pseudo-tokens are needed to distinguish between declarator modifies that otherwise have
// subtle syntactic differences. There is no other regular token that can be naturally used
// to distinguish these differences.
//
direct_declarator_modifier
    :   ('[' constant_expression? ']') => '[' constant_expression? ']'
            -> ^(DECLARATOR_ARRAY_MODIFIER constant_expression?)
    |   ('[' generic=parameter_list ']')? '(' normal=parameter_list ')'
            -> ^(DECLARATOR_PARAMETER_LIST_MODIFIER $normal);

// The POINTER_QUALIFIER pseudo-token is needed to distinguish the use of '*' in declarations
// from its use in expressions. The rewriting associated with pointer declarations is quite
// different than what is done in expressions (particularly multiplication!).
//    
pointer
    :    '*' type_qualifier_list? pointer -> ^(POINTER_QUALIFIER type_qualifier_list? pointer)
    |    '*' type_qualifier_list? -> ^(POINTER_QUALIFIER type_qualifier_list?);
    
type_qualifier_list
    :    type_qualifier+;
    
// The PARAMETER_LIST pseudo-token is needed to properly deal with empty parameter lists (it
// serves as a placeholder in that case). Also, direct_declarator_modifier entails two parameter
// lists (one for generic parameters, etc) and they need to be distinguished. Notice that
// the ellipsis is only allowed if there is at least one ordinary parameter declaration.
//
parameter_list
    :    parameter_declaration (',' parameter_declaration)* (',' ELLIPSIS)?
            -> ^(PARAMETER_LIST parameter_declaration+ ELLIPSIS?)
    |
            -> ^(PARAMETER_LIST) ;

// The PARAMETER pseudo-token is needed to clearly distinguish one parameter from another in a
// parameter list. Parameter declarations are potentially complicated and the tokens of each
// should be packaged in order to distinguish them clearly.
//
parameter_declaration
    :    declaration_specifiers parameter_declarator?
            -> ^(PARAMETER declaration_specifiers parameter_declarator?);

// The problem here is that both declarator and abstract_declarator can start with pointer.
parameter_declarator
    :    (declarator attributes?) => declarator attributes?
    |    abstract_declarator;
    
identifier_list
    :    identifier (',' identifier)* -> identifier+;
    
type_name
    :    specifier_qualifier_list abstract_declarator?;
    
abstract_declarator
    :    (pointer? direct_abstract_declarator) => pointer? direct_abstract_declarator
    |    pointer;
    
direct_abstract_declarator
    :    ('(' abstract_declarator ')') => 
          '(' abstract_declarator ')' direct_abstract_declarator_modifier*
    |    direct_abstract_declarator_modifier+;

direct_abstract_declarator_modifier
    :    '[' assignment_expression? ']'
    |    '(' parameter_list ')';
        
// Type names are special raw identifiers.
typedef_name
    :    { symbols.isType(input.LT(1).getText()) }? RAW_IDENTIFIER;
//    |    RAW_IDENTIFIER '.' RAW_IDENTIFIER;
    
// The INITIALIZER_LIST pseudo-token is needed to distinguish brace enclosed initializers from
// simple assignment expressions. Since the first initializer in an initializer list might also
// be an assignment expression, some sort of indication is needed to mark the list. Note: C99
// designators are not currently supported.
//
initializer
    :    assignment_expression
    |    '{' initializer_list ','? '}' -> ^(INITIALIZER_LIST initializer_list);
    
// Note that designated initializers are using the gcc form (rather than the C99 form).
// TODO: The designators should be in the AST since they have semantic significance.
initializer_list
    :    (RAW_IDENTIFIER ':')? initializer (',' (RAW_IDENTIFIER ':')? initializer)*
            -> initializer+;
    
/* ================= */
/* Statement grammar */
/* ================= */

// Allowing line_directive here creates problems because an expression_statement can start with
// a CONSTANT. This means the end of a line_directive can't be unambiguously found. For example
// something like
//
// # "somefile.nc" 1
// /* null statement */ ;
//
// can be parsed as
//
// # "somefile.nc"
// 1;  /* an expression_statement */
//
// See also the comment at line_directive.
statement
    :    labeled_statement
    |    compound_statement
    |    expression_statement
    |    selection_statement
    |    iteration_statement
    |    jump_statement
    |    atomic_statement
    |    line_directive_mini;    // This is a hack. Is it really necessary?
    

atomic_statement
    :    ATOMIC statement -> ^(ATOMIC statement);
    
labeled_statement
    :    identifier ':' statement -> ^(LABELED_STATEMENT identifier statement)
    |    CASE constant_expression ':' statement -> ^(CASE constant_expression statement)
    |    DEFAULT ':' statement -> ^(DEFAULT statement);

// The COMPOUND_STATEMENT pseudo-token is needed to clarify how many block items are contained
// in a particular compound statement (or scope).
//    
compound_statement
    :    '{' { symbols.enterScope(); }
         declaration* statement*
             { symbols.exitScope();  } '}' -> ^(COMPOUND_STATEMENT declaration* statement*);

// The STATEMENT pseudo-token is needed so that expression statements are clearly distinguished
// from declarations, other kinds of statements (if, while, etc), and also to clarify places
// where null statements are used.
//
expression_statement
    :    expression? ';' -> ^(STATEMENT expression?);
    
// Using a syntactic predicate here is probably wrong. Typedef declarations inside the
// controlled statements won't have their types entered into the symbol table when the predicate
// is being evaluated. Thus nested declarations using those types won't parse. Correct handling
// might require rewriting the grammar to resolve the ambiguity. The resulting grammar is ugly,
// however.
//
selection_statement
    :    (IF '(' expression ')' statement ELSE statement) =>
          IF '(' expression ')' s1=statement ELSE s2=statement
             -> ^(IF expression $s1 $s2)
    |    IF '(' expression ')' statement
             -> ^(IF expression statement)
    |    SWITCH '(' expression ')' statement
             -> ^(SWITCH expression statement);
    
// We need pseudo-tokens for the various expressions in a for loop header to deal with the case
// when one or more are left out. Otherwise we can't easily distinguish the meaning of one
// expression from the other.
//
iteration_statement
    :    WHILE '(' expression ')' statement -> ^(WHILE expression statement)
    |    DO statement WHILE '(' expression ')' ';' -> ^(DO statement expression)
    |    FOR '(' init=expression? ';' cond=expression? ';' iter=expression? ')' statement
            -> ^(FOR ^(FOR_INITIALIZE $init?)
                     ^(FOR_CONDITION  $cond?)
                     ^(FOR_ITERATION  $iter?) statement);
    
jump_statement
    :    GOTO identifier ';' -> ^(GOTO identifier)
    |    CONTINUE ';' -> CONTINUE
    |    BREAK ';' -> BREAK
    |    RETURN expression? ';' -> ^(RETURN expression?);
    
/* ===================================== */
/* Large scale program structure grammar */
/* ===================================== */

translation_unit
    :    (line_directive | external_declaration)+;

// It would be more accurate to move function_definition here. See the comment at 'declaration'
external_declaration
    :    declaration;

// This hack allows the parser to recognize the line directives left over from preprocessing.
// This information is then used to reconstruct the #include directives in the rewritten
// output. This is needed because of the unusual way in which nesC handles preprocessing.
// In particular, if the #includes are left expanded, the nesC compiler complains about
// multiple definitions of constructs (types) defined in separate files.
//
// Because line directives can occur pretty much anywhere this grammar is sprinkled with places
// where line directives are allowed. This is a bit of a hack. A better approach might be to
// only recognize line directives at global scope and to just shunt them to the hidden channel
// (lexically) when processing the body of a component or interface. This requires context
// sensitive lexical analysis. I haven't implemented that yet.
//
// For now it is necessary to distinguish between line directives that can occur at global scope
// from those that can occur inside other structures. See the comment at 'statement' for more
// information. See also: http://gcc.gnu.org/onlinedocs/cpp/Preprocessor-Output.html for details
// on the line directive syntax.
//
line_directive
    :    '#' CONSTANT STRING_LITERAL CONSTANT* -> ^(LINE_DIRECTIVE STRING_LITERAL);

line_directive_mini
    :    '#' CONSTANT STRING_LITERAL -> ^(LINE_DIRECTIVE STRING_LITERAL);
    
// This is a bit liberal. It permits things like 'int x { ... }.' That is, it does not require
// the declarator to have a parameter list modifier.
//
function_definition
    :    declaration_specifiers declarator (attributes | gcc_attributes)? compound_statement
            -> declaration_specifiers declarator compound_statement;

/* =============== */
/* nesC extensions */
/* =============== */

// The FILE pseudo-token is needed to provide a wrapper for the entire file. Otherwise if the
// optional translation unit is present, a null token is used as the parent of that unit's
// declarations and the interface|component. This null token is awkward.
//
nesC_file
    :    translation_unit? large_scale_construct
             -> ^(FILE translation_unit? large_scale_construct);

large_scale_construct
    :    interface_definition
    |    component;

// This is deprecated. The nesC 1.2 manual doesn't even discuss the semantics of 'includes'
// include_statement
//  :  INCLUDES identifier_list ';';
    

// Interface scope is nested inside the global scope.
// Bug: Right now the type parameters and attributes are ignored!
interface_definition
    :    INTERFACE identifier type_parameters? attributes?
        '{' { symbols.enterScope(); }
        (line_directive_mini | declaration)*
            { symbols.exitScope();  } '}'
            -> ^(INTERFACE identifier declaration*);
    
type_parameters
    :    '<' type_parameter_list '>';
    
type_parameter_list
    :    type_specifier attributes? (',' type_specifier attributes?)*;
  
// The COMPONENT_DEFINITION pseudo-token introduces an entire component definition. The
// complexities of COMPONENT_KIND are pushed into a child of this AST node.
//
component
    :   component_kind identifier component_parameters? attributes?
        component_specification
        implementation? -> ^(COMPONENT_DEFINITION component_kind identifier component_specification implementation? component_parameters?);

// The COMPONENT_KIND pseudo-token provides a way to wrap up component kinds that are two words
// such as 'generic module' and 'generic configuration.'
//
component_kind
    :    MODULE                -> ^(COMPONENT_KIND MODULE)
    |    CONFIGURATION         -> ^(COMPONENT_KIND CONFIGURATION)
    |    COMPONENT             -> ^(COMPONENT_KIND COMPONENT)
    |    GENERIC MODULE        -> ^(COMPONENT_KIND GENERIC MODULE)
    |    GENERIC CONFIGURATION -> ^(COMPONENT_KIND GENERIC CONFIGURATION);
    
implementation
    :    IMPLEMENTATION '{' body '}'
             -> ^(IMPLEMENTATION body?);

// The COMPONENT_PARAMETER_LIST pseudo-token is used to wrap all the parameters of a generic
// component. This is so the parameters are well contained as a single child of the component
// node.
//    
component_parameters
    :    '(' component_parameter_list? ')'
            -> ^(COMPONENT_PARAMETER_LIST component_parameter_list?);
    
component_parameter_list
    :    component_parameter (',' component_parameter)* -> component_parameter+;
    
component_parameter
    :    parameter_declaration
    |    TYPEDEF identifier attributes?;
    
body
    :    (configuration_body) => configuration_body
    |    module_body;

// Implementation scope is nested inside specification scope. Both scopes end when the
// implementation closes.
//
configuration_body
    :    { symbols.enterScope(); }
         configuration_element_list?
         { symbols.exitScope(); symbols.exitScope(); };
    
// See comment immediately above.
module_body
    :    { symbols.enterScope(); }
         translation_unit
         { symbols.exitScope(); symbols.exitScope(); };

configuration_element_list
    :    (line_directive_mini | configuration_element)+;
    
configuration_element
    :    components
    |    connection;

    // This causes major ambiguities with a module implementation (translation_unit). I'm not
    // sure how to best handle this right now, but probably significant changes will be
    // necessary.
    //
    //|    declaration;
    
components
    :    COMPONENTS component_line ';' -> ^(COMPONENTS component_line);
    
component_line
    :    component_declaration (',' component_declaration)* -> component_declaration+;
    
component_declaration
    :    component_ref (AS identifier)? -> ^(COMPONENT_DECLARATION component_ref identifier?);
    
  
// The COMPONENT_INSTANTIATION pseudo-token makes it easier to distingush full instantiations
// from simple component identifiers.
//  
component_ref
    :    identifier
    |    NEW identifier '(' component_argument_list? ')'
            -> ^(COMPONENT_INSTANTIATION identifier component_argument_list?);
    
// The COMPONENT_ARGUMENTS pseudo-token isn't strictly needed. The component arguments are
// fairly clearly distinguished in a component instantiation because they follow the required
// identifier. However, it does make the rewriter code a little easier if the AST breaks the
// arguments out as is done here.
//
component_argument_list
    :    component_argument (',' component_argument)*
            -> ^(COMPONENT_ARGUMENTS component_argument+);
    
component_argument
    :    assignment_expression
    |    wrapped_type_name;

// It is necessary to use a pseudo-token here to distinguish '=' and '->' from their use in
// expressions.
//
connection
    :    endpoint wire_rhs ';'
             -> ^(CONNECTION wire_rhs endpoint);

wire_rhs
    :    '='^  endpoint
    |    '->'^ endpoint
    |    '<-'^ endpoint;

// TODO: We should really wrap up the endpoint so that we can deal with argument_expression_list
// a little more cleanly in the rewriter. See the handling of CONNECTION in SyntaxViewer.java
// for more information.
//
endpoint
    :    identifier_path
    |    identifier_path '[' argument_expression_list ']';
    
identifier_path
    :    identifier ('.' identifier)*
            -> ^(IDENTIFIER_PATH identifier+)
    |    '[' indirect=identifier ']' ('.' normal_path+=identifier)*
            -> ^(DYNAMIC_IDENTIFIER_PATH $indirect $normal_path);
    
// Specification scope is nested inside the global scope. However, specification scope does not
// end until the associated implementation scope ends (items declared in the specification are
// visible in the implementation).
//
component_specification
    :    '{' { symbols.enterScope(); }
          (line_directive_mini | uses_provides)* '}' -> ^(SPECIFICATION uses_provides*);
    
uses_provides
    :    USES specification_element_list -> ^(USES specification_element_list)
    |    PROVIDES specification_element_list -> ^(PROVIDES specification_element_list)
    |    declaration -> ^(DECLARATION declaration);
    
specification_element_list
    :    specification_element -> specification_element
    |    '{' specification_element+ '}' -> specification_element+;
    
specification_element
    :    declaration
    |    interface_type (AS identifier)? instance_parameters? attributes? ';'
            -> ^(INTERFACE interface_type identifier?);
    
// The INTERFACE_TYPE pseudo-token is needed to wrap up the name of an interface with its type
// arguments, if any. This allows the type arguments to be distinguished from the identifier
// used in a renaming (with 'as'). See specification_element above.
//
interface_type
    :    INTERFACE identifier type_arguments?
            -> ^(INTERFACE_TYPE identifier type_arguments?);
    
type_arguments
    :    '<' wrapped_type_name (',' wrapped_type_name)* '>' -> wrapped_type_name+;

// Type names can consist of a number of separate "words" (type specifiers, abstract declarator,
// etc). In productions where a list of type names can appear it is helpful to wrap each one
// in its own subtree. This makes things clearer when, for example, rewriting the AST.
//
wrapped_type_name
    :    type_name -> ^(TYPE_NAME type_name);
    
instance_parameters
    :    '[' parameter_list ']';
    
attributes
    :    attribute+;
    
attribute
    :    '@' identifier '(' initializer_list ')';

// nesC programs are ultimately passed to gcc, and gcc-specific attributes are commonly used,
// for example, in libraries (header files, etc).
//
// TODO: Look up all places where gcc-specific attributes can appear and be sure the handling of
// them is complete. Right now 'gcc_attributes' is sprinkled haphazardly around the grammar for
// the sake of getting examples to compile.
//
gcc_attributes
    :    GCCATTRIBUTE '(' '(' gcc_attribute_list ')' ')';

gcc_attribute_list
    :    gcc_attribute? (',' gcc_attribute)*;

gcc_attribute
    :    assignment_expression;

// Ordinary identifiers are raw identifiers that are not type names.
identifier
    :    { !symbols.isType(input.LT(1).getText()) }? RAW_IDENTIFIER;

/* =========== */
/* Lexer rules */
/* =========== */

COMMENT1
    :    '/*' (options {greedy=false;} : .)* '*/' {$channel = HIDDEN;};
    
COMMENT2
    :    '//' (options {greedy=false;} : .)* ('\r' | '\n') {$channel = HIDDEN;};

// This is a gcc feature to mark expressions/declarations so -pedantic won't warn about them.
// I just want to ignore it.
COMMENT3
    :    '__extension__' {$channel = HIDDEN;};

RAW_IDENTIFIER
    :    ('_' | 'a' .. 'z' | 'A' .. 'Z') ('_' | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9')*;
    
CONSTANT
    :    NUMBER;
    
STRING_LITERAL
    :    '"' (options {greedy=false;} : .)* '"';
    
CHARACTER_LITERAL
    :    '\'' (options {greedy=false;} : .)* '\'';
    
WHITESPACE
    :    ( '\t' | ' ' | '\r' | '\n' | '\f' )+  {$channel = HIDDEN;};

// See the comment at the line_directive production for reasons why we might want to shunt line
// directives to the hidden channel under certain circumstances.
//
//LINE_DIRECTIVE
//    :    '#' (options {greedy=false;} : .)* ('\r' | '\n') {$channel = HIDDEN;};

fragment NUMBER
    :    DEC_NUMBER
    |    HEX_NUMBER;
    
fragment DEC_NUMBER
    :    ( DIGIT )+ ( NUMBER_SUFFIX )?;
    
fragment HEX_NUMBER
    :    NUMBER_PREFIX ( HEX_DIGIT )+ ( NUMBER_SUFFIX )?;
    
fragment DIGIT
    :    '0' .. '9';
    
fragment HEX_DIGIT
    :    '0' .. '9' | 'a' | 'b' | 'c' | 'd' | 'e' | 'f' | 'A' | 'B' | 'C' | 'D' | 'E' | 'F';

fragment NUMBER_PREFIX
    :    '0x';

fragment NUMBER_SUFFIX
    :    ('l' | 'L' | 'u' | 'U' | 'lu' | 'ul' | 'LU' | 'UL');
