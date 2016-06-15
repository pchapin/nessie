//-----------------------------------------------------------------------
// FILE    : Main.java
// SUBJECT : Wrapper program for the nesC parser.
// AUTHOR  : (C) Copyright 2009 by Peter C. Chapin <PChapin@vtc.vsc.edu>
//
//-----------------------------------------------------------------------
package edu.vtc.nesc.parser;

import org.antlr.runtime.*;
import org.antlr.runtime.tree.CommonTree;

/**
 * Wrapper class for the main method.
 *
 * @author Peter
 */
public class Main {

    private static ParserSymbolsManager globalSymbols = new ParserSymbolsManager();

    /**
     * This program provides a simple, semi-interactive interface to the parser. It can be used for testing or other
     * experimentation. A real application would just use the parser class (see the code below for how to do that).
     *
     * @param args The command line arguments
     */
    public static void main(String[] args)
            throws java.io.IOException, org.antlr.runtime.RecognitionException
    {
        // These are type names imported from the TinyOS library. When parsing just one file at a time, we need to
        // "pre-load" the global symbol table with this information.
        //
        String[] globalTypes = {
            "bool", "error_t", "message_t"
        };

        if (args.length == 0) {
            System.err.println("Error: Expected nesC source file name on the command line.");
            System.exit(1);
        }

        // Configure a global ParserSymbolsManager. This is necessary because nesC's global scope spans all files in the
        // program. That is, global entities declared in one file are visible in all other files compiled afterward.
        // Since this program is intended to parse just one nesC file at a time, we must pre-load the global symbol
        // table with information on commonly used types from the TinyOS library. This is a bit of a hack, but it will
        // allow the parser to be tested on non-trivial code examples.
        //
        for (String typeName : globalTypes) {
            globalSymbols.addType(typeName);
        }

        NesCLexer lex = new NesCLexer(new ANTLRFileStream(args[0]));
        TokenStream tokens = new CommonTokenStream(lex);
        NesCParser parser = new NesCParser(tokens);
        parser.setSymbols(globalSymbols);
        NesCParser.nesC_file_return result = parser.nesC_file();

        // Unconditionally output the abstract syntax tree.
        CommonTree abstractSyntax = (CommonTree)result.getTree();
        SyntaxViewer viewer = new SyntaxViewer(System.out, abstractSyntax);
        System.out.println("*** AST ==>");
        viewer.writeAST();
        System.out.println();

        // Display the rewritten source code.
        System.out.println("*** REWRITE ==>");
        viewer.rewrite();
        System.out.println();
    }
}
