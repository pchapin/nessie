//-----------------------------------------------------------------------
// FILE    : ParserSymbolsManager.java
// SUBJECT : Class for handling symbol tables used by the parser.
// AUTHOR  : (C) Copyright 2013 by Peter C. Chapin <PChapin@vtc.vsc.edu>
//
//-----------------------------------------------------------------------
package edu.vtc.nesc.parser;

import java.util.Stack;

/**
 * Class that manages a stack of symbol tables. This allows proper handling of nested scopes. Whenever a new scope is
 * entered, a new symbol table is created for that scope. Right now the only symbol information managed by this class is
 * that symbol's status as a type name. Properly parsing nesC requires that identifiers that are type names be tokenized
 * as such. This class assists with that process.
 *
 * @author Peter
 */
public final class ParserSymbolsManager {
    private Stack<ParserSymbols> scopes = new Stack<ParserSymbols>();


    /**
     * Creates an empty symbol table stack with a single, empty symbol table for the global
     * scope.
     */
    public ParserSymbolsManager()
    {
        // When constructed we need to create the global scope.
        enterScope();
    }


    /**
     * Stacks a new, empty symbol table on top of the scopes. This method should be called
     * whenever a new scope is entered.
     */
    public void enterScope()
    {
        scopes.push(new ParserSymbols());
    }


    /**
     * Adds a type name to the currently active scope (at the top of the scope stack).
     *
     * @param name The name of the type to add.
     * @throws edu.vtc.nesc.parser.ParserSymbols.SymbolRedefinitionException
     * @throws edu.vtc.nesc.parser.ParserSymbols.SymbolConflictException
     */
    public void addType(String name)
            // throws SymbolTable.SymbolRedefinitionException, SymbolTable.SymbolConflictException
    {
        try {
            scopes.peek().addType(name);
        }
        catch (Exception e) {
            // Eat all exceptions for now. These should be passed up but at the moment I don't know how to allow user
            // defined exceptions through ANTLR generated code. Google for "ANTLR throws spec" (the ANTLR site is down
            // at the moment).
        }
    }


    /**
     * Adds an ordinary identifier (non-type name) to the currently active scope (at the top of the scope stack).
     * @param name The name of the identifier to add.
     * @throws edu.vtc.nesc.parser.ParserSymbols.SymbolRedefinitionException
     * @throws edu.vtc.nesc.parser.ParserSymbols.SymbolConflictException
     */
    public void addIdentifier(String name)
            // throws SymbolTable.SymbolRedefinitionException, SymbolTable.SymbolConflictException
    {
        try {
            scopes.peek().addIdentifier(name);
        }
        catch (Exception e) {
            // Eat all exceptions for now. These should be passed up but at the moment I don't know how to allow user
            // defined exceptions through ANTLR generated code. Google for "ANTLR throws spec" (the ANTLR site is down
            // at the moment).
        }
    }


    /**
     * Checks to see if the given name is the name of a type. Scopes are searched from the current scope outward through
     * all enclosing scopes until the global scope is reached and searched. Note that a non-type name can hide a type
     * name in an enclosing scope.
     *
     * @param name The name to check.
     * @return true if the name specifies a type, false otherwise.
     */
    public boolean isType(String name)
    {
        boolean result = false;
        for (int i = scopes.size() - 1; i >= 0; --i) {
            if (scopes.get(i).isType(name)) {
                result = true;
                break;
            }
            if (scopes.get(i).isDefined(name)) {
                result = false;
                break;
            }
        }
        // The following is sometimes useful for debugging.
        // System.out.println("Checking " + name + " as a type; result = " + result);
        return result;
    }


    /**
     * Pops the scope stack by removing the top symbol table. This method should be called whenever a scope is exited.
     */
    public void exitScope()
    {
        scopes.pop();
    }


    /**
     * Writes all symbol tables to standard output. This is useful for debugging purposes.
     */
    public void dump()
    {
        for (int i = scopes.size() - 1; i >= 0; --i) {
            System.out.println("SCOPE LEVEL " + i + ": " + scopes.get(i));
        }
        System.out.println("");
    }

}
