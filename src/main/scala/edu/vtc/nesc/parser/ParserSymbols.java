//-----------------------------------------------------------------------
// FILE    : ParserSymbols.java
// SUBJECT : Class for handling symbol tables used by the parser.
// AUTHOR  : (C) Copyright 2013 by Peter C. Chapin <PChapin@vtc.vsc.edu>
//
//-----------------------------------------------------------------------
package edu.vtc.nesc.parser;

import java.util.TreeSet;

/**
 * This class records information about each name in a particular scope. Right now the only information that is
 * important is if the name specifies a type. Because of the possibility of hiding it is necessary to record non-type
 * names so that they can be found in inner scopes properly. In a more serious language processing application such
 * names would likely need to be tracked for other reasons as well.
 *
 * @author Peter
 */
public class ParserSymbols {
    /**
     * Exception class used to signal duplicate symbols in the same scope.
     */
    public class SymbolRedefinitionException extends Exception {
        public SymbolRedefinitionException(String message)
        {
            super(message);
        }
    }

    /**
     * Exception class used to signal conflicting symbols in the same scope (symbols that are used as both type names
     * and non-type names).
     */
    public class SymbolConflictException extends Exception {
        public SymbolConflictException(String message)
        {
            super(message);
        }
    }

    private TreeSet<String> types = new TreeSet<String>();
    private TreeSet<String> identifiers = new TreeSet<String>();


    /**
     * Adds a type name to the symbol table. Currently no attributes of the type are recorded.
     *
     * @param name The name of the type to add.
     */
    public void addType(String name)
            throws SymbolRedefinitionException, SymbolConflictException
    {
        if (types.contains(name)) {
            throw new SymbolRedefinitionException("Symbol: " + name + " already defined");
        }
        if (identifiers.contains(name)) {
            throw new SymbolConflictException("Symbol: " + name +" already used as identifier");
        }
        types.add(name);
    }


    /**
     * Adds an ordinary identifier (a non-type name) to the symbol table. Currently no attributes of the identifier are
     * recorded.
     *
     * @param name The name of the identifier to add.
     */
    public void addIdentifier(String name)
            throws SymbolRedefinitionException, SymbolConflictException
    {
        if (identifiers.contains(name)) {
            throw new SymbolRedefinitionException("Symbol: " + name + " already defined");
        }
        if (types.contains(name)) {
            throw new SymbolConflictException("Symbol: " + name + " already used as a type");
        }
        identifiers.add(name);
    }


    /**
     * Checks a name to see if it specifies a type.
     *
     * @param name The name to check.
     * @return true if the given name specifies a type in this scope; false if the name specifies a non-type or if the
     * name is not defined in this scope.
     */
    public boolean isType(String name)
    {
        return types.contains(name);
    }


    /**
     * Checks a name to see if it exists in this scope.
     * @param name The name to check.
     * @return true if the name is defined in this specific scope as either a type name or an ordinary identifier; false
     * otherwise.
     */
    public boolean isDefined(String name)
    {
        return types.contains(name) || identifiers.contains(name);
    }


    @Override
    public String toString()
    {
        return "(Types: " + types.toString() + "; Identifiers: " + identifiers.toString();
    }

}
