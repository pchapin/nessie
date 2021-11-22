package edu.vtc.nesc.parser

import org.scalatest._
import funsuite._
import matchers._
import java.io.{FileInputStream, File}
import org.antlr.runtime.{CommonTokenStream, ANTLRInputStream}

class NesCParserSuite extends AnyFunSuite with Assertions with should.Matchers {
  
  private val testRoot       = new File("testData")
  private val testSyntax     = new File(testRoot, "Syntax")
  private val syntaxPositive = new File(testSyntax, "Positive")
  private val syntaxNegative = new File(testSyntax, "Negative")

  /**
   * Create a fresh, empty symbol table stack. We don't want global symbols discovered in one
   * test to influence the next test. Unlike normal nesC, global symbols defined in one file
   * should not be carried over to the next. Thus this method must be used before each
   * appropriate test.
   *
   * @return A fresh, empty symbol table stack.
   */
  private def initializeEmptySymbols(): ParserSymbolsManager = {
    new ParserSymbolsManager
  }


  /**
   * Create a fresh global symbol table stack that is preloaded with certain type names from the
   * TinyOS library. This symbol table stack is used in tests that are intended to process
   * entire, realistic files. Unlike normal nesC, global symbols defined in one file should not
   * be carried over to the next. Thus this method must be used before each appropriate test.
   *
   * @return A fresh global symbol table stack preloaded with certain type names.
   */
  private def initializeGlobalSymbols(): ParserSymbolsManager = {
    val globalTypes: Array[String] = Array("__builtin_va_list", "bool", "error_t", "message_t")
    val globalSymbols: ParserSymbolsManager = new ParserSymbolsManager
    for (typeName <- globalTypes) {
      globalSymbols.addType(typeName)
    }
    globalSymbols
  }


  /**
   * Execute test cases.
   * 
   * @param testCaseNames An array of file names representing the test cases.
   * @param symbolInitializer Function that returns an initial symbol table manager to use for
   * each test case.
   * @param doParse A function that takes a NesCParser and executes the parse at a particular
   * start symbol.
   */
  private def doTests(testCaseNames    : Array[String],
                      symbolInitializer: () => ParserSymbolsManager,
                      doParse          : NesCParser => Unit): Unit = {
    
    for (testCaseName <- testCaseNames) {
      val testCase = new File(syntaxPositive, testCaseName)
      val testFile = new FileInputStream(testCase.getPath)
      try {
        val lex = new NesCLexer(new ANTLRInputStream(testFile))
        val tokens = new CommonTokenStream(lex)
        val parser = new NesCParser(tokens)
        parser.setSymbols(symbolInitializer())
        doParse(parser)
      }
      finally {
        testFile.close()
      }
    }
  }

  private def doWholeFileTests(testCaseNames: Array[String]): Unit = {
    doTests(testCaseNames, initializeGlobalSymbols, _.nesC_file(): @annotation.nowarn("msg=discarded non-Unit value"))
  }

  private def doTranslationUnitTests(testCaseNames: Array[String]): Unit = {
    doTests(testCaseNames, initializeGlobalSymbols, _.translation_unit(): @annotation.nowarn("msg=discarded non-Unit value"))
  }

  private def doExpressionTests(testCaseNames: Array[String]): Unit = {
    doTests(testCaseNames, initializeEmptySymbols, _.expression(): @annotation.nowarn("msg=discarded non-Unit value"))
  }

  private def doStatementTests(testCaseNames: Array[String]): Unit = {
    doTests(testCaseNames, initializeEmptySymbols, _.statement(): @annotation.nowarn("msg=discarded non-Unit value"))
  }

  private def doDeclarationTests(testCaseNames: Array[String]): Unit = {
    doTests(testCaseNames, initializeEmptySymbols, _.declaration(): @annotation.nowarn("msg=discarded non-Unit value"))
  }

  test("Basic Interface Test") {
    val testCaseNames =
      Array("Interface0000.nc", "Interface0010.nc", "Interface0020.nc", "Interface0030.nc")
    doWholeFileTests(testCaseNames)
  }

  test("Basic Configuration Test") {
    val testCaseNames =
      Array("Configuration0000.nc", "Configuration0010.nc", "Configuration0020.nc", "Configuration0030.nc")
    doWholeFileTests(testCaseNames)
  }

  test("Configuration Specification Test") {
    val testCaseNames =
      Array("Configuration0100.nc", "Configuration0110.nc", "Configuration0120.nc", "Configuration0130.nc")
    doWholeFileTests(testCaseNames)
  }

  test("Configuration Wiring Test") {
    val testCaseNames =
      Array("Configuration0200.nc", "Configuration0210.nc", "Configuration0220.nc")
    doWholeFileTests(testCaseNames)
  }

  test("Complex Configuration Test") {
    val testCaseNames =
      Array("Configuration0300.nc")
    doWholeFileTests(testCaseNames)
  }

  test("Basic Module Test") {
    val testCaseNames =
      Array("Module0000.nc", "Module0010.nc", "Module0020.nc", "Module0030.nc", "Module0040.nc")
    doWholeFileTests(testCaseNames)
  }

  test("Typedef Module Test") {
    val testCaseNames =
      Array("Module0100.nc", "Module0110.nc", "Module0120.nc")
    doWholeFileTests(testCaseNames)
  }

  test("Complex Module Test") {
    val testCaseNames =
      Array("Module0200.nc", "Module0210.nc", "Module0220.nc", "Module0230.nc", "Module0240.nc")
    doWholeFileTests(testCaseNames)
  }

  test("Basic Expression Test") {
    val testCaseNames =
      Array("Expression0000.nc", "Expression0002.nc", "Expression0004.nc", "Expression0006.nc",
            "Expression0008.nc", "Expression0010.nc", "Expression0012.nc", "Expression0014.nc",
            "Expression0016.nc", "Expression0018.nc", "Expression0020.nc", "Expression0022.nc",
            "Expression0024.nc", "Expression0026.nc", "Expression0028.nc")
    doExpressionTests(testCaseNames)
  }

  test("Assignment Expression Test") {
    val testCaseNames =
      Array("Expression0100.nc", "Expression0102.nc", "Expression0104.nc", "Expression0106.nc")
    doExpressionTests(testCaseNames)
  }

  test("Function Call Expression Test") {
    val testCaseNames =
      Array("Expression0200.nc", "Expression0202.nc", "Expression0204.nc", "Expression0206.nc",
            "Expression0208.nc")
    doExpressionTests(testCaseNames)
  }

  test("Complex Expression Test") {
    val testCaseNames =
      Array("Expression0300.nc")
    doExpressionTests(testCaseNames)
  }

  test("Basic Statement Test") {
    val testCaseNames =
      Array("Statement0000.nc", "Statement0005.nc", "Statement0010.nc", "Statement0015.nc",
            "Statement0020.nc", "Statement0025.nc")
    doStatementTests(testCaseNames)
  }

  test("Complex Statement Test") {
    val testCaseNames =
      Array("Statement0100.nc")
    doStatementTests(testCaseNames)
  }

  test("Basic Declaration Test") {
    val testCaseNames =
      Array("Declaration0000.nc", "Declaration0010.nc", "Declaration0020.nc", "Declaration0030.nc")
    doDeclarationTests(testCaseNames)
  }

  test("cv-Declaration Test") {
    val testCaseNames =
      Array("Declaration0100.nc", "Declaration0110.nc", "Declaration0120.nc")
    doDeclarationTests(testCaseNames)
  }

  test("Structure Declaration Test") {
    val testCaseNames =
      Array("Declaration0200.nc", "Declaration0210.nc", "Declaration0220.nc", "Declaration0230.nc")
    doDeclarationTests(testCaseNames)
  }

  test("Function Declaration Test") {
    val testCaseNames =
      Array("Declaration0300.nc", "Declaration0310.nc", "Declaration0320.nc", "Declaration0330.nc",
            "Declaration0340.nc")
    doDeclarationTests(testCaseNames)
  }

  test("Typedef Declaration Test") {
    val testCaseNames =
      Array("Declaration0400.nc", "Declaration0410.nc", "Declaration0420.nc", "Declaration0430.nc")
    doDeclarationTests(testCaseNames)
  }

  test("GCC Attributes Test") {
    val testCaseNames =
      Array("Declaration0500.nc", "Declaration0510.nc", "Declaration0520.nc", "Declaration0530.nc")
    doDeclarationTests(testCaseNames)
  }

  test("Translation Unit Test") {
    val testCaseNames =
      Array("TranslationUnit0100.nc", "TranslationUnit0110.nc", "TranslationUnit0120.nc")
    doTranslationUnitTests(testCaseNames)
  }

}
