package edu.vtc.nesc

import org.antlr.runtime.CommonToken
import org.antlr.runtime.tree._

/**
 * This object has methods for handling abstract syntax tree conversions and other high level
 * AST preparations. Since Scala can only do pattern matching on case classes one service
 * provided by this object is conversion to/from the trees produced by ANTLR to instances of a
 * suitably defined case class (ASTNode). Some additional high level methods are also provided.
 */
object TreeConverter {

  /**
   * Convert an ANTLR-style abstract syntax tree into an ASTNode case class instance. Note that
   * some of the information contained inside the ANTLR produced tree is not preserved in the
   * case class instance. Currently this is information not needed by Nessie.
   * 
   * @param t The ANTLR-style abstract syntax tree to be converted.
   * @return An ASTNode instance that represents the tree.
   */
  def ANTLRToScala(t: Tree): ASTNode = {
    var childList = List[ASTNode]()
    for (i <- 0 until t.getChildCount) {
      // I'm thinking there is probably a better way to do this.
      childList = childList ::: List(ANTLRToScala(t.getChild(i)))
    }
    val newNode = ASTNode(t.getType, t.getText, childList, None, None)

    // ANTLR uses one based positions for line numbers and zero based positions for column
    // numbers. In contrast Nessie uses one based positions for both lines and columns.
    //
    newNode.line = t.getLine
    newNode.positionInLine = t.getCharPositionInLine + 1

    // Update the parent reference in each child to link that child to this parent.
    for (child <- childList) {
      child.parent = Some(newNode)
    }
    newNode
  }


  /**
   * Convert an ASTNode case class instance into an ANTLR-style abstract syntax tree. This is
   * done so the modified trees created by Nessie can be passed back to Java for final output.
   * The ANTLR-style abstract syntax tree returned does not contain all the information about
   * tokens that would normally be present. Nessie does not use this information so this is not
   * an immediate problem.
   * 
   * @param root The ASTNode instance to convert.
   * @return An ANTLR-style abstract syntax tree.
   */ 
  def scalaToANTLR(root: ASTNode): CommonTree = {

    def processChildren(tree: CommonTree, children: List[ASTNode]): CommonTree = {
      children foreach ( child => tree addChild processSubtree(child) )
      tree
    }


    def processSubtree(node: ASTNode): CommonTree = {
      node match {
        case ASTNode(myType, myText, myChildren, _, _) =>
          val freshToken = new CommonToken(myType, myText)
          val freshTree = new CommonTree(freshToken)
          processChildren(freshTree, myChildren)
      }
    }

    processSubtree(root)
  }


  /**
   * Writes the abstract syntax tree to standard output. Each level of the tree is indented
   * relative to the level above it. This method is useful for debugging purposes.
   *
   * @param root The root of the tree to dump.
   */
  def dumpAST(root: ASTNode): Unit = {
    var indentationLevel = 0

    def processSubtree(node: ASTNode): Unit = {
      for (i <- 0 until indentationLevel) print("  ")

      // Is there a better way to extract the components of a single ASTNode?
      node match {
        case ASTNode(tokenType, text, children, _, _) =>
          println(text)
          indentationLevel = indentationLevel + 1
          children map processSubtree
          indentationLevel = indentationLevel - 1
      }
    }

    processSubtree(root)
  }
}
