package edu.vtc.nessie

import edu.vtc.nesc.ASTNode
import edu.vtc.nesc.parser.NesCLexer

/**
 * Instances of this class process the abstract syntax tree of interface definitions.
 * @param root The top node of the AST of the interface definition.
 */
class InterfaceProcessor(root: ASTNode) extends Processor(root) {

  private var myName: String = ""

  override def process(): ASTNode = {

    def processSubtree(node: ASTNode): ASTNode = {
      node match {
        // Record my name when I see it.
        case ASTNode(NesCLexer.INTERFACE_TYPE, text, children, parent, symbolTable) =>
          myName = children(1).text
          ASTNode(NesCLexer.INTERFACE_TYPE, text, children map processSubtree, parent, symbolTable)

        // Default case just passes control into the child subtrees.
        case ASTNode(tokenType, text, children, parent, symbolTable) =>
          ASTNode(tokenType, text, children map processSubtree, parent, symbolTable)
      }
    }

    processSubtree(root)
  }

}
