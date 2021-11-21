package edu.vtc.nesc

/**
 * Class representing nodes in the AST of a nesC program. Either the field children or the field parent must be a var so
 * that mutually referential ASTNode instances can be built.
 *
 * @param tokenType Token identifier as defined by ANTLR.
 * @param text The actual text of the token as it appears in the source.
 * @param children The child nodes of this node in the AST.
 * @param parent The parent node or None for the root node.
 * @param symbolTable The symbol table associated with this node, if any.
 */
sealed case class ASTNode(
  tokenType  : Int,
  text       : String,
  children   : List[ASTNode],
  var parent : Option[ASTNode],
  var symbolTable: Option[Symbols])
{
  // I don't really want to match on these and the class's parameter list is already too long. Thus I'm making these
  // class members. They need to be vars so they can be set during tree conversion. They should never be modified after
  // that, however.
  // 
  var line = 0
  var positionInLine = 0
}
