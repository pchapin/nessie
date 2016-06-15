package edu.vtc.nesc

object NesCTypes {

  class NesCTypeException(message: String) extends Exception(message)

  /**
   * Instances of this class describe Nessie's representation of a nesC type.
   */
  sealed abstract class Representation

  // Primitive types.
  case object Okay   extends Representation { override def toString = "OKAY"     }
  case object Uninit extends Representation { override def toString = "void"     }
  case object Char   extends Representation { override def toString = "char"     }
  case object UInt8  extends Representation { override def toString = "uint8_t"  }
  case object UInt16 extends Representation { override def toString = "uint16_t" }
  case object UInt32 extends Representation { override def toString = "uint32_t" }
  case object Int8   extends Representation { override def toString = "int8_t"   }
  case object Int16  extends Representation { override def toString = "int16_t"  }
  case object Int32  extends Representation { override def toString = "int32_t"  }
  case object ErrorT extends Representation { override def toString = "error_t"  }
  case object Top    extends Representation { override def toString = "TOP"      }
  
  // Type variables.
  case class TypeVariable(name: String) extends Representation
  
  // Structured types.
  type TypeBound = (TypeVariable, Representation)
  type TypeBinding = (String, Representation)
  case class Structure(name: String, members: List[(String, Representation)]) extends Representation
  case class Array(elementType: Representation, size: String) extends Representation
  case class Pointer(targetType: Representation) extends Representation
  
  case class Function(
    returnType    : Representation,
    parameterTypes: List[Representation]) extends Representation

  
  // Module types.
  type Import = (String, Representation)       // Parameter types not considered here.
  type Export = (String, Representation)       // Parameter types not considered here.
  case class Module(
    typeParameters : List[TypeBound],
    valueParameters: List[TypeBinding],
    imports        : List[Import],
    exports        : List[Export]) extends Representation
  
  
  def promote(delta: Map[String, Representation], t: Option[Representation]): Representation = {
    
    val tType = t match {
      case Some(TypeVariable(tvar)) => { 
        val tvarType = delta.get(tvar) match {
          case None => throw new NesCTypeException(s"Type Variable $t must exist in Delta")
          case newType => newType 
        }
        promote(delta, tvarType)
      }
      case Some(structuredType) => structuredType 
      case None => throw new NesCTypeException(s"Type Variable $t must exist in Delta")
    }
    
    tType 
  } // Recursively looks up the type variable until a structured type is found or returns Uninit.
    // Changed to take in Map[String,TR], t: string, and promote(delta, tvar) from TypeVariable

  /**
   * Looks up the type of a member in a structure that is indexed by a given identifier
   */
  def memberType(struct: Representation, index: String): Representation = {
    val memberList = struct match {
      case Structure(name, members) => members
      // case Uninit => List() // FLAG: This should be case Interface => lookupInterface("CommandName")
      case _ => throw new NesCTypeException("Must be structure type")
    }

    var currMbr = memberList.size
    
    while (currMbr > 0) {
    
      currMbr = currMbr - 1
      
      val (mbrName, mbrType) = memberList(currMbr) match {
        case (str, someType) => (str,someType)
        case _ => throw new NesCTypeException("Invalid member")
      }
      
      if (index == mbrName)
        return mbrType
    }
    return Function(Uninit,List()) // FLAG - just a placeholder to see what happens with Interfaces
    throw new NesCTypeException("Invalid address")
  }
    
  /**
   * Returns the least upper bound between two integer types
   */
  def leastUpperBound(left: Representation, right: Representation) = {
    val message = s"Incompatible Types: $left, $right"
    if (left == right) left
    else {
      left match {
      
        case Int8 => right match {
          case Int16 => Int16
          case Int32 => Int32
          case _ => throw new NesCTypeException(message)
        }
        
        case Int16 => right match {
          case Int8  => Int16
          case Int32 => Int32
          case _ => throw new NesCTypeException(message)
        }
        
        case Int32 => right match {
          case Int8  => Int32
          case Int16 => Int32
          case _ => throw new NesCTypeException(message)
        }
        
        case UInt8 => right match {
          case UInt16 => UInt16
          case UInt32 => UInt32
          case _ => throw new NesCTypeException(message)
        }
        
        case UInt16 => right match {
          case UInt8  => UInt16
          case UInt32 => UInt32
          case _ => throw new NesCTypeException(message)
        }
        
        case UInt32 => right match {
          case UInt8  => UInt32
          case UInt16 => UInt32
          case _ => throw new NesCTypeException(message)
        }
        
        case _ => throw new NesCTypeException(message)
      }
    }
  }
  
  def lookupTypeVar(delta: Map[String, Representation], s: String): Representation = {
    delta.get(s) match {
      case Some(typeRep) => typeRep
      case None => throw new NesCTypeException(s"Type Variable $s must exist in Delta")
    }
  }

  /**
   * Returns true if left <: right using nesT subtyping rules.
   */
  def areSubtypes(delta: Map[String, Representation], left: Representation, right: Representation): Boolean = {
    val message = s"Subtyping Error: $left $right"
    
    val typeCase = (left, right) match {
      case (TypeVariable(x), TypeVariable(y)) => areSubtypes(delta, lookupTypeVar(delta,x), lookupTypeVar(delta,y))
      case (TypeVariable(x), _) => areSubtypes(delta, lookupTypeVar(delta,x), right)
      case (_, TypeVariable(y)) => areSubtypes(delta, left, lookupTypeVar(delta,y))
      case _ => false
    }
    
    if (typeCase) return true
    
    if (left == right || right == Top) true
    else {
      
      left match {

        case Int8 => right match {
          case Int16 | Int32 => true
          case _  => false
        }

        case Int16 => right match {
          case Int32 => true
          case _ => false
        }

        case UInt8 => right match {
          case UInt16 | UInt32 => true
          case _  => false
        }

        case UInt16 => right match {
          case UInt32 => true
          case _ => false
        }
        
        case Array(aType, aSize) => right match {
          case Array(aType, "") => true
          case _ => false
        }
        
        /*  This is the case for Function ala the subtyping rules, but should be adapated
            so that it makes sense for the nesT version of functions with parameters
        case Function(fType, _) => right match {
          case Function(fType, _) => true
          case _ => false        
        }
        */
        
        case Structure(_, leftMemberList) => right match {
          case Structure(_, rightMemberList) => {
            var passedTest = true
            var tempTypeLeft: Representation = Okay
            var tempTypeRight: Representation = Okay
            if (leftMemberList.size < rightMemberList.size)
              passedTest = false
            else {
              for (i <- 0 until rightMemberList.size) {
                val (tempStringLeft, tempTypeLeft) = leftMemberList(i) match {
                  case (someString, typeRep) => (someString, typeRep)
                  case _ => throw new NesCTypeException(message)
                }
                val (tempStringRight, tempTypeRight) = rightMemberList(i) match {
                  case (someString, typeRep) => (someString, typeRep)
                  case _ => throw new NesCTypeException(message)
                }
                if ((!areSubtypes(delta, tempTypeLeft,tempTypeRight)) || (!(tempStringLeft==tempStringRight)))
                  passedTest = false
              }
            }
            passedTest
          }
          case _ => false 
        } // To be a subtype, the relevant fields of the structures must occur in the same order
          // and be at the start of the field list - Hopefully update to be more fluid

        
        case _ => false
      }
    }
  }


  def findChild(node: ASTNode, token: Int): ASTNode = {
    for (i <- 0 until node.children.length) {
      if (node.children(i).tokenType == token)
        return node.children(i)
    }
    throw new Exception(s"Unable to locate child token (type $token) of node '${node.text}'")
  }


  def existsChild(node: ASTNode, token: Int): Boolean = {
    for (i <- 0 until node.children.length) {
      if (node.children(i).tokenType == token)
        return true
    }
    false
  }

}
