package org.protocopter.compiler.tree

/** Base class for the protocopter Abstract Syntax tree.*/
sealed trait ASTNode



case class Assignment(lvalue : ASTNode, rvalue : ASTNode) extends ASTNode
case class Prototype(lvalue : ASTNode, rvalue : ASTNode) extends ASTNode
case class AppendPrototype(lvalue : ASTNode, rvalue : ASTNode) extends ASTNode



//Some kind of message on an object (slot access or "execute")
case class Message(expr : ASTNode, msgType : MessageType) extends ASTNode
abstract class MessageType extends ASTNode

case class ExecuteArgument(args : List[ASTNode]) extends ASTNode
//Possible RHS for Message tree
case class ExecuteCall(closureSlot : Identifier, args : Option[ExecuteArgument]) extends MessageType
//Possible RHS for Message tree
case class SlotAccess(slot:Identifier) extends MessageType



//Closure Node
case class Closure(args : Option[Arguments], block : List[ASTNode]) extends ASTNode
case class Arguments(names : List[String]) extends ASTNode


//IDentifier niceness...
abstract class Identifier extends ASTNode
case class DirectIdentifier(name : String) extends Identifier
case class IndirectIdentifer(expr : ASTNode) extends Identifier
case class Literal[A](literal : A) extends ASTNode

