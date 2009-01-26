package org.protocopter.compiler.tree

/** Base class for the protocopter Abstract Syntax tree.*/
sealed trait ASTNode
sealed trait Expr extends ASTNode


case class Assignment(lvalue : ASTNode, rvalue : ASTNode) extends ASTNode
case class AppendPrototype(lvalue : ASTNode, rvalue : ASTNode) extends ASTNode


case class Prototype(value : ASTNode) extends Expr

//function call parsing
case class FunctionCall(function : ASTNode, self : Option[ASTNode], args : Option[ExecuteArgument]) extends ASTNode
case class ExecuteArgument(args : List[ASTNode]) extends ASTNode


//Some kind of message on an object (slot access or "execute")
case class Message(expr : ASTNode, msgType : MessageType) extends Expr
abstract class MessageType extends ASTNode

//Possible RHS for Message tree
case class ExecuteCall(closureSlot : Identifier, args : Option[ExecuteArgument]) extends MessageType
//Possible RHS for Message tree
case class SlotAccess(slot:Identifier) extends MessageType



//Closure Node
case class ArgumentCodeBlock(args : Arguments, b : List[ASTNode]) extends CodeBlock(b)
case class CodeBlock(block : List[ASTNode]) extends Expr
case class Arguments(names : List[String]) extends ASTNode

case class Closure(b : List[ASTNode]) extends CodeBlock(b)
case class ArgumentClosure(a : Arguments, b1 : List[ASTNode]) extends ArgumentCodeBlock(a,b1)

//IDentifier niceness...
abstract class Identifier extends Expr
case class DirectIdentifier(name : String) extends Identifier
case class IndirectIdentifer(expr : ASTNode) extends Identifier
case class Literal[A](literal : A) extends Expr

