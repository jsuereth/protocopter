package org.protocopter.compiler.t2p


import tree._

trait Tree2CodeConverter {
  
  def map(nodeList : List[tree.ASTNode]) : List[pcode.PCodeInstruction] = {
    //TODO - Keep metadata and thigns
    nodeList.flatMap(mapStatement(_))
  }
  
  def mapStatement(node : ASTNode) : List[pcode.PCodeInstruction] = {
    node match {
      case FunctionCall(f,Some(self),Some(args)) => Nil //TODO - we don't  handle agument lists yet...
      case FunctionCall(f,None,Some(args)) => Nil //TODO - We don't handle argument lists yet
      case FunctionCall(f,Some(self),None) => mapExpression(f) ::: mapExpression(self) ::: List(pcode.ExecuteFunction())
      case FunctionCall(f,None,None) => mapExpression(f) ::: List(pcode.ExecuteFunction())
      case Assignment(lhs, rhs) =>
        mapLValue(lhs) :::  mapExpression(rhs) ::: List(pcode.AssignSlot())
      case AppendPrototype(lhs, rhs) => Nil //TODO - We don't handle appending prototypes yet...      
      case x : Expr => List(pcode.PushScopeInstruction(), pcode.PushLiteralInstruction("it")) ::: mapExpression(x) ::: List(pcode.AssignSlot())
      case _ => Nil
    }
  }
  
  def mapLValue(node:ASTNode) : List[pcode.PCodeInstruction] = node match {
    case Message(expr, SlotAccess(DirectIdentifier(id))) =>
      mapExpression(expr) ::: List(pcode.PushLiteralInstruction(id))
    case Message(expr, SlotAccess(IndirectIdentifer(id))) =>
      mapExpression(expr) ::: mapExpression(id)      
    case DirectIdentifier(name) =>
      List(pcode.PushScopeInstruction(), pcode.PushLiteralInstruction(name))
    case IndirectIdentifer(expr) => 
      List(pcode.PushScopeInstruction()) ::: mapExpression(expr)
    case _ => Nil
  }
  
  def mapExpression(node : ASTNode) : List[pcode.PCodeInstruction] = {
    node match {
      case Literal(l) => List(pcode.PushLiteralInstruction(l))      
      case DirectIdentifier(name) => List(pcode.PushLiteralInstruction(name), pcode.PushReferenceInstruction())
      case IndirectIdentifer(expr) => mapExpression(expr) ::: List(pcode.PushReferenceInstruction())
      case CodeBlock(block) => List(pcode.PushCodeBlock(map(block)))
      case Prototype(expr) => mapExpression(expr) ::: List(pcode.PrototypeObject())
      case Message(expr, SlotAccess(slot)) => mapExpression(expr) ::: mapExpression(slot) ::: List(pcode.SlotAccessInstruction())
      case _ => Nil
    }
  }
}
