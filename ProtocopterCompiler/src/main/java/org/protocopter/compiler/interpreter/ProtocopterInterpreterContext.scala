package org.protocopter.compiler.interpreter

import compiler.parser._
import compiler.tree._
import org.protocopter.lang.core._
import org.protocopter.lang.core.impl._
import org.protocopter.lang.core.helpers._

class ProtocopterInterpreterContext(val output : java.io.PrintStream) extends ProtocopterParser with TreeEvaluator {
  
  override def context = BaseObject.prototype
  
  def eval(line : String) {
    parseAll(statement, line) match {
      case Success(tree, left) =>
        output.println(eval(tree))
        prompt()
      case Failure(msg, left) =>
        output.println("line: " + left.pos.line + " column: " + left.pos.column + " " + msg)
        prompt()
      case Error(msg, left) =>
        output.println("line: " + left.pos.line + " column: " + left.pos.column + " " + msg)
        prompt()
    }
    
  } 
  
  def prompt() = output.print("> ")
  
  
}

/** Implementation of a closure that works in a REPL*/
class EvaluatedClosureBlock(ast : List[ASTNode]) extends (ProtocopterObject => ProtocopterObject) with TreeEvaluator {
  var contextVar : ProtocopterObject = null
  def context = contextVar
  
  def apply(obj : ProtocopterObject) : ProtocopterObject = {
    contextVar = obj
    val results = for(node <- ast) yield eval(node) 
    results.lastOption.getOrElse(obj)
  }
}

trait TreeEvaluator {
  import org.protocopter.lang.core.helpers.CallFromScalaHelpers._
  def context : ProtocopterObject
  
  import org.protocopter.compiler.tree._
  protected def eval(tree : ASTNode) : ProtocopterObject = {
    tree match {
      case Assignment(l,r) =>
        val (obj, slot) = evalAsLValue(l)
        obj.set(slot, eval(r))
        obj
      case Prototype(l, r) =>
        val (obj, slot) = evalAsLValue(l)
        obj.set(slot, eval(r).prototype)
        obj
      case Message(l,r) =>
        val obj = eval(l)
        r match {
          case ExecuteCall(slot, Some(args)) => obj.execute(evalIdentifier(slot), Some(evalArgs(args)))() 
          case ExecuteCall(slot, None) => obj.execute(evalIdentifier(slot), None)()
          case SlotAccess(id) => obj.lookup(evalIdentifier(id))()
        }
      case Closure(args, block) => new AbstractProtocopterClosure(List(), args.getOrElse(Arguments(List())).names, new EvaluatedClosureBlock(block)) {}
        
      case id : Identifier => context.lookup(evalIdentifier(id))()
      case Literal(l) => ReflectionUtil.mapJavaObjectToProtocopterObject(l)
      case _ => null
    }
  }
  
  
  private def evalArgs(value : ExecuteArgument) = {
    def evalArgList(args : List[ASTNode]) : List[ProtocopterObject] = args match {
      case head :: tail => eval(head) :: evalArgList(tail)
      case Nil => Nil
    } 
    evalArgList(value.args)
  }
  private def evalIdentifier(value : Identifier) = value match {
    case DirectIdentifier(slot) => slot
    case IndirectIdentifer(expr) => proto2string(eval(expr))
  }
  
  private def evalAsLValue(tree : ASTNode) : (ProtocopterObject, String) = tree match {
    case Message(l,SlotAccess(id)) =>
        (eval(l), evalIdentifier(id))
    case _ => throw new RuntimeException("Invalid l-value! tree=" + tree)
  }
  
  
}