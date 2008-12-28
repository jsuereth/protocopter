package org.protocopter.lang.core.impl

/** This is the base object from which everything dervies*/
object BaseObject extends AbstractProtocopterObject(List()) {

  //TODO - insert bootstrap code...
  override def toString = "BaseObject" 
}

object ClosureHelper {
  implicit def pimpProto(obj : ProtocopterObject) = new {
    def addMethod(name : String, args : List[String])(body : ProtocopterObject => ProtocopterObject) {
      obj.set(name, obj.createClosure(args, body))
    }
  }
}

object ListObject extends AbstractProtocopterObject(List(BaseObject)) {
  
  import ClosureHelper._
  
  this.addMethod("iterator", List()) {
    context =>
       //TODO - Create an iterator
       context
  }
  
  this.addMethod("foreach", List("f")) {    
    context =>
       //TODO - Make more parallell?
       val itr = context.execute("iterator", None)()       
       def hasNext : Boolean = {
         itr.execute("hasNext", None)() match {
           case BooleanObject(value) => value
           case _ => false
         }
       }
       while(hasNext) {
         val nextObj = itr.execute("next", Some(List()))()
         context.execute("f", Some(List(nextObj)))
       }
       //TODO - No return?
       NilObject()
  }
  override def toString = "List"
}

case class NilObject extends AbstractProtocopterObject(List()) {
  override def lookup(name : String) = throw new IllegalAccessError("cannot lookup slots on Nil") 
  override def prototype = throw new IllegalAccessError("cannot prototype Nil")
  override def slotInspectionString = "nil"
  override def createClosure(args : List[String], block : ProtocopterObject => ProtocopterObject) = throw new IllegalAccessError("cannot create a closure on Nil")
  override def execute(name : String, arg : Option[List[ProtocopterObject]]) = throw new IllegalAccessError("cannot execute a slot on Nil")
  override def set(name : String, value : ProtocopterObject)  = throw new IllegalAccessError("cannot set a slot on Nil")
}

case class BooleanObject(val value : Boolean) extends AbstractProtocopterObject(List(BaseObject)) {
  //TODO - Insert bootstrap code
  override def toString = value.toString
}
case class IntObject(val value : Int) extends AbstractProtocopterObject(List(BaseObject)) {
  //TODO - insert bootstrap code... 
  override def toString = value.toString
}

class DoubleObject(val value : Double) extends AbstractProtocopterObject(List(BaseObject)) {
  //TODO - Insert bootstrap code
  override def toString = value.toString
}

case class StringObject(val value : String) extends AbstractProtocopterObject(List(BaseObject)) {
  //TODO - Insert bootstrap code
  override def toString = value
}