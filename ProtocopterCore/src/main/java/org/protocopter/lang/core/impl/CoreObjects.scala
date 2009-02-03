package org.protocopter.lang.core.impl

/** Base protocotper object for use by all others... */
class CoreObject extends AbstractProtocopterObject {  
   /** Creates a new protocopter object of the correct implementation type */
   protected  def createNewProtocopterObject : ProtocopterObject = new CoreObject 
}

/** This class represents and object that has a prototype.  this is only to be used in the standard library code */
abstract class PrototypedObject(parent : ProtocopterObject) extends CoreObject {
   set(box("prototypes"), box(List(this)))
}

/**
 * The base object from which all others derive
 */
object BaseObject extends CoreObject {
  override def toString = "BaseObject"
}


/** Case class for all integer values.  (Wrapper object) */
case class IntObject(value : Int) extends PrototypedObject(BaseObject) {
}
case class DoubleObject(value : Double) extends PrototypedObject(BaseObject){
  
}
/** 
 * For now, case class for string objects...
 * TODO - Strings need to be handled specially so we can access slots with themm...
 */
case class StringObject(value : String) extends CoreObject {
}
/** Case classes for boolean objects */
case class BooleanObject(value : Boolean) extends PrototypedObject(BaseObject) {
  
}




/**
 * This is the abstract class from which all code blocks are implemented.
 */
abstract class CodeBlockObject extends PrototypedObject(BaseObject) with ProtocopterCodeBlock {
  //Implemented by implementation classes (anonymous function classes
  def call(lexicalScope : ProtocopterObject) : ProtocopterObject
}


 


/** "Null" object */
case class NilObject() extends CoreObject {
  override def lookup(name : ProtocopterObject) : ProtocopterObject = throw new IllegalAccessError("cannot lookup slots on Nil") 
  override def prototype = throw new IllegalAccessError("cannot prototype Nil")
  override def set(name : ProtocopterObject, value : ProtocopterObject)  = throw new IllegalAccessError("cannot set a slot on Nil")
  override def remove(name : ProtocopterObject) = throw new IllegalAccessError("cannot remove slot from null!")
}



