package org.protocopter.lang.core.impl

/** Abstract class for use when making protocopter objects. */
abstract class AbstractProtocopterObject extends ProtocopterObject {
  /** Private list of prototypes*/
  private[impl] var slots = Map[String, ProtocopterObject]() 
    /**
     * Looks for a given slot on an object.  Returns "None" if it is undefined.
     */
    override def lookup(name : String) : Option[ProtocopterObject] = {
      //Check ourselves first, then defer to parents.
      if(slots.contains(name)) {
        Some(slots(name))
      } else {
      //Breadth First Search of parents
        import DefinitionHelper._
        
        def convert(obj : ProtocopterObject) = obj match {
          case obj : AbstractProtocopterObject => Some(obj)
          case _ => None
        }
        /** Breadth-first search algorithm for slot lookup. */
        def bfSearch(queue : List[ProtocopterObject], seen : Set[ProtocopterObject]) : Option[ProtocopterObject] = {
          queue match {
            // Check the head item
            case head :: tail =>
              //Make sure head is of appropriate class...
              convert(head) match {
                //If we find a valid castable object AND we haven't seen it yet...
                case Some(obj) if !seen.contains(head) =>
                  if(obj.slots.contains(name)) {
                    Some(obj.slots(name))
                  } else {
                    //Get next set of children and add them to quuee.
                    val newChildren = for { 
                      child <- obj.getPrototypes
                      if !seen.contains(child)                      
                    } yield child
                    //Continue on breadth-first search
                    bfSearch(tail ++ newChildren, seen + head)
                  }
                //This shoudl never happen, but don't fail if it does...
                case _ =>
                  bfSearch(tail, seen + head)
              }
            // No more items in queue...
            case Nil =>
              None
          }
        }        
        bfSearch(this.getPrototypes, Set(this))
      }
    }
    override def set(name : String, value : ProtocopterObject) {
      slots += ((name, value))
    }
        
    /** Creates a prototype of this object */
    override def prototype : ProtocopterObject = new PrototypedObject(this)
    
    override def listSlots() : List[String] = {
      Nil
    }
}


object BaseObject extends AbstractProtocopterObject() {
  
}

//TODO - Optimize this...
object ListPrototypeObject extends PrototypedObject(BaseObject) {
  
}
/** Simple implementation of a list...*/
class ListObject(values : List[ProtocopterObject]) extends AbstractProtocopterObject {
    for((value,idx) <- values.zipWithIndex) {      
      set(idx.toString, value)
    }
    
    //TODO - Prototypes slot?
}

/** This class represents and object that has a prototype. */
class PrototypedObject(prototype : ProtocopterObject) extends AbstractProtocopterObject {
  //TODO - Add to prototype list...
  set("prototypes", new ListObject(List(prototype)))
}

/** Case class for all integer values.  (Wrapper object) */
case class IntObject(value : Int) extends PrototypedObject(BaseObject) {
}
/** For now, case class for string objects...*/
case class StringObject(value : String) extends PrototypedObject(BaseObject) {
}
/** Case classes for boolean objects */
case class BooleanObject(value : Boolean) extends PrototypedObject(BaseObject) {
  
}


abstract class CodeBlockObject extends PrototypedObject(BaseObject) with ProtocopterCodeBlock {
  //Implemented by implementation classes (anonymous function classes
  def call(lexicalScope : ProtocopterObject) : ProtocopterObject
}


 


/** "Null" object */
case class NilObject() extends AbstractProtocopterObject {
  override def lookup(name : String) = throw new IllegalAccessError("cannot lookup slots on Nil") 
  override def prototype = throw new IllegalAccessError("cannot prototype Nil")
  override def set(name : String, value : ProtocopterObject)  = throw new IllegalAccessError("cannot set a slot on Nil")
  override def listSlots() : List[String] = {
    Nil
  }
    
}



