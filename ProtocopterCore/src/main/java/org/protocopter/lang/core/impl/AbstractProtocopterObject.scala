package org.protocopter.lang.core.impl

import collection.jcl.HashMap
import collection.jcl.HashSet
import collection.jcl.ArrayList
/**
 * Abstract trait for all protocopter objects to implement
 */
trait AbstractProtocopterObject extends ProtocopterObject with BoxingUtil {
  
   var slots : HashMap[String, ProtocopterObject] = new HashMap()
  
   /**
    * Pattern matcher for nicer lookup code
    * - Checks for an existing slot in the current object and returns its value
    */
   private[this] object ExistingSlot {
     def unapply(slotName:String) = {
       if(slots.contains(slotName)) {
         Some(slots(slotName))
       } else {
         None
       }
     }
   }
   
   /** 
    * Looks for a slot definiton on parents using breadth-first-serach 
    */
   protected def lookupOnParents(name : String) : Option[ProtocopterObject] = {
     /** Breadth-first search algorithm for slot lookup. */
     def bfSearch(queue : Seq[AbstractProtocopterObject], seen : Set[AbstractProtocopterObject]) : Option[ProtocopterObject] = {
          queue match {
            // Check the head item
            case head :: tail =>
              //Make sure head is of appropriate class...
              if(head.slots.contains(name)) {
                    Some(head.slots(name))
              } else {
                 //Get next set of children and add them to quuee.
                  val newChildren = for { 
                     child <- head.getParents
                     if !seen.contains(child)                      
                  } yield child
                  //Continue on breadth-first search
                  bfSearch(tail ++ newChildren, seen + head)
              }              
            // No more items in queue...
            case Nil =>
              None
          }
     }     
     bfSearch(getParents, Set(this))
   }
   /**
    * Retreives our parents as a list
    */
   protected def getParents : Seq[AbstractProtocopterObject] = {
     lookup(box("prototypes")).map(unboxSeq(_).asInstanceOf[Seq[AbstractProtocopterObject]]).getOrElse(List())
   }
   /**
     * Looks up a slot from this object.
     */
    def lookup(name : ProtocopterObject) : Option[ProtocopterObject] = {
      unboxString(name) match {
        case "prototypes" =>
          //TODO - Do something special?
          None
        case ExistingSlot(value) => Some(value)
        case slotName @ _ =>
          //TODO - lookup on parent          
          lookupOnParents(slotName)
      }
      
    }
    /**
     * Removes a slot from this object
     */
    def remove(name : ProtocopterObject) {
      slots.removeKey(unboxString(name))
    }
    /**
     * Sets the value of a slot on this obejct
     */
    def set(name : ProtocopterObject, value : ProtocopterObject) {
      slots(unboxString(name)) = value
    }        
    /** 
     * Creates a prototype of this object 
     */
    def prototype : ProtocopterObject = {
      //Create two new protocopter objects
      //Make one be the parent list and place it in our slot
      //Make the other be the new object returned
      val obj = createNewProtocopterObject
      obj.set(box("prototypes"), box(List(this)))
      obj
    }
    

    
    /** Creates a new protocopter object of the correct implementation type */
    protected def createNewProtocopterObject : ProtocopterObject 
}
