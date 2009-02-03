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
     def bfSearch(queue : List[AbstractProtocopterObject], seen : Set[AbstractProtocopterObject]) : Option[ProtocopterObject] = {
       Console.println("bfSearch( queue: " + queue + ", seen: " + seen + ")")
          queue match {
            // Check the head item
            case head :: tail =>
              //Make sure head is of appropriate class...
              if(head.slots.contains(name)) {
                 Some(head.slots(name))
              } else {
                //Get next set of parents and add them to quuee.
                val newParents = for { 
                    parent <- head.getParents
                    if !seen.contains(parent)                      
                } yield parent
                  //Continue on breadth-first search
                bfSearch(tail ++ newParents, seen + head)
              }              
            // No more items in queue...
            case Nil =>
              None
          }
     }    
     bfSearch(getParents.toList, Set(this))
   }
   /**
    * Retreives our parents as a list
    */
   protected def getParents : Seq[AbstractProtocopterObject] = {
     //DOn't use lookup method, as 1) it boxes and 2) it calls this method and 3) it's incorrect to do so.
     slots.get("prototypes").map(unboxSeq(_).asInstanceOf[Seq[AbstractProtocopterObject]]).getOrElse({Console.println("no prototypes slot!"); List()})
   }
   /**
     * Looks up a slot from this object.
     */
    override def lookup(name : ProtocopterObject) : ProtocopterObject = {
      unboxString(name) match {
        case ExistingSlot(value) => value
        case slotName @ _ =>
          //TODO - call lookup slot if it exists
          lookupOnParents(slotName).getOrElse(error("Slot not found: " + name))
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
    
    
    override def toString = "ProtocopterObject(slots:" + slots.keySet.mkString("[",",","]") + ")"
}
