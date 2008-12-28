package org.protocopter.lang.core.impl

import scala.actors._
import scala.collection.mutable.ListBuffer

object Constants {
  val PARENTS_SLOTNAME = "parents"
}

/** 
 * Abstract implementations of ProtocopterObject's methods 
 * TODO - Destruction/Reference cleanups pf actors!
 */
abstract class AbstractProtocopterObject(protected var parents : List[AbstractProtocopterObject]) extends ProtocopterObject {
  import ProtocopterObjectProtocol._
  
/** Slots for this object */
  protected var slots = Map[String, ProtocopterObject]()
  
  
  /** TODO - Ensure our actors play nice */
  private def parentSlotLookup(name : String, seen : Set[ProtocopterObject]) : LookUpResponse = {
    
    //Helper method to recurse over parents looking for slots
    def helperLookUp(leftOverParents : List[AbstractProtocopterObject], seen : Set[ProtocopterObject]) : LookUpResponse = {      
      leftOverParents match {
        //Rip apart list, pulling out unseen prototypes
        case head :: tail if ! seen.contains(head) =>  //Go left-to-right depth first in searching
          //Attempt to pull slot from parent directly.
           val result = head.msgHandler !! LookupMessage(name, seen)          
           result() match {
             //We found a real object! YAY!
             case obj @ LookUpResponse(Some(_), _) => 
               obj
             //We didn't find anything, keep looking with the updated "seen" set so we don't circle looking for objects
             case LookUpResponse(_, nextSeen) => 
               helperLookUp(tail, nextSeen) 
             //Failure case... TODO - This should never happen!
             case _ => 
               helperLookUp(tail, seen + head)
           }
        //If head was seen, just keep looking
        case head :: tail => helperLookUp(tail, seen)
        // Empty prototype list, return no slot found
        case Nil => LookUpResponse(None, seen)
      }
    }
    //Call our helper method and rip out the response
    helperLookUp(parents, seen)
  }
  
  /** TODO - Implement hard slot lookup */
  private def slotLookup(name : String, seen : Set[ProtocopterObject]) : LookUpResponse = {
    /** Lookup on local slot */
    def localSlotLookup = {	    
	     if(slots.contains(name)) {
	       new LookUpResponse(Some(slots(name)), seen + this)
	     } else {
	       new LookUpResponse(None, seen + this)
	     }
     }
    
    //First we handle "special" slots...  These always exist...
    name match {
      //TODO - Implement the prototypes slot correctly...
      case Constants.PARENTS_SLOTNAME => LookUpResponse(None, seen)
      case _ => 
        localSlotLookup match {
        case resp @ LookUpResponse(Some(x), seen) =>
          resp
        case LookUpResponse(None, seen) =>
          parentSlotLookup(name, seen)
        case _ => LookUpResponse(None, seen)
      }
    }
    
    
  }
  /** This method is used to execute a closure as a method of this object -= called from *this* actor (i.e. don't need to delegate) */
  protected def closureExecution(name : String, arg : Option[List[ProtocopterObject]]) : ProtocopterObject = {
    slotLookup(name, Set()) match {
      case LookUpResponse(Some(obj : ProtocopterClosure), seen) => 
        val result = obj.call(this, arg)
        //TODO - Return some object, in case of failure
        result
      case x @ _ => throw new IllegalAccessException("slot: " + name + " is not executable, found: " + x)
    }
  }
  
  import Actor._
  /** internal message queue*/
  val msgHandler = actor {
      /** Helper method to handle exceitons*/
      def returnResultOrException[A](f : => A) {
        try {
          Actor.sender ! f
        } catch {
          case t =>
            //TODO - Wrap in protocopter object?
            Actor.sender ! t
        }
      }
    loop {
      react {
        case LookupMessage(name, seen) =>  
          returnResultOrException(slotLookup(name, seen))
        case SetMessage(name, value) => 
          slots = slots + (name -> value)
        case ExitScope => //TODO - minimal GC info here?
          //TODO - 
        //case unknown @ _ => System.err.println(toString + " : UNKNOWN MESSAGE! - " + unknown + "\n from " + Actor.sender) 
      }
    }
  }  
  
  //Public interface!
  
  override def lookup(name : String) : Future[ProtocopterObject] = {   
    msgHandler !! (LookupMessage(name, Set()), { 
                     case LookUpResponse(Some(obj), _) => obj
                     case _ =>
                       System.err.println(this + ": returning exception for slot " + name)
                       throw new IllegalArgumentException("No such slot: " + name)
                   })
  } 
  
  override def execute(name : String, arg : Option[List[ProtocopterObject]]) : Future[ProtocopterObject] = {
    //We spawn off a new "actor" just for the execution of a method
    Futures.future({
      closureExecution(name, arg)
    })    
  }
  
  override def set(name : String, value : ProtocopterObject) {
    msgHandler ! SetMessage(name, value)
  }
  //TODO - Make sure this works...
  override def prototype : ProtocopterObject = new AbstractProtocopterObject(List(this)) {}
  
  override def createClosure(args : List[String], block : ProtocopterObject => ProtocopterObject) : ProtocopterClosure = 
    new AbstractProtocopterClosure(List(this), args, block) {}
  
  override def toString() = {
    "object from " + parents.mkString(",")
  }
  
  override def slotInspectionString = {
    val slotStrings = for((slotName, _) <- slots) yield slotName + ":" + lookup(slotName)().toString
    toString + slotStrings.mkString("{", ", ", "}")
  }
}

object ProtocopterObjectProtocol {
    /** Used by actors to send messages*/
 case class LookupMessage(name : String, seen : Set[ProtocopterObject])
 case class SetMessage(name: String, value : ProtocopterObject)
 case class RecursiveLookUp(name : String, seen : ListBuffer[ProtocopterObject])
  //Response returned for a look up slot
 case class LookUpResponse(value : Option[ProtocopterObject], seen : Set[ProtocopterObject])
  //This is called
 case class ExitScope

}
