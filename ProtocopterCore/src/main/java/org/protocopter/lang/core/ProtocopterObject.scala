package org.protocopter.lang.core

import scala.actors._

/** 
 * Trait representing any object in Protocopter.  This is the "pubic" interface for java/scala 
 */
trait ProtocopterObject {
    /**
     * Sends a message to this object (with the optional arguments)
     */
    def lookup(name : String) : Option[ProtocopterObject]
    def set(name : String, value : ProtocopterObject)        
    /** Creates a prototype of this object */
    def prototype : ProtocopterObject
    
    def listSlots() : List[String]
}




