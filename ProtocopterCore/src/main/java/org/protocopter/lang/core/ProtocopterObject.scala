package org.protocopter.lang.core

import scala.actors._

/** 
 * Trait representing any object in Protocopter.  This is the "pubic" interface for java/scala 
 */
trait ProtocopterObject {
    /**
     * Sends a message to this object (with the optional arguments)
     */
    def lookup(name : String) : Future[ProtocopterObject]
    def execute(name : String, arg : Option[List[ProtocopterObject]]) : Future[ProtocopterObject]
    def set(name : String, value : ProtocopterObject)
    
    /** Creates a prototype of this object */
    def prototype : ProtocopterObject
    /** Creates a closure with "this" object as the always available scope */
    def createClosure(args : List[String], block : ProtocopterObject => ProtocopterObject) : ProtocopterClosure
    
    /** Helper for debuggin */
    def slotInspectionString : String
    
    
}




