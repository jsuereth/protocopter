package org.protocopter.lang.core.helpers

import scala.actors.Future
import scala.actors.Futures._

class JVMClassWrapper(clazz : Class[_]) {
  
  
    def lookup(name : String) : Future[ProtocopterObject] = future(null)
    def execute(name : String, arg : Option[List[ProtocopterObject]]) : Future[ProtocopterObject] = future(null)
    def set(name : String, value : ProtocopterObject) {}
    
    /** Creates a prototype of this object */
    def prototype : ProtocopterObject = null
    
    /** Creates a closure with "this" object as the always available scope */
    def createClosure(args : List[String], block : ProtocopterObject => ProtocopterObject) : ProtocopterCodeBlock = null
    
    /** Helper for debuggin */
    def slotInspectionString : String = ""
}
