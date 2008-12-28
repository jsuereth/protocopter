package org.protocopter.lang.core

/** 
 * Trait representing a callable method of code.  All protocopter closures/methods extend this trait 
 */
trait ProtocopterClosure extends ProtocopterObject {
  //TODO - Do we need a self type of ProtocopterObject
  
  /** Calls the block of code with "self" as the reference object and "args" as arguments */
  def call(self : ProtocopterObject, args : Option[List[ProtocopterObject]]) : ProtocopterObject
}
