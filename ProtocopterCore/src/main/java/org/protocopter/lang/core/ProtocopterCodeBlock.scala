package org.protocopter.lang.core

/** 
 * Trait representing a callable method of code.  All protocopter closures/methods extend this trait 
 */
trait ProtocopterCodeBlock extends ProtocopterObject {  
  //TODO - maybe we just have a call method?  
  def call(lexicalScope : ProtocopterObject) : ProtocopterObject
}
