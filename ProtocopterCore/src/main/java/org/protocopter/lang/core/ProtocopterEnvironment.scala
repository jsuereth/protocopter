package org.protocopter.lang.core



object ProtocopterEnvironment extends impl.BoxingUtil {
  /**
   * Returns the current execution context
   */
  def current() : ProtocopterObject = {
    import impl._
    BaseObject.prototype
  }
  
  override def createNewProtocopterObject() = {
    import impl._
    new CoreObject
  }
  
  
  
  
}