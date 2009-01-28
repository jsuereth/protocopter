package org.protocopter.lang.core



object ProtocopterEnvironment {
  def current : ProtocopterObject = {
    import impl._
    BaseObject.prototype
  }
}