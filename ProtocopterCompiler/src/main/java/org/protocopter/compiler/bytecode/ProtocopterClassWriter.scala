package org.protocopter.compiler.bytecode
/**
 * Interface for a writer-of-classes
 */
trait ProtocopterClassWriter {
  /**
   * Interface to write out classes when compiling protocopter code.
   */
  def writeClass(className : String, definition : Array[Byte])
}
