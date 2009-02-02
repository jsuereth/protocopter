package org.protocopter.lang.core.impl

object DefinitionHelper extends BoxingUtil {
  //Simple method for converting a function to a Code-Block 
  implicit def function2CodeBlock(f : ProtocopterObject => ProtocopterObject) = new CodeBlockObject() {
    def call(lexicalScope : ProtocopterObject) : ProtocopterObject = f(lexicalScope)
  }
  
  def isNil(obj : ProtocopterObject) = obj match {
    case NilObject() => true
    case _ => false
  }
  
  def createNewProtocopterObject = {   
    new CoreObject
  }
}
