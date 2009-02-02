package org.protocopter.compiler
/**
 * Main entry point into the compiler
 */
object ProtocopterCompiler {
  
  
  def main(args:Array[String]) {
   //TODO - Handle args for real
    
    //HACK - for now just treat all inputs as source and pump out bytecode...
    val sources = for(arg <- args) yield {
      import java.io._
      (removeExtension(arg), new FileInputStream(arg))
    }    
    DefaultCompiler.createCompiler.compile(sources.toList)
  }
  
  def removeExtension(arg : String) = {
    arg.slice(0,arg.lastIndexOf('.')).toString
  }
  
  
}
