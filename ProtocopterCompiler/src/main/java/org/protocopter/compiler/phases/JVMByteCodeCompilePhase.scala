package org.protocopter.compiler.phases

import bytecode._
/** 
 * Compiles from PCode into JVM ByteCode. 
 */
class JVMByteCodeCompilePhase extends JavaByteCodeConverter with Phase {
  
 /** Executes this phase in the given compiler context.*/
 def execute(ctx : PhaseExecutionContext) = {   
   /** The classwriter for our compilation*/
   val classWriter = new ProtocopterClassWriter {
     //TODO - Use our context
     override def writeClass(className : String, definition : Array[Byte]) {
       def getOutputFile = className + ".class"
       import java.io._
        //TODO - Error handling...
       val output = new FileOutputStream(getOutputFile)
       output.write(definition) 
       output.close()
    }
   }
   def compileUnit(unit : CompilationUnit) {
     compile(unit.name, unit.pcode.getOrElse(List()), classWriter)
   }
    for( unit <- ctx.units) {
      compileUnit(unit)
    }
  }
}
