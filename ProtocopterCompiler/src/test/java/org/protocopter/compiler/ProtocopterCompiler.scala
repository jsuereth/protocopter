package org.protocopter.compiler

object ProtocopterCompiler extends parser.ProtocopterParser with t2p.Tree2CodeConverter with bytecode.JavaByteCodeConverter {
  
  val myClassWriter = new bytecode.ProtocopterClassWriter() {
    override def writeClass(className : String, definition : Array[Byte]) {
       def getOutputFile = className + ".class"
       import java.io._
        //TODO - Error handling...
       val output = new FileOutputStream(getOutputFile)
       output.write(definition) 
       
       output.close()
    }
  }
  
  def compile(name : String, contents : String) {
    parseAll(module, contents) match {
      case Success(tree, next) =>
        val pcodes = map(tree)        
        compile(name, pcodes, myClassWriter)
        
        
        //TODO - Error handling...
      case Failure(msg,next) => Console.println("at line " + next.pos.line + " column " + next.pos.column +":" +  msg )
      case Error(msg,next) => Console.println("at line " + next.pos.line + " column " + next.pos.column +":"+ msg )
    }
  }
  
  
  def main(args : Array[String]) {
    Console.println("HAI")
    
    compile("testProco", """i <- 5
x <- { 
  z <- i
}
""")
  }
}
