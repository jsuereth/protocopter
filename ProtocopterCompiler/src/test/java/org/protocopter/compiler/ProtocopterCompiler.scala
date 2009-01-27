package org.protocopter.compiler

object ProtocopterCompiler extends parser.ProtocopterParser with t2p.Tree2CodeConverter with bytecode.JavaByteCodeConverter {
  
  
  def compile(name : String, contents : String) {
    parseAll(module, contents) match {
      case Success(tree, next) =>
        val pcodes = map(tree)
        val clazz = convert(name, pcodes)
        
        import java.io._
        val output = new FileOutputStream(name + ".class")
        output.write(clazz.toByteArray)
        output.close()
        //TODO - Error handling...
      case Failure(msg,next) => Console.println("at line " + next.pos.line + " column " + next.pos.column +":" +  msg )
      case Error(msg,next) => Console.println("at line " + next.pos.line + " column " + next.pos.column +":"+ msg )
    }
  }
  
  
  def main(args : Array[String]) {
    Console.println("HAI")
    
    compile("testProco", """i <- 5
""")
  }
}
