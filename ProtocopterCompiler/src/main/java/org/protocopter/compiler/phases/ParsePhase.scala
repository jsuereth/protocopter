package org.protocopter.compiler.phases
/** Parse the source and creates an AST */
class ParsePhase extends parser.ProtocopterParser with Phase {
  
  def execute(ctx : PhaseExecutionContext) = {
    for( unit <- ctx.units) {
      parseUnit(unit)
    }
  }
  
  def parseUnit(unit : CompilationUnit) {
    import scala.io._
    val input = Source.fromInputStream(unit.input).getLines mkString "\n"
    parseAll(module, input) match {
      case Success(tree, next) =>
        unit.tree = Some(tree)        
        //TODO - Error handling...
      case Failure(msg,next) => Console.println("at line " + next.pos.line + " column " + next.pos.column +":" +  msg )
      case Error(msg,next) => Console.println("at line " + next.pos.line + " column " + next.pos.column +":"+ msg )
    }
  }
  
}
