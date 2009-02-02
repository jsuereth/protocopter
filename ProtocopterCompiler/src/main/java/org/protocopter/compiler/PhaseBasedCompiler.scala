package org.protocopter.compiler
/** A Compiler that operates on a group of phases */
class PhaseBasedCompiler(val phases : List[Phase]) {
  
  
  import java.io._
  def compile(units : List[(String, InputStream)]) {
    
    //Creates a context for us to execute under
    val ctx = new PhaseExecutionContext {}    
    ctx.units = units map {
      case (name, input) =>
        new CompilationUnit(name, input)
    }
    //All phases use the same context, so they can perform tree/pcode manipulations if needed
    for(phase <- phases) {
      phase.execute(ctx)
    }
  }
}

import phases._
object DefaultCompiler {
  def createCompiler : PhaseBasedCompiler = {
    val phases = List(new ParsePhase(), new TreeToPCodePhase(), new JVMByteCodeCompilePhase())
    new PhaseBasedCompiler(phases)
  }
}