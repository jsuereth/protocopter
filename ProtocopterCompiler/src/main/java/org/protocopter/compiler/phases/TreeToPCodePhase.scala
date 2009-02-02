package org.protocopter.compiler.phases

import t2p._
/** Converts AST into pcode */
class TreeToPCodePhase extends Tree2CodeConverter with Phase {
  def execute(ctx : PhaseExecutionContext) = {
    for( unit <- ctx.units) {
      translateUnit(unit)
    }
  }
  
  def translateUnit(unit : CompilationUnit) {
    val pcodes = unit.tree.map(map(_))
    unit.pcode = pcodes;
  }
}
