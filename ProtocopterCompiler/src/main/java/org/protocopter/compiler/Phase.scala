package org.protocopter.compiler
/** The interface for all phases in the system. */
trait Phase {
  /** Executes this phase on the given context */
  def execute(ctx : PhaseExecutionContext)
}
