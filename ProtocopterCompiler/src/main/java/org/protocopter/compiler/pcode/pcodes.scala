package org.protocopter.compiler.pcode

/** Abstract interface for all "Protocopter ByteCodes" or pcodes */
sealed trait PCodeInstruction

case class PushReferenceInstruction() extends PCodeInstruction
case class SlotAccessInstruction() extends PCodeInstruction
case class PushScopeInstruction() extends PCodeInstruction
case class PushLiteralInstruction(literal : Any) extends PCodeInstruction
case class PushCodeBlock(block : Seq[PCodeInstruction]) extends PCodeInstruction
case class ExecuteFunction() extends PCodeInstruction
case class PrototypeObject() extends PCodeInstruction

case class AssignSlot() extends PCodeInstruction
case class DeleteSlot() extends PCodeInstruction

