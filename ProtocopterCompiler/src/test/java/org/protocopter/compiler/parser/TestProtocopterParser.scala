package org.protocopter.compiler.parser

import compiler.tree._

import org.junit._
import Assert._

class TestProtocopterParser extends ProtocopterParser {

  def parseHelper[R,T](parseResult : ParseResult[T])(f : PartialFunction[T, Unit]) {
    parseResult match {
      case Success(node, next) =>
        if(f.isDefinedAt(node)) {
          f(node)
        } else {
          fail("Unexpected tree! " + node)
        }
      case Failure(msg,next) => fail("at line " + next.pos.line + " column " + next.pos.column +":" +  msg )
      case Error(msg,next) => fail("at line " + next.pos.line + " column " + next.pos.column +":"+ msg )
    }    
  }
  
  @Test
  def mustParseIdentifier() {
    val id = "hai"
    parseHelper(parseAll(expr, id)) {
      case DirectIdentifier(name) => assertEquals("Failed to parse correct identifier", id, name)
    }
  }
  @Test
  def mustParseStrings() {
     val exprToParse = "\"hai\""
    parseHelper(parseAll(expr, exprToParse)) {
      case Literal(name : String) => assertEquals("Failed to parse correct identifier", "hai", name)
    }
  }
  @Test
  def mustParseCallMessage() {
    val id = "hai"
    val slot = "josh"
    
    parseHelper(parseAll(messageType, "!" + slot)) {
      case ExecuteCall(DirectIdentifier(name), None) => assertEquals("Failed to parse slot access", slot, name)
    }
    parseHelper(parseAll(message, id + "!" + slot)) {
	    case Message(DirectIdentifier(name), ExecuteCall(DirectIdentifier(slotName), None)) => 
	      assertEquals("identifier and parsed id do not match", id, name)
	      assertEquals("slot and parsed slot do not match", slot, slotName)
    }
    parseHelper(parseAll(expr, id + "!" + slot)) {
	    case Message(DirectIdentifier(name), ExecuteCall(DirectIdentifier(slotName), None)) => 
	      assertEquals("identifier and parsed id do not match", id, name)
	      assertEquals("slot and parsed slot do not match", slot, slotName)
    }
  }
  
  @Test
  def mustParseRegularSlotAccess() {
    val id = "hai"
    val slot = "josh"
    
    parseHelper(parseAll(messageType, "." + slot)) {
      case SlotAccess(DirectIdentifier(name)) => assertEquals("Failed to parse slot access", slot, name)
    }
    
    parseHelper(parseAll(message, id + "." + slot)) {
	    case Message(DirectIdentifier(name), SlotAccess(DirectIdentifier(slotName))) => 
	      assertEquals("identifier and parsed id do not match", id, name)
	      assertEquals("slot and parsed slot do not match", slot, slotName)
    }
    parseHelper(parseAll(expr, id + "." + slot)) {
	    case Message(DirectIdentifier(name), SlotAccess(DirectIdentifier(slotName))) => 
	      assertEquals("identifier and parsed id do not match", id, name)
	      assertEquals("slot and parsed slot do not match", slot, slotName)
    }
  }
  @Test
  def mustParseCodeBlock() {
    val closureString = """{ |x| x; }"""
    parseHelper(parseAll(expr, closureString)) {
      case ArgumentCodeBlock(Arguments(List(arg)), List(DirectIdentifier(slotName))) =>
        assertEquals("Failed to parse arguments", "x", arg)
        assertEquals("Failed to parse slot Access", "x", slotName)
    }
  }
  @Test
  def mustParseIndirectSlotnames() {
    val slotAccessString = """obj.@otherObj"""
    parseHelper(parseAll(expr, slotAccessString)) {
      case Message(DirectIdentifier(name), SlotAccess(IndirectIdentifer(DirectIdentifier(slotName)))) =>
        assertEquals("Failed to parse arguments", "obj", name)
        assertEquals("Failed to parse arguments", "otherObj", slotName)
    }
  }
  
  @Test
  def mustParseInterestingClosure() {
    val closureString = """{ |x,y| x;y; }"""
    parseHelper(parseAll(expr, closureString)) {
      case ArgumentCodeBlock(Arguments(List(x,y)), List(DirectIdentifier(x2),DirectIdentifier(y2))) =>
        assertEquals("Failed to parse arguments", "x", x)
        assertEquals("Failed to parse arguments", "y", y)
        assertEquals("Failed to parse statement1", "x", x2)
        assertEquals("Failed to parse statment2", "y", y2)
    }
  } 
  
  @Test
  def mustParseInterestingClosureWithEOLs() {
    val closureString = """{ 
           |x, y| 
           x
           y 
}"""
    parseHelper(parseAll(expr, closureString)) {
      case ArgumentCodeBlock(Arguments(List(x,y)), List(DirectIdentifier(x2),DirectIdentifier(y2))) =>
        assertEquals("Failed to parse arguments", "x", x)
        assertEquals("Failed to parse arguments", "y", y)
        assertEquals("Failed to parse statement1", "x", x2)
        assertEquals("Failed to parse statment2", "y", y2)
    }
  }
  
    @Test
  def mustParseInterestingCodeBlockEOLs() {
    val closureString = """{ 
           x
           y 
}"""
    parseHelper(parseAll(expr, closureString)) {
      case CodeBlock(List(DirectIdentifier(x2),DirectIdentifier(y2))) =>
        assertEquals("Failed to parse statement1", "x", x2)
        assertEquals("Failed to parse statment2", "y", y2)
    }
  }
  @Test
  def mustParseInterestingClosureEOLs() {
    val closureString = """[ 
           x
           y 
]"""
    parseHelper(parseAll(expr, closureString)) {
      case Closure(List(DirectIdentifier(x2),DirectIdentifier(y2))) =>
        assertEquals("Failed to parse statement1", "x", x2)
        assertEquals("Failed to parse statment2", "y", y2)
    }
  }
  
  @Test
  def mustParseInterestingClosureWithArgsEOLs() {
    val closureString = """[ 
           |x, y| 
           x
           y 
]"""
    parseHelper(parseAll(expr, closureString)) {
      case ArgumentClosure(Arguments(List(x,y)), List(DirectIdentifier(x2),DirectIdentifier(y2))) =>
        assertEquals("Failed to parse arguments", "x", x)
        assertEquals("Failed to parse arguments", "y", y)
        assertEquals("Failed to parse statement1", "x", x2)
        assertEquals("Failed to parse statment2", "y", y2)
    }
  }
  
  
  @Test
  def mustParseAssignment() {
    val assignmentString = """x <- y;"""
    parseHelper(parseAll(statement, assignmentString)) {
      case Assignment(DirectIdentifier(name), DirectIdentifier(value)) =>
        assertEquals("Failed to parse arguments", "x", name)
        assertEquals("Failed to parse slot Access", "y", value)
    }
  }
}

