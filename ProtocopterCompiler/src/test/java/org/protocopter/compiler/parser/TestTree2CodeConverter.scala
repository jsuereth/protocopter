package org.protocopter.compiler.parser

import compiler.tree._
import t2p._
import org.junit._
import Assert._
import pcode._

class TestTree2CodeConverter extends ProtocopterParser with Tree2CodeConverter {
 def parseHelper[R,T](parseResult : ParseResult[ASTNode])(f : PartialFunction[List[PCodeInstruction], Unit]) {
    parseResult match {
      case Success(node, next) =>
        val pcodes = map(List(node))
        
        if(f.isDefinedAt(pcodes)) {
          f(pcodes)
        } else {
          fail("Unexpected tree! " + node)
        }
      case Failure(msg,next) => fail("at line " + next.pos.line + " column " + next.pos.column +":" +  msg )
      case Error(msg,next) => fail("at line " + next.pos.line + " column " + next.pos.column +":"+ msg )
    }    
  }
 
  @Test
  def musttranslateAssignment() {
	val assignmentString = """x <- y;"""
    parseHelper(parseAll(statement, assignmentString)) {
      case x => Console.println(x)
    }
  }
}
