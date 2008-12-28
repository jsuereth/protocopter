package org.protocopter.compiler.parser

import scala.util.parsing.combinator._
import scala.util.parsing._
import scala.util.parsing.input._

class ProtocopterParser extends RegexParsers with JavaTokenParsers {
  
  import compiler.tree._
  
  
  override protected val whiteSpace = """[ \t\r]+""".r
  
  def module = rep(statement)
  
  def statement : Parser[ASTNode] = (assign
                                     | prototype
                                     | appendPrototype
                                     | expr) <~ eol 
  
  //TODO - Actually infer semi-colons instead of making EOL *always* be a semi-colon
  def eol : Parser[String] = ";" | "\n"
  
  def assign = (expr <~ "<-") ~ expr ^^ { case l ~ r => Assignment(l,r)}
  def prototype= (expr <~ "<~") ~ expr ^^ { case l ~ r => Prototype(l,r)}
  def appendPrototype = (expr <~ "<<") ~ expr ^^ { case l ~ r => AppendPrototype(l,r)}
  
  
  //This should parse expressions of identifiers/literals and messages
  def expr : Parser[ASTNode] = (message | term) 
  
  def message = term ~ messageType ^^ {case expr ~ msg => Message(expr,msg)}
  def messageType = slotAccess | functionCall
  def term : Parser[ASTNode] = (identifier 
                                | literals 
                                | closure
                                | "(" ~> expr <~ ")")
  
  
  
  //Slot access rules
  def slotAccess : Parser[MessageType] = "." ~> identifier ^^ { case x => SlotAccess(x)}    

  //Function call grammar rules?
  def callArgs = "(" ~> rep1sep(expr, ",") <~ ")" ^^ { case x => ExecuteArgument(x) }
  def functionCall : Parser[MessageType] = "!" ~> identifier ~ opt(callArgs) ^^ { case func ~ args => ExecuteCall(func, args) }
  
  //Closure Grammar rules
  //TODO - Clean up the look of this one  bit
  def closure = ("{" ~ opt(rep(eol))) ~> (opt(closureArgs) <~ opt(rep(eol))) ~ rep(statement) <~ (opt(rep(eol)) ~ "}") ^^ { case x ~ y => Closure(x,y)}
  def closureArgs = "|" ~> rep1sep(ident, ",") <~ "|" ^^ { case x => Arguments(x) }  
  
  //Literal/Identifier grammar rules
  def identifier = directIdent | indirectIdent
  def indirectIdent = "@" ~> expr ^^ { case x => IndirectIdentifer(x) }
  def directIdent = ident ^^ { case x => DirectIdentifier(x) }  
  def literals : Parser[ASTNode] = ( 
                  (stringLiteral  ^^ { case x => Literal(x.slice(1, x.length-1).toString)})
                  //TODO - Parse floats?
                  | (floatingPointNumber  ^^ { case x => Literal(x.toFloat)})
                  | (booleanLiteral ^^ { case x => Literal(x)})
                 )     
  def booleanLiteral : Parser[Boolean] = ("true" ^^ {case x => true } 
                                          | "false" ^^ {case x => false}) 
  
}
