package org.protocopter.compiler

import pcode._
import tree._
import java.io._
/** Contains the data needed to perform compilation on a given "unit" of protocopter.  */
class CompilationUnit(val name : String, val input : InputStream) {  
  var tree : Option[List[ASTNode]] = None
  var pcode : Option[List[PCodeInstruction]] = None  
}