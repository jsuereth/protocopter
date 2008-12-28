package org.protocopter.compiler.interpreter

import java.io._

object Interpreter extends ProtocopterInterpreterContext(System.out) {
  val input = new BufferedReader(new InputStreamReader(System.in))

  def main(args : Array[String]) {	  
	  var line = input.readLine
	  while(line != null) {	    
	    eval(line)
	    line = input.readLine
	  }
  }
}
