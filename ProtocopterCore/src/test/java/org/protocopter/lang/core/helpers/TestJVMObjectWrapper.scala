package org.protocopter.lang.core.helpers

import org.junit._
import Assert._

class TestJVMObjectWrapper {

  @Test
  def mustCallMethods() {
    val string = "HAI"
    val pObj = new JVMObjectWrapper(string)
    val result  = pObj.execute("toString", None)()
    
    import core.impl._
    val string2 = "HAI2"
    val otherPObj = new StringObject(string2)
    val result2 = pObj.execute("compareTo", Some(List(otherPObj)))
    ()
  }
}
