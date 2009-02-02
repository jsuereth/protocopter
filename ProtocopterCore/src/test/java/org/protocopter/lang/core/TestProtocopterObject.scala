package org.protocopter.lang.core

import scala.actors.Actor  
import scala.actors.Future

import core.impl._


import org.junit._
import Assert._

class TestProtocopterObject {
  import ProtocopterEnvironment._
  @Test
  def prototypeMustHaveNewSlots {
    val pObj = BaseObject.prototype
    assertNotSame("BaseObject is not different than prototype", BaseObject, pObj)
    
    val arg1 = BaseObject.prototype
    val slot1 = "hai"
    
    pObj.set(box(slot1), arg1)
    
    assertEquals("Failed to set slot on object", arg1, pObj.lookup(box(slot1)).getOrElse(null))    
    assertTrue("Found slot on parent object after setting on child", BaseObject.lookup(box(slot1)).isEmpty)
    
    ()
  }
  
   @Test
  def prototypeMustInheritSlots {
    val pObj = BaseObject.prototype
    val pObj2 = pObj.prototype
    assertNotSame("BaseObject is not different than prototype", BaseObject, pObj)
    
    val arg1 = BaseObject.prototype
    val slot1 = "hai"
    
    pObj.set(box(slot1), arg1)
    
    assertEquals("Failed to find slot from prototype", arg1, pObj2.lookup(box(slot1)).get())
    ()
  }
  @Test
  def mustUpdatePrototypeList {
    val pObj = BaseObject.prototype
    val pObj2 = BaseObject.prototype
    
    ()
  }
  
  @Test
  def mustSetValue() {
    val pObj = BaseObject.prototype
    val otherObj = BaseObject.prototype
    val slotname = "hai"
    
    pObj.set(box(slotname), otherObj)
    assertNotSame("Objects should not be equal", pObj, otherObj)
    assertEquals("Failed to set slot on object", otherObj, pObj.lookup(box(slotname)).get())
  }
}
