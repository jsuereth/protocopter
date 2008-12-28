package org.protocopter.lang.core

import core.impl._
import org.junit._
import Assert._



class TestProtocopterClosure {
  @Test
  def mustReturnCorrectly() {
    val pObj = BaseObject.prototype
    val stubReturnValue = BaseObject.prototype
    pObj.set("stub", new AbstractProtocopterClosure(List(), List(), x => stubReturnValue){})
    val result = pObj.execute("stub", None)
    assertEquals("Failed to execute closure", stubReturnValue, result())
  }
  @Test
  def mustFindArguments() {
    System.err.println("MustFindArguments!")
    val pObj = BaseObject.prototype
    val stubReturnValue = BaseObject.prototype
    val stubArg1 = new IntObject(5)
    val stubArg2 = BaseObject.prototype
    val stubClosure = new AbstractProtocopterClosure(
      List(), 
      List("arg1", "arg2"), 
      {
        context =>
        assertEquals("Failed to find argument 1",stubArg1, context.lookup("arg1")())
        assertEquals("Failed to find argument 2",stubArg2, context.lookup("arg2")())
        stubReturnValue
    }) {}
    pObj.set("stub", stubClosure)
    val result = pObj.execute("stub", Some(List(stubArg1, stubArg2)))
    assertEquals("Failed to execute closure", stubReturnValue, result())
  }
  
  @Test
  def mustPrototypeBeforeCalling() {
    val pObj = BaseObject.prototype
    val stubReturnValue = BaseObject.prototype
    val stubArg1 = BaseObject.prototype
    val stubArg2 = BaseObject.prototype
    val stubClosure = new AbstractProtocopterClosure(
      List(), 
      List("arg1", "arg2"), 
      {
        context =>
        assertEquals("Failed to find argument 1",stubArg1, context.lookup("arg1")())
        assertEquals("Failed to find argument 2",stubArg2, context.lookup("arg2")())
        stubReturnValue
    }) {}
    pObj.set("stub", stubClosure)    
    val result = pObj.execute("stub", Some(List(stubArg1, stubArg2)))
    assertEquals("Failed to execute closure", stubReturnValue, result())
    var exceptionThrown = false
    try {
      val x = stubClosure.lookup("arg1")()
      System.err.println(x)
    } catch {
      case _ => exceptionThrown = true;      
    }
    assertTrue("Should not find arguments in Closure object slots", exceptionThrown)
  }
  @Test
  def accessingObjectOnContextShouldNotBlock() {
    //THis test is failing
    
    val pObj = BaseObject.prototype
    val stubReturnValue = BaseObject.prototype
    val stubField = BaseObject.prototype
    pObj.set("slot1", stubField);
     val stubClosure = new AbstractProtocopterClosure(
      List(), 
      List(), 
      {
        context =>
          val value = context.lookup("self")().lookup("slot1")()
          assertEquals("Failed to find argument 1",stubField, value)
          stubReturnValue
    }) {}
    pObj.set("stub", stubClosure)    
    val result = pObj.execute("stub", None)
    assertEquals("Failed to execute closure", stubReturnValue, result())    
  }
}
