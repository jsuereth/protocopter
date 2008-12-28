package org.protocopter.lang.core.helpers

import core.impl._
import org.junit._

import Assert._
import ReflectionUtil._


class Bean {
      var myIntValue = 4
      var myBoolValue = false
      var myStringValue = "hai1"
}


class TestReflectionUtil {

  @Test
  def mustMapProtocopterObjectstoJava() {
    val boolValue = true
    val boolObj = new BooleanObject(boolValue) 
    val boolResult = mapProtocoperObjectToJavaObject(boolObj, classOf[Boolean])    
    assertEquals("Failed to convert Boolean value", boolValue, boolResult)    
    
    val intValue = 5
    val intObj = new IntObject(intValue) 
    val intResult = mapProtocoperObjectToJavaObject(intObj, classOf[Int])    
    assertEquals("Failed to convert Int value", intValue, intResult)
    
    val stringValue = "hai"
    val stringObj = new StringObject(stringValue)
    val stringResult = mapProtocoperObjectToJavaObject(stringObj, classOf[String])
    assertEquals("Failed to convert String value", stringValue, stringResult)
  }

  
  @Test
  def mustSetFields() {
    val bean = new Bean
    
    val intValue = 5;
    val pIntValue = new IntObject(intValue)
    setFieldValue(bean, "myIntValue", pIntValue)    
    assertEquals("Failed to set int field", intValue, bean.myIntValue)
    
    val boolValue = true;
    val pBoolValue = new BooleanObject(boolValue)
    setFieldValue(bean, "myBoolValue", pBoolValue)    
    assertEquals("Failed to set int field", boolValue, bean.myBoolValue)
    
    val stringValue = "this will be set";
    val pStringValue = new StringObject(stringValue)
    setFieldValue(bean, "myStringValue", pStringValue)    
    assertEquals("Failed to set string field", stringValue, bean.myStringValue)
  }
  
}
