package org.protocopter.lang.core.helpers

import core.impl._

object CallFromScalaHelpers {
  implicit def pimpProto(obj : ProtocopterObject) = new {
    //Easy method access
    def addMethod(name : String, args : List[String])(body : ProtocopterObject => ProtocopterObject) {
      obj.set(name, obj.createClosure(args, body))
    }
    //Easy slot access
    def apply(name : String) = obj.lookup(name)()
    //Execution of method
    def apply(name : String, args : ProtocopterObject*) = obj.execute(name, if(args.length > 0) Some(args.toList) else None)
  }
  
  implicit def proto2bool(obj : ProtocopterObject) = {
    obj match {
      case null => false
      case BooleanObject(value) => value
      case NilObject() => false
      case _ => true
    }
  }
  
  implicit def proto2int(obj : ProtocopterObject) = {
    obj match {
      case null => 0
      case IntObject(value) => value
      case _ => 0
    }
  }
  implicit def proto2string(obj : ProtocopterObject) = {
    obj match {
      case null => ""
      case StringObject(value) => value
      case _ => ""
    }
  }
  
}
