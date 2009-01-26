package org.protocopter.lang.core.helpers

import scala.actors._

import ReflectionUtil._
/**
 * Wraps a non-primitive JVM object for use in protocopter
 * 
 * SHOULD NOT be used for Strings, Doubles, Integers, Floats, Booleans and Longs
 */
class JVMObjectWrapper(val obj : AnyRef) {
  
    override def toString = "Wrapped(" + obj.toString + ")"
    /**
     * Sends a message to this object (with the optional arguments)
     */
    def lookup(name : String) : Future[ProtocopterObject] = {
      Futures.future(lookupLocal(name))
    }
    
    def lookupLocal(name : String) : ProtocopterObject = {
      getFieldValueOrMethods(name, obj) match {
        //TODO - AutoBox/Unbox primitives...
        case Some(Left(x)) => mapJavaObjectToProtocopterObject(x)
        //case Some(Right(x)) => new JavMethodProtocopterObject(name)
        case None => throw new IllegalAccessException("No slot found: " + name) 
      }
    }
    def execute(name : String, arg : Option[List[ProtocopterObject]]) : Future[ProtocopterObject] = {
      def executeJavaMethod = {
	      val args = arg.getOrElse(List())
	      findMethod(name, args.length, obj) match {
	        case Some(m) if m.isVarArgs =>
	          throw new IllegalAccessException("Varargs not yet implemented!")
	        case Some(m) =>
	          if(args.isEmpty) {
                val mResult = m.invoke(obj)
	            mapJavaObjectToProtocopterObject(mResult)
	          } else {
	             //TODO - Handle var-args
	             val mappedArgs : List[AnyRef]= for((argVal, argType) <- args.zip(m.getParameterTypes.toList)) yield {
	               mapProtocoperObjectToJavaObject(argVal, argType)
	             }
                 mapJavaObjectToProtocopterObject(m.invoke(obj, mappedArgs.toArray : _*))
              }
	        case None => throw new IllegalAccessException("slot: " + name + " is not executable")
	      }
      }
     Futures.future(executeJavaMethod)    
    }
    def set(name : String, value : ProtocopterObject)= {
      //TODO - Look for setters?
      setFieldValue(obj, name, value)
    }
    
    /** Creates a prototype of this object */
    def prototype : ProtocopterObject = error("TODO - Not Implemented")
    /** Creates a closure with "this" object as the always available scope */
    def createClosure(args : List[String], block : ProtocopterObject => ProtocopterObject) : ProtocopterCodeBlock = error("TODO - not implemented")
    
    /** Helper for debuggin */
    def slotInspectionString : String = {
      obj.getClass.getCanonicalName + getSlotNames(obj).mkString(" slots(", ", ", ")")
    }
    
    private[this] class JavMethodProtocopterObject(name : String) {
      /**
       * Sends a message to this object (with the optional arguments)
       */
      def lookup(name : String) : Future[ProtocopterObject] = Futures.future(throw new IllegalAccessException("Java methods do not have slots! (yet)"))
      def execute(name : String, arg : Option[List[ProtocopterObject]]) : Future[ProtocopterObject] = {
          throw new IllegalAccessException("Cannot execute slots on java method objects!(yet)")
      }
      def set(name : String, value : ProtocopterObject) = Futures.future(throw new IllegalAccessException("Java methods do not have slots! (yet)"))
    
      /** Creates a prototype of this object */
      def prototype : ProtocopterObject = throw new IllegalAccessException("Java methods cannot be prototyped! (yet)")
      /** Creates a closure with "this" object as the always available scope */
      def createClosure(args : List[String], block : ProtocopterObject => ProtocopterObject) : ProtocopterCodeBlock = {
        throw new IllegalAccessException("Java methods cannot be used for creating closures!")
      }
    
      /** Helper for debuggin */
      def slotInspectionString : String = toString
      /** Calls the block of code with "self" as the reference object and "args" as arguments */
	  def call(self : ProtocopterObject, args : Option[List[ProtocopterObject]]) : ProtocopterObject = {
	    JVMObjectWrapper.this.execute(name, args)()
	  }
      override def toString = "java.lang.Method"
    } 
}
