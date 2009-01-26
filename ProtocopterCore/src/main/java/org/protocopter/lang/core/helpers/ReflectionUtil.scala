package org.protocopter.lang.core.helpers

import scala.reflect.Manifest

object ReflectionUtil {
  val STRING_TYPE = classOf[String]
  val INT_TYPE = classOf[Int]
  val DOUBLE_TYPE = classOf[Double]
  val FLOAT_TYPE = classOf[Float]
  val LONG_TYPE = classOf[Long]
  val BOOL_TYPE = classOf[Boolean]
  val OBJ_TYPE = classOf[Object]
  
  import impl._
  def proto2string(obj : ProtocopterObject) : String = obj match {
    case BooleanObject(value) => value.toString
    case StringObject(value) => value
    case IntObject(value) => value.toString
    case NilObject() => ""
    case _ => "" //TODO - To Strings...
  }
  def proto2bool(obj : ProtocopterObject) : Boolean = obj match {    
    case BooleanObject(value) => value
    case StringObject(value) => value != null && !value.isEmpty
    case IntObject(value) => value != 0
    case NilObject() => false
    case _ => true
  }
  
  def proto2int(obj : ProtocopterObject) : Int = obj match {
    case IntObject(value) => value
    case _ => 0
  }
  /**
   * Maps a protocopter object into a java type.
   */
  def mapProtocoperObjectToJavaObject[A](obj : ProtocopterObject, clazz : Class[A]) : A = {    
    clazz match {
      case STRING_TYPE => proto2string(obj).asInstanceOf[A]
      case BOOL_TYPE => proto2bool(obj).asInstanceOf[A]
      case INT_TYPE => proto2int(obj).asInstanceOf[A]
      case OBJ_TYPE => obj.asInstanceOf[A]
      case _ => throw new IllegalArgumentException("cannot cast protocopter objec to: " + clazz) 
    }
  }
  /** 
   * Converts a java object to the equivalent protocopter object
   * TODO - Use flyweights/immutables for primitives + strings?
   */
  def mapJavaObjectToProtocopterObject[A](value : A) : ProtocopterObject = {
    import impl._
    value match {
      case x : Boolean => new BooleanObject(x)
      case x : String => new StringObject(x)
      case x : Int => new IntObject(x)
      //case x : Double => new DoubleObject(x)
      //case x : AnyRef => new JVMObjectWrapper(x)
      case _ => throw new UnsupportedOperationException("Cannot convert " + value + " to protocopter")
    }
  }
  
  /** Returns the slot names on a java object */
  def getSlotNames(obj : AnyRef) : Seq[String] = {
    val methods = for { 
      m <- obj.getClass.getMethods
    } yield m.getName
    val fields = for { 
      f <- obj.getClass.getFields
    } yield f.getName
    (Set() ++ methods ++ fields).toSeq
  }
  /** Finds a method on a class. */
  def findMethod(name : String, numArgs : Int, obj : AnyRef) : Option[java.lang.reflect.Method] = {
    val methods = for {
      m <- obj.getClass.getMethods
      if name == m.getName
      if m.getParameterTypes.size == numArgs || m.isVarArgs //TODO better checks with varargs
    } yield m
    methods.firstOption
  }
  /** Finds all methods of a given name on an object */
  def findMethods(name : String, obj : AnyRef) : Seq[java.lang.reflect.Method] = {
    for { m <- obj.getClass.getMethods 
      if name == m.getName
    } yield m
  }
  /**
   * Sets the field with name "name" on object "obj" with value "value"
   */
  def setFieldValue(obj : AnyRef, name : String, value : ProtocopterObject) {
    findField(name, obj) match {
      case Some(field) =>  
        val javaValue = mapProtocoperObjectToJavaObject(value, field.getType)
        field.setAccessible(true)
        field.getType match {
          case BOOL_TYPE => field.setBoolean(obj, javaValue.asInstanceOf[Boolean])
          case INT_TYPE => field.setInt(obj, javaValue.asInstanceOf[Int])
          case LONG_TYPE => field.setLong(obj, javaValue.asInstanceOf[Long])
          case FLOAT_TYPE => field.setFloat(obj, javaValue.asInstanceOf[Float])
          case DOUBLE_TYPE => field.setDouble(obj, javaValue.asInstanceOf[Double])          
          case _ => field.set(obj, javaValue)
        }
      case None => throw new IllegalAccessException("Cannot set slot: " + name + " on java object: " + obj)
    }
    ()
  }
  /** Finds a field on a java method */
  def findField(name : String, obj : AnyRef) : Option[java.lang.reflect.Field] = {
     val fieldOptions = for { 
      field <- obj.getClass.getDeclaredFields //TODO - Do we need plain-old get fields?
      if name == field.getName
    } yield field
     fieldOptions.firstOption
  }
  //Finds a field of a given name on an object, returns the value
  def findFieldValue(name : String, obj : AnyRef) : Option[Any] = {
    //Helper method to wrap field value (be it int/bool/etc.
    def getFieldValue(field : java.lang.reflect.Field) = {
        field.setAccessible(true)
        //TODO - Check type of field and call appropriate getter
        field.getType match {
          case INT_TYPE => field.getInt(obj)
          case LONG_TYPE => field.getLong(obj)
          case BOOL_TYPE => field.getBoolean(obj)
          case FLOAT_TYPE => field.getFloat(obj)
          case DOUBLE_TYPE => field.getDouble(obj)
          case _ => field.get(obj)
      } 
   }
    
    findField(name,obj) map {
      field => getFieldValue(field)         
    }
  }
  /** Returns the value of a field name *or* a method */
  def getFieldValueOrMethods(name : String, obj : AnyRef) : Option[Either[Any, Seq[java.lang.reflect.Method]]] = {
    findFieldValue(name, obj) match {
      case Some(x) => Some(Left(x))
      case None => findMethods(name, obj) match {
        case Nil => None
        case result @ _ => Some(Right(result))
      }
    }    
  }
  
}
