package org.protocopter.lang.core.impl

object ProtocopterImplicits {
  implicit def int2obj(value : Int) = new IntObject(value)
  implicit def str2obj(value : String) = new StringObject(value)
  implicit def double2obj(value : Double) = new DoubleObject(value)
}
