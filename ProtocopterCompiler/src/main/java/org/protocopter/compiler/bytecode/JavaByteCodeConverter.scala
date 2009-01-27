package org.protocopter.compiler.bytecode

import pcode._
import _root_.org.objectweb.asm._

/**
 * Converts pcode to java byte code.
 */
trait JavaByteCodeConverter {
  import Opcodes._
  /**
   * Creates .class file for a protocopter module
   * 
   * TODO - Take in a CompilationUnit or SourceTranslation or some robust object...
   * 
   * @name The name of the module
   * @pcodes  The definition of the module
   */
  def convert(name : String, pcodes : List[PCodeInstruction])= {
    val clazz = new ClassWriter(ClassWriter.COMPUTE_MAXS)
    //TODO - Debugging info?
    
    //TODO - We should extend some kind of protocopter interface...?
    clazz.visit(V1_6, ACC_PUBLIC | ACC_FINAL, name, null, "java/lang/Object", null)
    //TODO - Does the source come first, or after?
    clazz.visitSource(name + ".proco", null)
    
    //TODO - java main class should call "loadModule" method
    val mainMethod = clazz.visitMethod(ACC_PUBLIC | ACC_STATIC | ACC_VARARGS, "main", Type.getMethodDescriptor(Type.VOID_TYPE, Array(Type.getType(classOf[Array[String]]))), null, null)
    mainMethod.visitCode()
    mainMethod.visitFieldInsn(GETSTATIC, "java/lang/System","out",Type.getDescriptor(System.out.getClass))
    mainMethod.visitLdcInsn("HAI WURLD!")
    mainMethod.visitMethodInsn(INVOKEVIRTUAL, Type.getDescriptor(classOf[java.io.PrintStream]), "println", Type.getMethodDescriptor(Type.VOID_TYPE, Array(Type.getType(classOf[Object]))))
    mainMethod.visitInsn(RETURN)
    //The actual values are calculated for us, we just need to call the visitor.
    mainMethod.visitMaxs(0,0)
    mainMethod.visitEnd()
    
    
    //TODO - Create "loadModule" method with actual code.
    
    clazz.visitEnd()
    clazz
  }
}
