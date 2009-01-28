package org.protocopter.compiler.bytecode

import pcode._
import _root_.org.objectweb.asm._

/**
 * Converts pcode to java byte code.
 */
trait JavaByteCodeConverter {
  import Opcodes._
  
  val POBJ_TYPE = Type.getObjectType("org/protocopter/lang/core/ProtocopterObject");
  val PENV_TYPE = Type.getObjectType("org/protocopter/lang/core/ProtocopterEnvironment");
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
    mainMethod.visitMethodInsn(INVOKESTATIC, PENV_TYPE.getDescriptor(),"current",Type.getMethodDescriptor(POBJ_TYPE, Array()))
    
    //TODO - Add script arguments to "scope" object from current... 
    
    mainMethod.visitMethodInsn(INVOKESTATIC, Type.getObjectType(name).getDescriptor(), "init", Type.getMethodDescriptor(Type.VOID_TYPE, Array(POBJ_TYPE)))
    mainMethod.visitInsn(RETURN)
    //The actual values are calculated for us, we just need to call the visitor.
    mainMethod.visitMaxs(0,0)
    mainMethod.visitEnd()
    
    val moduleMethod = clazz.visitMethod(ACC_PUBLIC | ACC_STATIC, "init",Type.getMethodDescriptor(Type.VOID_TYPE, Array(POBJ_TYPE)), null, null)
    moduleMethod.visitCode()
    //TODO - Insert real code instead of
    for(pcode <- pcodes) {
      convertInstruction(moduleMethod, pcode)
    }
    
    
    moduleMethod.visitFieldInsn(GETSTATIC, "java/lang/System","out",Type.getDescriptor(System.out.getClass))
    moduleMethod.visitLdcInsn("HAI WURLD!")
    moduleMethod.visitMethodInsn(INVOKEVIRTUAL, Type.getDescriptor(classOf[java.io.PrintStream]), "println", Type.getMethodDescriptor(Type.VOID_TYPE, Array(Type.getType(classOf[Object]))))
    moduleMethod.visitInsn(RETURN)
    //The actual values are calculated for us, we just need to call the visitor.
    moduleMethod.visitMaxs(0,0)
    moduleMethod.visitEnd()
    
    
    //TODO - Create "loadModule" method with actual code.
    
    clazz.visitEnd()
    clazz
  }
  
  def convertInstruction(method : MethodVisitor, pcode : PCodeInstruction) {
    pcode match {
      case PushLiteralInstruction(lit) =>
        method.visitLdcInsn(lit)
        //TODO - Should we convert to ProtocopterObject before returning?
        //TODO - Check to make sure we're not talking about BaseObject or other...
      case PushScopeInstruction() =>
        //Pull in the first-argument to this function which is our scope.
        method.visitVarInsn(ALOAD, 0);
      case SlotAccessInstruction() =>
        //TODO - Convert from proco-obj to a string first...?
        method.visitMethodInsn(INVOKEVIRTUAL, POBJ_TYPE.getDescriptor(), "lookup", Type.getMethodDescriptor(POBJ_TYPE, Array()))
      case AssignSlot() =>
        //TODO - Convert from proco-obj to string first...?
        method.visitMethodInsn(INVOKEVIRTUAL, Type.VOID_TYPE.getDescriptor(), "set", Type.getMethodDescriptor(Type.VOID_TYPE, Array(POBJ_TYPE, POBJ_TYPE)))
      case PrototypeObject() =>
        method.visitMethodInsn(INVOKEVIRTUAL, POBJ_TYPE.getDescriptor(), "prototype", Type.getMethodDescriptor(POBJ_TYPE, Array()))
      case PushCodeBlock(block) =>
        //TODO - Write out code block as anonyomous subclass...
        //TODO - create new instance of code block subclass
      case _ => Console.println("Error... Unknown bytecode")
    }
  }
}
