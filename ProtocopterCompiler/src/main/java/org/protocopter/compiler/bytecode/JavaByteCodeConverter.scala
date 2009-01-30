package org.protocopter.compiler.bytecode

import pcode._
import _root_.org.objectweb.asm._


/**
 * Converts pcode to java byte code.
 */
trait JavaByteCodeConverter {
  import Opcodes._
  

  val POBJ_TYPE = Type.getObjectType("org/protocopter/lang/core/ProtocopterObject")
  val PENV_TYPE = Type.getObjectType("org/protocopter/lang/core/ProtocopterEnvironment")
  val PCODE_BLOCK_TYPE = Type.getObjectType("org/protocopter/lang/impl/CodeBlockObject")
  
  
  protected def getTypeForClass(name : String) = Type.getObjectType(name)
  
  
  
  /** Mangles a code block name */
  private def mangleCodeBlockName(moduleName:String, codeBlockIdx : Long) = moduleName + "$$CodeBlock$$" + codeBlockIdx
  
  /** Compiles a module from its PCodes into JVM Byte code */
  def compile(moduleName : String, codeBlock : List[PCodeInstruction], writer : ProtocopterClassWriter) : Unit = {
    //TODo - take in source code    
    for( (name,pclass) <- extractClasses(moduleName, ProtocopterModule(codeBlock)) ) {
      writer.writeClass(name, convert(name, pclass).toByteArray)
    }
    
  }
  
  
  /**
   * extracts the various compilation units needed.
   */
  def extractClasses(name : String, module : PClass) : List[(String, PClass)] = {
    var idx = 1;
    val codeBlocks = for {
      pcode <- module.definition
      if pcode.isInstanceOf[PushCodeBlock]
      val PushCodeBlock(block) = pcode
    } yield block
    
    val results = codeBlocks.zipWithIndex.flatMap {
      case (block,idx) => 
        val blockName = mangleCodeBlockName(name, idx)
        val blockObj = ProtocopterCodeBlock(block.toList)
        (blockName, blockObj ) :: extractClasses(blockName, blockObj )      
    }
    
    (name, module) :: results
  }
  
  
  def convert(className : String, pclass : PClass) = pclass match {
    case m @ ProtocopterModule(_) => convertModule(className, m)
    case cb @ ProtocopterCodeBlock(_) => convertCodeBlock(className, cb)
  }
  
  /**
   * Creates .class file for a protocopter module
   * 
   * TODO - Take in a CompilationUnit or SourceTranslation or some robust object...
   * 
   * @name The name of the module
   * @pcodes  The definition of the module
   */
  def convertModule(name : String, module : ProtocopterModule)= {
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
    var idx = 0;
    for(pcode <- module.definition) {
      //TODO - better way of managing code block name mangling!
      if(pcode.isInstanceOf[PushCodeBlock]) { idx += 1 }
      convertInstruction(moduleMethod, pcode, name, idx);     
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
  
  /** Converts a code block into a class file */
  def convertCodeBlock(name : String, codeBlock : ProtocopterCodeBlock) = {
    val clazz = new ClassWriter(ClassWriter.COMPUTE_MAXS)
    //TODO - Debugging info?    
    //TODO - We should extend some kind of protocopter interface...?
    clazz.visit(V1_6, ACC_PUBLIC | ACC_FINAL, name, null, PCODE_BLOCK_TYPE.getInternalName, null)
    
    //Write constructor
    val constructor = clazz.visitMethod(ACC_PUBLIC, name, Type.getMethodDescriptor(Type.VOID_TYPE, Array()), null, null)
    constructor.visitCode()
    constructor.visitVarInsn(ALOAD, 0);
    constructor.visitMethodInsn(INVOKESPECIAL, POBJ_TYPE.getInternalName,"<init>", Type.getMethodDescriptor(Type.VOID_TYPE, Array()))
    //Max stack is computed for us.
    constructor.visitMaxs(0,0)
    constructor.visitEnd()
    
    //Write method impl 
    val callMethod = clazz.visitMethod(ACC_PUBLIC | ACC_STATIC, "call",Type.getMethodDescriptor(Type.VOID_TYPE, Array(POBJ_TYPE)), null, null)
    callMethod.visitCode()
    //TODO - Insert real code instead of
    var idx = 0;
    for(pcode <- codeBlock.definition) {
      //TODO - better way of managing code block name mangling!
      if(pcode.isInstanceOf[PushCodeBlock]) { idx += 1 }
      convertInstruction(callMethod, pcode, name, idx)     
    }
    callMethod.visitMaxs(0,0)
    callMethod.visitEnd()
    
    clazz.visitEnd()
    clazz
  }
  
  /** Converts PCode instructions into corresponding JVM byte-code instructions. */
  def convertInstruction(method : MethodVisitor, pcode : PCodeInstruction, name : String, idx : Long) : Unit = {
    pcode match {
      case PushLiteralInstruction(lit) =>
        method.visitLdcInsn(lit)
        //TODO - Should we convert to ProtocopterObject before returning?
        //TODO - Check to make sure we're not talking about BaseObject or other...
      case PushScopeInstruction() =>
        //Pull in the first-argument to this function which is our scope.
        method.visitVarInsn(ALOAD, 0)
      case SlotAccessInstruction() =>
        //TODO - Convert from proco-obj to a string first...?
        method.visitMethodInsn(INVOKEVIRTUAL, POBJ_TYPE.getDescriptor(), "lookup", Type.getMethodDescriptor(POBJ_TYPE, Array(POBJ_TYPE)))
      case AssignSlot() =>
        //TODO - Convert from proco-obj to string first...?
        method.visitMethodInsn(INVOKEVIRTUAL, Type.VOID_TYPE.getDescriptor(), "set", Type.getMethodDescriptor(Type.VOID_TYPE, Array(POBJ_TYPE, POBJ_TYPE)))
      case PrototypeObject() =>
        method.visitMethodInsn(INVOKEVIRTUAL, POBJ_TYPE.getDescriptor(), "prototype", Type.getMethodDescriptor(POBJ_TYPE, Array()))
      case PushCodeBlock(block) =>
        //TODO - Write out code block as anonyomous subclass...
        //create new instance of code block subclass
        method.visitTypeInsn(NEW, getTypeForClass(mangleCodeBlockName(name, idx)).getDescriptor)
      case ExecuteFunction() =>
        //TODO - we need to understand how many argument there were, OR us varargs?
      case DeleteSlot() =>
        method.visitMethodInsn(INVOKEVIRTUAL, POBJ_TYPE.getDescriptor(), "remove", Type.getMethodDescriptor(Type.VOID_TYPE, Array(POBJ_TYPE)))
      case PushReferenceInstruction() =>
        //TODO - Isn't this the same as slot access?
      case _ => Console.println("Error... Unknown bytecode")
    }
  }
}
