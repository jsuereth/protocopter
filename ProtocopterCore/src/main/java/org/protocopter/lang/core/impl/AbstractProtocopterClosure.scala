package org.protocopter.lang.core.impl

//TODO - Create "Closure" object for prototype

/**
 * Prototype for all closures
 */
object ClosurePrototype extends AbstractProtocopterObject(List(BaseObject)) {
    
  override def toString = {
    "Closure"
  }
}


/** Abstract implementation of a closure */
abstract class AbstractProtocopterClosure(ps : List[AbstractProtocopterObject], argNames : List[String], codeBlock : ProtocopterObject => ProtocopterObject) 
   extends AbstractProtocopterObject(ps ::: List(ClosurePrototype)) with ProtocopterClosure {
  
  /** Implemented in instances */
  def call(self : ProtocopterObject, args : Option[List[ProtocopterObject]]) : ProtocopterObject = {
    val executionContext = this.prototype        
    executionContext.set("self", self)    
    //TODO - Add self to the prototype list as well
    args match {
      case Some(args) => unfoldArgs(executionContext, args)
      case _ =>  
    }
    val result = executeBlock(executionContext)
    //TODO - Destroy execution context
    result
  }
  /** 
   * Pulls out arguments by name and places them in the "context" object
   * 
   * TOOD - Varargs?
   */
  protected def unfoldArgs(context : ProtocopterObject, args : List[ProtocopterObject]) = {
    for((name, argvalue) <- argNames.zip(args)) {
      context.set(name, argvalue)
    }
  }
  /** Executes the block passed in by our constructor*/
  protected def executeBlock(context : ProtocopterObject) : ProtocopterObject = codeBlock(context)
  /** overriden so we get the scala type as well as the protocopter inheritance */
  override def prototype : ProtocopterObject = new AbstractProtocopterClosure(List(this), argNames, codeBlock){}

  
}