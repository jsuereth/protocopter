package org.protocopter.lang.core.impl

object DefinitionHelper {
  //Simple method for converting a function to a Code-Block 
  implicit def function2CodeBlock(f : ProtocopterObject => ProtocopterObject) = new CodeBlockObject() {
    def call(lexicalScope : ProtocopterObject) : ProtocopterObject = f(lexicalScope)
  }
  
  
  def object2int(obj : ProtocopterObject) = obj match {
    case IntObject(value) => Some(value)
    case _ => None
  }
  
  def isNil(obj : ProtocopterObject) = obj match {
    case NilObject() => true
    case _ => false
  }
  
  
  def object2string(obj : ProtocopterObject) = obj match {
    case StringObject(value) => Some(value)
    case _ => None
  }
  /** Adds methods to the Protocopter Object for easier use.*/  
  class PimpedProtocopterObject(obj : AbstractProtocopterObject) {
    def getPrototypes : List[ProtocopterObject] = {
      if(obj.slots.contains("prototypes")) {
        ( 
          for(prototype <- object2list(obj.slots("prototypes"))) 
            yield prototype
        ).toList
      } else {
        Nil
      }
    }
  }
  implicit def pimpObj(obj : AbstractProtocopterObject) = new PimpedProtocopterObject(obj)
  
  /** Allows us to perform for expressions over protocopter objects (if they are lists...)*/
  def object2list(obj : ProtocopterObject) = new Iterable[ProtocopterObject] {
      /** Creates a new iterator over all elements contained in this
	   *  object.
	   *  
	   *  @return the new iterator
	   */
	  def elements: Iterator[ProtocopterObject] = new Iterator[ProtocopterObject] {
	      var idx = 0
	    
		  /** Does this iterator provide another element?
		   */
		  def hasNext: Boolean = obj.lookup(idx.toString) match {
		    case None => false
		    case Some(NilObject()) => false
            case _ => true
		  }
		
		  /** Returns the next element.
		   */
		  def next(): ProtocopterObject  = {
		    val oldIdx = idx
            idx += 1
		    obj.lookup(oldIdx.toString) match {
		      case None => null
              case Some(x) => x
		    }
           }
      }
  }
}
