package org.protocopter.lang.core.impl

trait BoxingUtil {
  /** Must be implemented by our parents */
  protected def createNewProtocopterObject : ProtocopterObject
  
  def box(x : Double): ProtocopterObject = {
    new DoubleObject(x)
  }
  def box(x : Int): ProtocopterObject = {
    new IntObject(x)
  }
  def box(x : Boolean): ProtocopterObject = {
    new BooleanObject(x)
  }
  def box(x : Byte): ProtocopterObject = {
    BaseObject
  }
  def box(x : AnyRef) : ProtocopterObject = {
    
    //TODO - Box values into protocopter if needed...
    BaseObject
  }
  
  def box(obj:String) : ProtocopterObject= {
    new StringObject(obj)
  }
  
  def unboxString(obj : ProtocopterObject) : String = {
    //TODO - Implement
    obj match {
      case StringObject(value) => value
      case _ => obj.toString
    }
  }
  
  
  def box(obj : Seq[ProtocopterObject]) : ProtocopterObject = {
    box(obj.toList)
  }
  
  
  
  /** Creates a protocopter object from a list of protocopter objects */
  def box(prototypes : List[ProtocopterObject]) : ProtocopterObject = {
      val parentsSlot = createNewProtocopterObject
      for( (prototype,idx) <- prototypes.zipWithIndex) {
        parentsSlot.set(box(idx.toString), prototype)
      }      
      parentsSlot
  }
  
  def unboxSeq(obj : ProtocopterObject) : Seq[ProtocopterObject] = {
    
    obj match {
      case apo : AbstractProtocopterObject =>
        /** Helper method to numerify the keyset */
        def numerifyKeys = apo.slots.keySet.filter {
          key => try {
            key.toInt
            true
          } catch {
            case _ => false
          }
        }.map(_.toInt)
        
        import collection.jcl.Conversions._        
        val numericSlots =  new java.util.TreeSet ++ numerifyKeys
        /** Returns a new anonymous sequence with all methods defined for a protocopter object */
        new Seq[ProtocopterObject]() {          
          def apply(idx : Int) = {
            apo.slots(numericSlots(idx).toString)
          }
          
          def elements = new Iterator[ProtocopterObject] {
            val internalItr = numericSlots.iterator            
            def hasNext = internalItr.hasNext
            def next = apo.slots(internalItr.next.toString)
          }
          def length = numericSlots.size
        }
      case _ => error("Unknown protocopter object type!")
    }
  }
}
