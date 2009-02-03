package org.protocopter.lang.core


/** 
 * Trait representing any object in Protocopter.  This is the "pubic" interface for java/scala 
 */
trait ProtocopterObject { 
    /**
     * Looks up a slot from this object.
     */
    def lookup(name : ProtocopterObject) : ProtocopterObject
    /**
     * Removes a slot from this object
     */
    def remove(name : ProtocopterObject)
    /**
     * Sets the value of a slot on this obejct
     */
    def set(name : ProtocopterObject, value : ProtocopterObject)        
    /** 
     * Creates a prototype of this object 
     */
    def prototype : ProtocopterObject    
}




