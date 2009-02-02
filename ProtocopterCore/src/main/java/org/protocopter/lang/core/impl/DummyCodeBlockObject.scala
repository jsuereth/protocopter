package org.protocopter.lang.core.impl

class DummyCodeBlockObject extends CodeBlockObject {
  override def call(lexicalScope : ProtocopterObject) : ProtocopterObject = lexicalScope
}
