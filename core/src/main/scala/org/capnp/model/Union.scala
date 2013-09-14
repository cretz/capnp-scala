package org.capnp.model

trait Union {
  protected def unionTag: Int
}

trait UnionObject[T <: Union] {
  protected def cases: Map[Int, () => T]
  
  def apply(tag: Int): T = cases(tag)()
}

trait AnonUnionObject[T <: Struct with Union] extends UnionObject[T] with StructObject[T] {
  protected def unionTagBitOffset: Long
  
  def apply(): T = ???
  
  override def apply(msg: Option[Message], ptr: StructPtr): T = {
    val s = apply(ptr.buf.readUInt16(64L + ptr.startWord * 64L + unionTagBitOffset))
    s.buf = Some(ptr.buf.slice(64L + ptr.startWord * 64L))
    s.dataBytes = ptr.dataWords * 8
    s.pointerWords = ptr.ptrWords
    s.msg = msg
    s
  }
} 