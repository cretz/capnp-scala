package org.capnp.model

trait Union {
  protected def unionTag: Int
}

trait UnionObject[T <: Union] {
  protected def cases: Map[Int, () => T]
  
  def apply(tag: Int): T = cases(tag)()
}

abstract class AnonUnionObject[T <: Struct with Union] extends StructBuildable[T] {
  protected def cases: Map[Int, StructObject[_ <: T]]
  protected def unionTagBitOffset: Long
  
  override def apply(ptr: StructPtr): T =
    cases(ptr.buf.readUInt16(64L + ptr.startWord * 64L + unionTagBitOffset))(ptr)
} 