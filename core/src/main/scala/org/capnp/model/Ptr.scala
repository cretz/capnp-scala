package org.capnp.model

sealed trait Ptr {
  val seg: Message#Segment
}
object Ptr {
  def apply(seg: Message#Segment, buf: ByteBuf): Ptr = buf.readFirstUInt2(0) match {
    case 0 => StructPtr(seg, buf)
    case 1 => ListPtr(seg, buf)
    case 2 => FarPtr(seg, buf)
  }
}

case class StructPtr(seg: Message#Segment, startWord: Int, dataWords: Int, ptrWords: Int) extends Ptr
object StructPtr {
  def apply(seg: Message#Segment, buf: ByteBuf): StructPtr = StructPtr(
    seg,
    buf.readLastInt30(0),
    buf.readUInt16(32),
    buf.readUInt16(48)
  )
}

sealed trait ListPtr extends Ptr
object ListPtr {
  def apply(seg: Message#Segment, buf: ByteBuf): ListPtr = buf.readFirstUInt3(32) match {
    case 7 => CompListPtr(seg, buf)
    case t => PrimListPtr(seg, t, buf)
  }
}

case class PrimListPtr(seg: Message#Segment, startWord: Int, elemSizeType: Byte, count: Int) extends ListPtr
object PrimListPtr {
  def apply(seg: Message#Segment, buf: ByteBuf): PrimListPtr = apply(seg, buf.readFirstUInt3(32), buf)
  def apply(seg: Message#Segment, elemType: Byte, buf: ByteBuf): PrimListPtr = PrimListPtr(
    seg,
    buf.readLastInt30(0),
    elemType,
    buf.readLastUInt29(32).toInt
  )
}

case class CompListPtr(seg: Message#Segment, startWord: Int, words: Long, tag: StructPtr) extends ListPtr
object CompListPtr {
  def apply(seg: Message#Segment, buf: ByteBuf): CompListPtr = {
    val startWord = buf.readLastInt30(0)
    CompListPtr(
      seg,
      startWord + 1,
      buf.readLastUInt29(32),
      // TODO: Can this be a far ptr that needs to be resolved?
      StructPtr(seg, buf.slice(64 + startWord * 64L))
    )
  }
}

case class FarPtr(seg: Message#Segment, twoWordLanding: Boolean, segOffset: Long, segId: Int) extends Ptr
object FarPtr {
  def apply(seg: Message#Segment, buf: ByteBuf): FarPtr = ???
}