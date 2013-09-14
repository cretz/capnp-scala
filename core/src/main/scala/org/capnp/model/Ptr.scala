package org.capnp.model

sealed trait Ptr {
  val buf: ByteBuf
}
object Ptr {
  def apply(buf: ByteBuf): Ptr = buf.readFirstUInt2(0) match {
    case 0 => StructPtr(buf) match {
      case StructPtr(_, 0, 0, 0) => NullPtr(buf)
      case s => s
    }
    case 1 => ListPtr(buf)
    case 2 => FarPtr(buf)
  }
}

case class NullPtr(buf: ByteBuf) extends Ptr

case class StructPtr(buf: ByteBuf, startWord: Int, dataWords: Int, ptrWords: Int) extends Ptr
object StructPtr {
  def apply(buf: ByteBuf): StructPtr = StructPtr(
    buf,
    buf.readLastInt30(0),
    buf.readUInt16(32),
    buf.readUInt16(48)
  )
}

sealed trait ListPtr extends Ptr
object ListPtr {
  def apply(buf: ByteBuf): ListPtr = buf.readFirstUInt3(32) match {
    case 7 => CompListPtr(buf)
    case t => PrimListPtr(t, buf)
  }
}

case class PrimListPtr(buf: ByteBuf, startWord: Int, elemSizeType: Byte, count: Int) extends ListPtr
object PrimListPtr {
  def apply(buf: ByteBuf): PrimListPtr = apply(buf.readFirstUInt3(32), buf)
  def apply(elemType: Byte, buf: ByteBuf): PrimListPtr = PrimListPtr(
    buf,
    buf.readLastInt30(0),
    elemType,
    buf.readLastUInt29(32).toInt
  )
}

case class CompListPtr(buf: ByteBuf, startWord: Int, words: Long, tag: StructPtr) extends ListPtr
object CompListPtr {
  def apply(buf: ByteBuf): CompListPtr = {
    val startWord = buf.readLastInt30(0)
    CompListPtr(
      buf,
      startWord + 1,
      buf.readLastUInt29(32),
      // TODO: Can this be a far ptr that needs to be resolved?
      StructPtr(buf.slice(64 + startWord * 64L))
    )
  }
}

case class FarPtr(buf: ByteBuf, twoWordLanding: Boolean, segOffsetWords: Long, segId: Int) extends Ptr
object FarPtr {
  def apply(buf: ByteBuf): FarPtr = {
    FarPtr(
      buf,
      buf.readBool(2),
      buf.readLastUInt29(0),
      buf.readUInt32(32).toInt
    )
  }
}