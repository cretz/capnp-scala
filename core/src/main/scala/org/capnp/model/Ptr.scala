package org.capnp.model

sealed trait Ptr {
  val msg: Message
  val buf: ByteBuf
  
  def write(ptrBuf: ByteBuf): this.type
}

object Ptr {
  def apply(msg: Message, buf: ByteBuf): Ptr = buf.readFirstUInt2(0) match {
    case 0 => StructPtr(msg, buf) match {
      case StructPtr(_, _, 0, 0, 0) => NullPtr(msg, buf)
      case s => s
    }
    case 1 => ListPtr(msg, buf)
    case 2 => FarPtr(msg, buf)
  }
}

case class NullPtr(msg: Message, buf: ByteBuf) extends Ptr {
  def write(ptrBuf: ByteBuf): this.type = { ptrBuf.writeUInt64(0, 0); this }
}

case class StructPtr(msg: Message, buf: ByteBuf, startWord: Int, dataWords: Int, ptrWords: Int)
    extends Ptr {
  def write(ptrBuf: ByteBuf): this.type = {
    ptrBuf.writeFirstUInt2(0, 0).
      writeLastInt30(0, startWord).
      writeUInt16(32, dataWords).
      writeUInt16(48, ptrWords)
    this
  }
}

object StructPtr {
  def apply(msg: Message, buf: ByteBuf): StructPtr = StructPtr(
    msg,
    buf,
    buf.readLastInt30(0),
    buf.readUInt16(32),
    buf.readUInt16(48))
}

sealed trait ListPtr extends Ptr

object ListPtr {
  def apply(msg: Message, buf: ByteBuf): ListPtr = buf.readFirstUInt3(32) match {
    case 7 => CompListPtr(msg, buf)
    case t => PrimListPtr(msg, t, buf)
  }
}

case class PrimListPtr(msg: Message, buf: ByteBuf, startWord: Int, elemSizeType: Byte, count: Int)
    extends ListPtr {
  def write(ptrBuf: ByteBuf): this.type = {
    ptrBuf.writeFirstUInt2(0, 0).
      writeLastInt30(0, startWord).
      writeFirstUInt3(32, elemSizeType).
      writeUInt16(48, count)
    this
  }
}

object PrimListPtr {
  def apply(msg: Message, buf: ByteBuf): PrimListPtr = apply(msg, buf.readFirstUInt3(32), buf)

  def apply(msg: Message, elemType: Byte, buf: ByteBuf): PrimListPtr = PrimListPtr(
    msg,
    buf,
    buf.readLastInt30(0),
    elemType,
    buf.readLastUInt29(32).toInt)
}

case class CompListPtr(msg: Message, buf: ByteBuf, startWord: Int, words: Long, tag: StructPtr)
    extends ListPtr {
  // Does not write the tag
  def write(ptrBuf: ByteBuf): this.type = {
    ptrBuf.writeFirstUInt2(0, 0).
      writeLastInt30(0, startWord - 1).
      writeFirstUInt3(32, 7).
      writeLastUInt29(32, words)
    this
  }
}

object CompListPtr {
  def apply(msg: Message, buf: ByteBuf): CompListPtr = {
    val startWord = buf.readLastInt30(0)
    CompListPtr(
      msg,
      buf,
      startWord + 1,
      buf.readLastUInt29(32),
      // TODO: Can this be a far ptr that needs to be resolved?
      StructPtr(msg, buf.slice(64 + startWord * 64L)))
  }
}

case class FarPtr(msg: Message, buf: ByteBuf, twoWordLanding: Boolean, segOffsetWords: Long, segId: Int)
    extends Ptr {
  def write(ptrBuf: ByteBuf): this.type = {
    ptrBuf.writeFirstUInt2(0, 2).
      writeBool(2, twoWordLanding).
      writeLastUInt29(0, segOffsetWords).
      writeUInt32(32, segId)
    this
  }
}

object FarPtr {
  def apply(msg: Message, buf: ByteBuf): FarPtr = {
    FarPtr(
      msg,
      buf,
      buf.readBool(2),
      buf.readLastUInt29(0),
      buf.readUInt32(32).toInt)
  }
}