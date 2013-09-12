package org.capnp.model

object ListSize extends Enumeration {
  val Void = Value(0)
  val Bit = Value(1)
  val Byte = Value(2)
  val TwoByte = Value(3)
  val FourByte = Value(4)
  val Word = Value(5)
  val Pointer = Value(6)
  val Composite = Value(7)
}

trait ListBuf extends Pointable {
  val buf: ByteBuf
  
  lazy val firstElementOffset = buf.readLastInt30(0) * 64L + 64
}
object ListBuf {
  def apply(buf: ByteBuf): ListBuf = ListSize(buf.readFirstUInt3(32)) match {
    case ListSize.Composite => CompositeListBuf(buf)
    case s => PrimitiveListBuf(buf, s)
  }
}

case class PrimitiveListBuf(buf: ByteBuf, size: ListSize.Value) extends ListBuf {
  lazy val count = buf.readLastUInt29(32)
  
  def asString() = {
    require(size == ListSize.Byte)
    // Get all bytes
    val bytes = 0L until count map(i => buf.readInt8(firstElementOffset + i * 8))
    require(bytes.last == 0)
    new String(bytes.dropRight(1).toArray, "UTF-8")
  }
}
object PrimitiveListBuf {
  def apply(str: String): PrimitiveListBuf = {
    ???
  }
}

case class CompositeListBuf(buf: ByteBuf) extends ListBuf {
  lazy val size = buf.readLastUInt29(32) * 64L
  lazy val tag = StructBuf(buf.slice(firstElementOffset, 64))
  lazy val count = tag.dataStart / 64
  
  def get(i: Int) = {
    StructBuf(0, tag.dataSize, tag.pointerSize, buf.slice(firstElementOffset + 64 + i * (tag.dataSize + tag.pointerSize)))
  }
}