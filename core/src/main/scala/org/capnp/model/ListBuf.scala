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
  
  lazy val firstElementOffset = buf.readInt30(2) * 64 + 64
  
}
object ListBuf {
  def apply(buf: ByteBuf): ListBuf = ListSize(buf.readUInt3(32)) match {
    case ListSize.Composite => CompositeListBuf(buf)
    case s => PrimitiveListBuf(buf, s)
  }
}

case class PrimitiveListBuf(buf: ByteBuf, size: ListSize.Value) extends ListBuf {
  lazy val count = buf.readUInt29(35)
}
case class CompositeListBuf(buf: ByteBuf) extends ListBuf {
  lazy val size = buf.readUInt29(35) * 64L
  lazy val pointable = Pointable(buf.slice(64))
//  lazy val count = structBuf.dataStart
}