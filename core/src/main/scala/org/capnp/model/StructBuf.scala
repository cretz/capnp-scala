package org.capnp.model

case class StructBuf(dataStart: Long, dataSize: Long, pointerSize: Long, buf: ByteBuf) extends Pointable {
  lazy val pointables = {
    0L until pointerSize by 64 map(m => Pointable(buf.slice(dataStart + dataSize + m)))
  }
}
object StructBuf {
  def apply(buf: ByteBuf): StructBuf = {
    apply(buf.readLastInt30(0) * 64L, buf)
  }
  
  def apply(dataStart: Long, buf: ByteBuf): StructBuf = {
    StructBuf(
      dataStart,
      buf.readUInt16(32) * 64L,
      buf.readUInt16(48) * 64L,
      buf.slice(64)
    )
  }
}