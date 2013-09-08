package org.capnp.model

case class StructBuf(buf: ByteBuf) extends Pointable {

  lazy val dataStart = buf.readInt30(2)
  lazy val dataSize = buf.readUInt16(32) * 64L
  lazy val pointerSize = buf.readUInt16(48) * 64L
  
  lazy val pointables = {
    0L until pointerSize by 64 map(m => Pointable(buf.slice(64 + m)))
  }
}