package org.capnp.model

case class FarPointer(buf: ByteBuf) extends Pointable {

  lazy val twoWordLanding = buf.readBool(2)
  lazy val segOffset = buf.readUInt29(3) * 64L
  lazy val segIndex = buf.readUInt32(32)
}