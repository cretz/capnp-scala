package org.capnp.model

trait PointerContext {
  def getPointer(buf: ByteBuf): Option[Pointable] = ???
}