package org.capnp.model

trait Pointable {

}
object Pointable {
  def apply(buf: ByteBuf): Pointable = buf.readUInt2(0) match {
    case 0 => StructBuf(buf)
    case 1 => ListBuf(buf)
  }
}