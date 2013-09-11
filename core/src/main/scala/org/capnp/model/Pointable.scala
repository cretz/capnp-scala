package org.capnp.model

trait Pointable {

}
object Pointable {
  def apply(buf: ByteBuf): Pointable = {
    buf.readFirstUInt2(0) match {
    case 0 => StructBuf(buf)
    case 1 => ListBuf(buf)
    case 2 => FarPointer(buf)
    case _ => ???
  } }
}