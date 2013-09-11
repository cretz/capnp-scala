package org.capnp.model

import java.io.InputStream
import java.nio.ByteBuffer
import java.nio.ByteOrder

class Message(val segments: Seq[ByteBuf]) {
  lazy val rootBuf = StructBuf(segments.head)
  
  def root[T <: Struct](v: () => T): T = {
    //v.apply(ByteBuffer.allocate(0))
    v()
  }
}
object Message {
  
  def readAll(s: InputStream, packed: Boolean = false): Message = {
    // TODO: very blocking and only one byte at a time :-(
    readAll(
      Iterator continually s.read takeWhile(-1 !=) map(_.toByte) toArray, packed
    )
  }
  
  def readAll(bytes: Array[Byte], packed: Boolean): Message = {
    if (packed) readAll(ByteBuffer.wrap(Packer.unpack(bytes.iterator).toArray))
    else readAll(ByteBuffer.wrap(bytes))
  }
  
  def readAll(buf: ByteBuffer): Message = readAll(new ByteBuf(buf))
  
  // Eager
  def readAll(buf: ByteBuf): Message = {
    val segCount = buf.readUInt32(0) + 1
    val start = (32 + (segCount * 32)) + ((32 + (segCount * 32)) % 64)
    val sizes = 0 until segCount.toInt map(i => buf.readUInt32(32 + (i * 32)))
    new Message(sizes.foldLeft((Seq.empty[ByteBuf], start)) { (m, n) =>
      (m._1 :+ buf.slice(m._2), m._2 + n * 64)
    }._1)
  }
}