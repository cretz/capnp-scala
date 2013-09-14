package org.capnp.model

import java.io.InputStream
import java.nio.ByteBuffer
import java.nio.ByteOrder

// TODO: build streamable
case class Message(segments: Seq[ByteBuf]) {
  lazy val rootBuf = StructBuf(segments.head)
  
  def root[T <: Struct](obj: StructObject[T]): Option[T] = getStruct(segments.head, obj)
  
  // This follows far pointers
  def getPtr(ptrBuf: ByteBuf): Ptr = Ptr(ptrBuf) match {
    case p: FarPtr => getPtr(segments(p.segId).slice(p.segOffsetWords * 64L))
    case p => p
  }

  def getStruct[T <: Struct](ptrBuf: ByteBuf, obj: StructObject[T]): Option[T] = getPtr(ptrBuf) match {
    case p: StructPtr => Some(obj(Some(this), p))
    case _: NullPtr => None
    case _ => throw new Exception("Invalid struct pointer")
  }
  
  def getPrimSeq[T](ptrBuf: ByteBuf, elemType: Type.Value): PointableSeq[T] = getPtr(ptrBuf) match {
    case p: PrimListPtr =>
      new PrimitiveSeq[T](
        this,
        p.buf.slice(64L + p.startWord * 64L),
        p.count,
        p.elemSizeType,
        elemType
      )
    case _: NullPtr => PointableSeq.empty[T]
    case _ => throw new Exception("Invalid list pointer")
  }
  
  def getCompSeq[T <: Struct](ptrBuf: ByteBuf, obj: StructObject[T]): PointableSeq[T] = getPtr(ptrBuf) match {
    case p: CompListPtr =>
      new CompositeSeq[T](
        this,
        p.buf.slice(64L + p.startWord * 64L),
        p.tag.startWord,
        p.tag.dataWords,
        p.tag.ptrWords,
        obj
      )
    case _: NullPtr => PointableSeq.empty[T]
    case _ => throw new Exception("Invalid list pointer")
  }
}
object Message {
  
  def readAll(s: InputStream, packed: Boolean = false): Message = {
    // TODO: very blocking and only one byte at a time :-(
    readAll(
      (Iterator continually(s.read) takeWhile(_ != -1) map(_.toByte)).toArray, packed
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
    Message(sizes.foldLeft((Seq.empty[ByteBuf], start)) { (m, n) =>
      (m._1 :+ buf.slice(m._2), m._2 + n * 64)
    }._1)
  }
}