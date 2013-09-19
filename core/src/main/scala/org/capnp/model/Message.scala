package org.capnp.model

import java.io.InputStream
import java.nio.ByteBuffer
import java.nio.ReadOnlyBufferException

// TODO: build streamable
sealed trait Message {
  def segments: Seq[ByteBuf]
  
  // This follows far pointers
  def getPtr(ptrBuf: ByteBuf): Ptr = Ptr(this, ptrBuf) match {
    case p: FarPtr => getPtr(segments(p.segId).slice(p.segOffsetWords * 64L))
    case p => p
  }
  
  def getDynObject(ptrBuf: ByteBuf): Option[DynObject] = getPtr(ptrBuf) match {
    case _: NullPtr => None
    case p => Some(DynObject(p))
  }
  
  def addDynObject(ptrBuf: ByteBuf): DynObject = throw new ReadOnlyBufferException

  def getStruct[T <: Struct](ptrBuf: ByteBuf, bld: StructBuildable[T]): Option[T] = getPtr(ptrBuf) match {
    case p: StructPtr => Some(bld(p))
    case _: NullPtr => None
    case _ => throw new Exception("Invalid struct pointer")
  }
  
  def addStruct[T <: Struct](ptrBuf: ByteBuf, obj: StructObject[T]): T = throw new ReadOnlyBufferException
  
  def getPrimSeq[T](ptrBuf: ByteBuf, elemType: Type.Value): Option[PrimitiveSeq[T]] = getPtr(ptrBuf) match {
    case p: PrimListPtr => Some(getPrimSeqFromPtr[T](p, elemType))
    case _: NullPtr => None
    case _ => throw new Exception("Invalid list pointer")
  }
  
  def getPrimSeqFromPtr[T](p: PrimListPtr, elemType: Type.Value): PrimitiveSeq[T] =
    new PrimitiveSeq[T](
        this,
        p.buf.slice(64L + p.startWord * 64L),
        p.count,
        p.elemSizeType,
        elemType)
  
  def addPrimSeq[T](ptrBuf: ByteBuf, size: Int, elemType: Type.Value): PrimitiveSeq[T] = throw new ReadOnlyBufferException
  
  def getCompSeq[T <: Struct](ptrBuf: ByteBuf, bld: StructBuildable[T]): Option[CompositeSeq[T]] = getPtr(ptrBuf) match {
    case p: CompListPtr => Some(getCompSeqFromPtr[T](p, bld))
    case _: NullPtr => None
    case _ => throw new Exception("Invalid list pointer")
  }
  
  def getCompSeqFromPtr[T <: Struct](p: CompListPtr, bld: StructBuildable[T]): CompositeSeq[T] =
    new CompositeSeq[T](
        this,
        p.buf.slice(64L + p.startWord * 64L),
        p.tag.startWord,
        p.tag.dataWords,
        p.tag.ptrWords,
        bld)
  
  def addCompSeq[T <: Struct](ptrBuf: ByteBuf, size: Int, obj: StructObject[T]): CompositeSeq[T] = throw new ReadOnlyBufferException
}

object Message {
  
  def readAll(s: InputStream, packed: Boolean = false): ReadOnlyMessage =
    // TODO: very blocking and only one byte at a time :-(
    readAll(
      (Iterator continually(s.read) takeWhile(_ != -1) map(_.toByte)).toArray, packed
    )
  
  def readAll(bytes: Array[Byte], packed: Boolean): ReadOnlyMessage =
    if (packed) readAll(ByteBuffer.wrap(Packer.unpack(bytes.iterator).toArray))
    else readAll(ByteBuffer.wrap(bytes))
  
  def readAll(buf: ByteBuffer): ReadOnlyMessage = readAll(new ByteBufImpl(buf.asReadOnlyBuffer()))
  
  // Eager
  def readAll(buf: ByteBuf): ReadOnlyMessage = {
    val segCount = buf.readUInt32(0) + 1
    val start = (32 + (segCount * 32)) + ((32 + (segCount * 32)) % 64)
    val sizes = 0 until segCount.toInt map(i => buf.readUInt32(32 + (i * 32)))
    ReadOnlyMessage(sizes.foldLeft((Seq.empty[ByteBuf], start)) { (m, n) =>
      (m._1 :+ buf.slice(m._2), m._2 + n * 64)
    }._1)
  }
  
  def forWrite[T <: Struct](obj: StructObject[T], preferredMaxWordsPerSegment: Int = 1024): MutableMessage[T] =
    MutableMessage(preferredMaxWordsPerSegment, obj)
}

case class ReadOnlyMessage(segments: Seq[ByteBuf]) extends Message {
  def root[T <: Struct](bld: StructBuildable[T]): Option[T] = getStruct(segments.head, bld)
}

case class MutableMessage[R <: Struct](preferredMaxWordsPerSegment: Int, private val rootObj: StructObject[R]) extends Message {
  var segments: Seq[ByteBuf] = Seq(new ByteBufImpl(preferredMaxWordsPerSegment))
  
  val root: R = addStruct(segments.head, rootObj)
  
  // returns start word
  def reserveContinuousSpace(ptrBuf: ByteBuf, words: Int): (Int, ByteBuf) = {
    // TODO: far pointers when necessary...
    ???
  }
  
  override def addStruct[T <: Struct](ptrBuf: ByteBuf, obj: StructObject[T]): T = {
    val dataWords = (obj.dataBytes / 8) + (obj.dataBytes % 8)
    val (startWord, reservedBuf) = reserveContinuousSpace(ptrBuf, dataWords + obj.pointerWords)
    val ptr = StructPtr(this, reservedBuf, startWord, dataWords, obj.pointerWords)
    ptr.write
    obj(ptr)
  }
  
  override def addPrimSeq[T](ptrBuf: ByteBuf, size: Int, elemType: Type.Value): PrimitiveSeq[T] = {
    val dataBytes = ((size.toLong * Type.bitSize(elemType)) / 8L).toInt
    val dataWords = (dataBytes / 8) + (dataBytes % 8)
    val (startWord, reservedBuf) = reserveContinuousSpace(ptrBuf, dataWords)
    val ptr = PrimListPtr(this, reservedBuf, startWord, Type.primitiveListElementType(elemType), size)
    ptr.write()
    getPrimSeqFromPtr[T](ptr, elemType)
  }
  
  override def addCompSeq[T <: Struct](ptrBuf: ByteBuf, size: Int, obj: StructObject[T]): CompositeSeq[T] = {
    val dataWords = (obj.dataBytes / 8) + (obj.dataBytes % 8)
    // Reserve enough for all the structs + a tag
    val (startWord, reservedBuf) = reserveContinuousSpace(ptrBuf, dataWords + obj.pointerWords + 1)
    // Build the regular struct tag ptr
    val tagPtr = StructPtr(this, reservedBuf.slice(startWord * 64L), 0, dataWords, obj.pointerWords)
    tagPtr.write()
    // Now the regular comp list ptr
    val ptr = CompListPtr(this, tagPtr.buf, startWord + 1, size * (dataWords + obj.pointerWords), tagPtr)
    ptr.write()
    getCompSeqFromPtr(ptr, obj)
  }
}