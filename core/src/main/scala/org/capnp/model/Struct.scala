package org.capnp.model

import scala.collection.mutable.Seq

abstract class Struct extends Pointable {
  
  val ptr: StructPtr
  
  private val buf = ptr.buf.slice(64L + ptr.startWord * 64L)
  
  private def ptrBuf(idx: Int): ByteBuf =
    buf.slice(idx * 64L + ptr.dataWords * 64L)
  
  private def primSeq[T](ptrOff: Int, elemType: Type.Value): Seq[T] =
    primSeqOption[T](ptrOff, elemType) getOrElse Seq.empty[T]
    
  private def primSeqOption[T](ptrOff: Int, elemType: Type.Value): Option[Seq[T]] =
    ptr.msg.getPrimSeq(ptrBuf(ptrOff), elemType)
    
  protected def newPrimSeq[T](ptrOff: Int, size: Int, elemType: Type.Value): Seq[T] = {
    require(isNullPtr(ptrOff), "Struct already exists")
    ???
  }
  
  protected def withPrimSeq[T](ptrOff: Int, size: Int, assertSize: Boolean, elemType: Type.Value): Seq[T] =
    primSeqOption[T](ptrOff, elemType) match {
      case Some(s) =>
        require(!assertSize || s.size == size, "Expected seq with " + size + " elements, found " + s.size)
        s
      case None => newPrimSeq[T](ptrOff, size, elemType)
    }
  
  private def primSeq_=[T](ptr: Int, elemType: Type.Value, v: Seq[T]): Unit = ???
  
  protected def isNullPtr(ptrOff: Int) = ptr.msg.getPtr(ptrBuf(ptrOff)).isInstanceOf[NullPtr]
  
  protected def boolField(offset: Long): Boolean = buf.readBool(offset)
  protected def boolField_=(offset: Long, v: Boolean) { buf.writeBool(offset, v) }
  protected def boolSeq(ptr: Int): Seq[Boolean] = primSeq[Boolean](ptr, Type.Bool)
  protected def boolSeqOption(ptr: Int): Option[Seq[Boolean]] = primSeqOption[Boolean](ptr, Type.Bool)
  protected def newBoolSeq(ptr: Int, size: Int): Seq[Boolean] = newPrimSeq[Boolean](ptr, size, Type.Bool)
  protected def withBoolSeq(ptr: Int, size: Int, assertSize: Boolean): Seq[Boolean] = withPrimSeq[Boolean](ptr, size, assertSize, Type.Bool)
  
  protected def int8Field(offset: Long): Byte = buf.readInt8(offset)
  protected def int8Field_=(offset: Long, v: Byte) { buf.writeInt8(offset, v) }
  protected def int8Seq(ptr: Int): Seq[Byte] = primSeq[Byte](ptr, Type.Int8)
  protected def int8SeqOption(ptr: Int): Option[Seq[Byte]] = primSeqOption[Byte](ptr, Type.Int8)
  protected def newInt8Seq(ptr: Int, size: Int): Seq[Byte] = newPrimSeq[Byte](ptr, size, Type.Int8)
  protected def withInt8Seq(ptr: Int, size: Int, assertSize: Boolean): Seq[Byte] = withPrimSeq[Byte](ptr, size, assertSize, Type.Int8)
  
  protected def int16Field(offset: Long): Short = buf.readInt16(offset)
  protected def int16Field_=(offset: Long, v: Short) { buf.writeInt16(offset, v) }
  protected def int16Seq(ptr: Int): Seq[Short] = primSeq[Short](ptr, Type.Int16)
  protected def int16SeqOption(ptr: Int): Option[Seq[Short]] = primSeqOption[Short](ptr, Type.Int16)
  protected def newInt16Seq(ptr: Int, size: Int): Seq[Short] = newPrimSeq[Short](ptr, size, Type.Int16)
  protected def withInt16Seq(ptr: Int, size: Int, assertSize: Boolean): Seq[Short] = withPrimSeq[Short](ptr, size, assertSize, Type.Int16)
  
  protected def int32Field(offset: Long): Int = buf.readInt32(offset)
  protected def int32Field_=(offset: Long, v: Int) { buf.writeInt32(offset, v) }
  protected def int32Seq(ptr: Int): Seq[Int] = primSeq[Int](ptr, Type.Int32)
  protected def int32SeqOption(ptr: Int): Option[Seq[Int]] = primSeqOption[Int](ptr, Type.Int32)
  protected def newInt32Seq(ptr: Int, size: Int): Seq[Int] = newPrimSeq[Int](ptr, size, Type.Int32)
  protected def withInt32Seq(ptr: Int, size: Int, assertSize: Boolean): Seq[Int] = withPrimSeq[Int](ptr, size, assertSize, Type.Int32)
  
  protected def int64Field(offset: Long): Long = buf.readInt64(offset)
  protected def int64Field_=(offset: Long, v: Long) { buf.writeInt64(offset, v) }
  protected def int64Seq(ptr: Int): Seq[Long] = primSeq[Long](ptr, Type.Int64)
  protected def int64SeqOption(ptr: Int): Option[Seq[Long]] = primSeqOption[Long](ptr, Type.Int64)
  protected def newInt64Seq(ptr: Int, size: Int): Seq[Long] = newPrimSeq[Long](ptr, size, Type.Int64)
  protected def withInt64Seq(ptr: Int, size: Int, assertSize: Boolean): Seq[Long] = withPrimSeq[Long](ptr, size, assertSize, Type.Int64)
  
  protected def uint8Field(offset: Long): Short = buf.readUInt8(offset)
  protected def uint8Field_=(offset: Long, v: Short) { buf.writeUInt8(offset, v) }
  protected def uint8Seq(ptr: Int): Seq[Short] = primSeq[Short](ptr, Type.UInt8)
  protected def uint8SeqOption(ptr: Int): Option[Seq[Short]] = primSeqOption[Short](ptr, Type.UInt8)
  protected def newUInt8Seq(ptr: Int, size: Int): Seq[Short] = newPrimSeq[Short](ptr, size, Type.UInt8)
  protected def withUInt8Seq(ptr: Int, size: Int, assertSize: Boolean): Seq[Short] = withPrimSeq[Short](ptr, size, assertSize, Type.UInt8)
  
  protected def uint16Field(offset: Long): Int = buf.readUInt16(offset)
  protected def uint16Field_=(offset: Long, v: Int) { buf.writeUInt16(offset, v) }
  protected def uint16Seq(ptr: Int): Seq[Int] = primSeq[Int](ptr, Type.UInt16)
  protected def uint16SeqOption(ptr: Int): Option[Seq[Int]] = primSeqOption[Int](ptr, Type.UInt16)
  protected def newUInt16Seq(ptr: Int, size: Int): Seq[Int] = newPrimSeq[Int](ptr, size, Type.UInt16)
  protected def withUInt16Seq(ptr: Int, size: Int, assertSize: Boolean): Seq[Int] = withPrimSeq[Int](ptr, size, assertSize, Type.UInt16)
  
  protected def uint32Field(offset: Long): Long = buf.readUInt32(offset)
  protected def uint32Field_=(offset: Long, v: Long) { buf.writeUInt32(offset, v) }
  protected def uint32Seq(ptr: Int): Seq[Long] = primSeq[Long](ptr, Type.UInt32)
  protected def uint32SeqOption(ptr: Int): Option[Seq[Long]] = primSeqOption[Long](ptr, Type.UInt32)
  protected def newUInt32Seq(ptr: Int, size: Int): Seq[Long] = newPrimSeq[Long](ptr, size, Type.UInt32)
  protected def withUInt32Seq(ptr: Int, size: Int, assertSize: Boolean): Seq[Long] = withPrimSeq[Long](ptr, size, assertSize, Type.UInt32)
  
  protected def uint64Field(offset: Long): BigInt = buf.readUInt64(offset)
  protected def uint64Field_=(offset: Long, v: BigInt) { buf.writeUInt64(offset, v) }
  protected def uint64Seq(ptr: Int): Seq[BigInt] = primSeq[BigInt](ptr, Type.UInt64)
  protected def uint64SeqOption(ptr: Int): Option[Seq[BigInt]] = primSeqOption[BigInt](ptr, Type.UInt64)
  protected def newUInt64Seq(ptr: Int, size: Int): Seq[BigInt] = newPrimSeq[BigInt](ptr, size, Type.UInt64)
  protected def withUInt64Seq(ptr: Int, size: Int, assertSize: Boolean): Seq[BigInt] = withPrimSeq[BigInt](ptr, size, assertSize, Type.UInt64)
  
  protected def float32Field(offset: Long): Float = buf.readFloat32(offset)
  protected def float32Field_=(offset: Long, v: Float) { buf.writeFloat32(offset, v) }
  protected def float32Seq(ptr: Int): Seq[Float] = primSeq[Float](ptr, Type.Float32)
  protected def float32SeqOption(ptr: Int): Option[Seq[Float]] = primSeqOption[Float](ptr, Type.Float32)
  protected def newFloat32Seq(ptr: Int, size: Int): Seq[Float] = newPrimSeq[Float](ptr, size, Type.Float32)
  protected def withFloat32Seq(ptr: Int, size: Int, assertSize: Boolean): Seq[Float] = withPrimSeq[Float](ptr, size, assertSize, Type.Float32)
  
  protected def float64Field(offset: Long): Double = buf.readFloat64(offset)
  protected def float64Field_=(offset: Long, v: Double) { buf.writeFloat64(offset, v) }
  protected def float64Seq(ptr: Int): Seq[Double] = primSeq[Double](ptr, Type.Float64)
  protected def float64SeqOption(ptr: Int): Option[Seq[Double]] = primSeqOption[Double](ptr, Type.Float64)
  protected def newFloat64Seq(ptr: Int, size: Int): Seq[Double] = newPrimSeq[Double](ptr, size, Type.Float64)
  protected def withFloat64Seq(ptr: Int, size: Int, assertSize: Boolean): Seq[Double] = withPrimSeq[Double](ptr, size, assertSize, Type.Float64)
  
  protected def structField[T <: Struct](ptrOff: Int, bld: StructBuildable[T]): Option[T] =
    ptr.msg.getStruct(ptrBuf(ptrOff), bld)
  
  protected def structField_=[T <: Struct](ptr: Int, v: Option[T]): Unit = ???
  
  protected def groupField[T <: Group](): T = ???
  
  protected def groupField_=[T <: Group](v: T): Unit = ???
  
  protected def unionField[T <: Union](tagOffset: Long, obj: UnionObject[T]): T =
    obj(uint16Field(tagOffset))

  protected def unionField_=[T <: Union](tagOffset: Long, v: T): Unit = ???
  
  protected def textField(ptr: Int): String = textFieldOption(ptr) getOrElse ""
  
  protected def textFieldOption(ptr: Int): Option[String] = int8SeqOption(ptr) map { b =>
    if (b.isEmpty) ""
    else if (b.last != 0) throw new IllegalArgumentException("Last byte is not 0")
    else new String(b.dropRight(1).toArray, "UTF8")
  }

  protected def textField_=(ptr: Int, v: String): Unit = int8SeqOption(ptr) match {
    case Some(_) => throw new IllegalArgumentException("Text field already set")
    case _ =>
      val bytes = v.getBytes("UTF8") :+ 0.toByte
      val seq = newInt8Seq(ptr, bytes.size)
      bytes.zipWithIndex foreach { case (x, i) => seq(i) = x }
  }
  
  protected def structSeq[T <: Struct](ptrOff: Int, bld: StructBuildable[T]): Seq[T] =
    ptr.msg.getCompSeq(ptrBuf(ptrOff), bld) getOrElse Seq.empty[T]
  
  protected def structSeqOption[T <: Struct](ptrOff: Int, bld: StructBuildable[T]): Option[Seq[T]] =
    ptr.msg.getCompSeq(ptrBuf(ptrOff), bld)
    
  protected def newStructSeq[T <: Struct](ptrOff: Int, size: Int, bld: StructBuildable[T]): Seq[T] = {
    require(isNullPtr(ptrOff), "Struct already exists")
    ???
  }
  
  protected def withStructSeq[T <: Struct](ptrOff: Int, size: Int, assertSize: Boolean, bld: StructBuildable[T]): Seq[T] =
    structSeqOption[T](ptrOff, bld) match {
      case Some(s) =>
        require(!assertSize || s.size == size, "Expected seq with " + size + " elements, found " + s.size)
        s
      case None =>
        newStructSeq[T](ptrOff, size, bld)
    }
  
  protected def dataField(ptr: Int): Seq[Byte] = int8Seq(ptr)
  
//  protected def dataField_=(ptr: Int, v: Seq[Byte]): Unit = int8Seq_=(ptr, v)
  
  protected def enumField[T <: Enumeration#Value](offset: Long, v: (Int) => T): T =
    v(buf.readUInt16(offset))

  protected def enumField_=(offset: Long, v: Enumeration#Value): Unit = 
    buf.writeUInt16(offset, v.id)
  
  protected def unionField[T <: Union](offset: Int, obj: UnionObject[T]): T =
    obj(buf.readUInt16(offset))
    
  protected def unionFieldAs[T <: Union](offset: Int, u: () => T): T = {
    val r = u()
    buf.writeUInt16(offset, r.unionTag)
    // TODO: Zero out?
    r
  }

  protected def unionField_=[T <: Union](offset: Int, obj: T): Unit = ???
  
  protected def dynField(ptrOff: Int): Option[DynObject] =
    ptr.msg.getDynObject(ptrBuf(ptrOff))

  protected def dynField_=(ptr: Int, v: Option[DynObject]): Unit = ???
}

abstract class StructBuildable[T <: Struct] {
  def apply(ptr: StructPtr): T
}

abstract class StructObject[T <: Struct](val dataBytes: Int, val pointerWords: Int) extends StructBuildable[T] {
}