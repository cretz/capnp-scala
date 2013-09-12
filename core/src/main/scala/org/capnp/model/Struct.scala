package org.capnp.model

import java.nio.ByteBuffer

abstract class Struct(id: BigInt, dataWords: Int, pointerWords: Int) extends Pointable {
  
  private[model] var _buf: Option[ByteBuf] = None
  private[model] def buf_=(v: Option[ByteBuf]) { _buf = v }
  private[model] def buf = _buf.getOrElse {
    val b = new ByteBuf(dataWords * 64L + pointerWords * 64L)
    _buf = Some(b)
    b
  }
  
  private[model] var seg: Option[Message#Segment] = None
  private var pointables = Map.empty[Int, Option[Pointable]]
  
//  private def pointable(index: Int) = pointables.getOrElse(index, {
//    val p = seg map(_.getPtr(buf.slice(index * 64L + dataWords * 64L, 64)))
//    pointables += index -> p
//    p
//  })
  private def ptrBuf(idx: Int) = {
    buf.slice(idx * 64L + dataWords * 64L)
  }
  
  private def primSeq[T](ptr: Int, elemType: Type.Value): Seq[T] =
    pointables.getOrElse(ptr, {
      val s = seg.map(_.getPrimSeq(ptrBuf(ptr), elemType))
      pointables += ptr -> s
      Some(s.getOrElse(Seq.empty[T]))
    }).get.asInstanceOf[Seq[T]]
  private def primSeq_=[T](ptr: Int, elemType: Type.Value, v: Seq[T]) {
    pointables += ptr -> Some(PointableSeq.fromSeq(v))
  }
  
  protected def boolField(offset: Long): Boolean = buf.readBool(offset)
  protected def boolField_=(offset: Long, v: Boolean) { buf.writeBool(offset, v) }
  protected def boolSeq(ptr: Int): Seq[Boolean] = primSeq[Boolean](ptr, Type.Bool)
  protected def boolSeq_=(ptr: Int, v: Seq[Boolean]) { primSeq_=(ptr, Type.Bool, v) }
  
  protected def int8Field(offset: Long): Byte = buf.readInt8(offset)
  protected def int8Field_=(offset: Long, v: Byte) { buf.writeInt8(offset, v) }
  protected def int8Seq(ptr: Int): Seq[Byte] = primSeq[Byte](ptr, Type.Int8)
  protected def int8Seq_=(ptr: Int, v: Seq[Byte]) { primSeq_=(ptr, Type.Int8, v) }
  
  protected def int16Field(offset: Long): Short = buf.readInt16(offset)
  protected def int16Field_=(offset: Long, v: Short) { buf.writeInt16(offset, v) }
  protected def int16Seq(ptr: Int): Seq[Short] = primSeq[Short](ptr, Type.Int16)
  protected def int16Seq_=(ptr: Int, v: Seq[Short]) { primSeq_=(ptr, Type.Int16, v) }
  
  protected def int32Field(offset: Long): Int = buf.readInt32(offset)
  protected def int32Field_=(offset: Long, v: Int) { buf.writeInt32(offset, v) }
  protected def int32Seq(ptr: Int): Seq[Int] = primSeq[Int](ptr, Type.Int32)
  protected def int32Seq_=(ptr: Int, v: Seq[Int]) { primSeq_=(ptr, Type.Int32, v) }
  
  protected def int64Field(offset: Long): Long = buf.readInt64(offset)
  protected def int64Field_=(offset: Long, v: Long) { buf.writeInt64(offset, v) }
  protected def int64Seq(ptr: Int): Seq[Long] = primSeq[Long](ptr, Type.Int64)
  protected def int64Seq_=(ptr: Int, v: Seq[Long]) { primSeq_=(ptr, Type.Int64, v) }
  
  protected def uint8Field(offset: Long): Short = buf.readUInt8(offset)
  protected def uint8Field_=(offset: Long, v: Short) { buf.writeUInt8(offset, v) }
  protected def uint8Seq(ptr: Int): Seq[Short] = primSeq[Short](ptr, Type.UInt8)
  protected def uint8Seq_=(ptr: Int, v: Seq[Short]) { primSeq_=(ptr, Type.UInt8, v) }
  
  protected def uint16Field(offset: Long): Int = buf.readUInt16(offset)
  protected def uint16Field_=(offset: Long, v: Int) { buf.writeUInt16(offset, v) }
  protected def uint16Seq(ptr: Int): Seq[Int] = primSeq[Int](ptr, Type.UInt16)
  protected def uint16Seq_=(ptr: Int, v: Seq[Int]) { primSeq_=(ptr, Type.UInt16, v) }
  
  protected def uint32Field(offset: Long): Long = buf.readUInt32(offset)
  protected def uint32Field_=(offset: Long, v: Long) { buf.writeUInt32(offset, v) }
  protected def uint32Seq(ptr: Int): Seq[Long] = primSeq[Long](ptr, Type.UInt32)
  protected def uint32Seq_=(ptr: Int, v: Seq[Long]) { primSeq_=(ptr, Type.UInt32, v) }
  
  protected def uint64Field(offset: Long): BigInt = buf.readUInt64(offset)
  protected def uint64Field_=(offset: Long, v: BigInt) { buf.writeUInt64(offset, v) }
  protected def uint64Seq(ptr: Int): Seq[BigInt] = primSeq[BigInt](ptr, Type.UInt64)
  protected def uint64Seq_=(ptr: Int, v: Seq[BigInt]) { primSeq_=(ptr, Type.UInt64, v) }
  
  protected def float32Field(offset: Long): Float = buf.readFloat32(offset)
  protected def float32Field_=(offset: Long, v: Float) { buf.writeFloat32(offset, v) }
  protected def float32Seq(ptr: Int): Seq[Float] = primSeq[Float](ptr, Type.Float32)
  protected def float32Seq_=(ptr: Int, v: Seq[Float]) { primSeq_=(ptr, Type.Float32, v) }
  
  protected def float64Field(offset: Long): Double = buf.readFloat64(offset)
  protected def float64Field_=(offset: Long, v: Double) { buf.writeFloat64(offset, v) }
  protected def float64Seq(ptr: Int): Seq[Double] = primSeq[Double](ptr, Type.Float64)
  protected def float64Seq_=(ptr: Int, v: Seq[Double]) { primSeq_=(ptr, Type.Float64, v) }
  
  protected def ptrField[T](ptr: Int): T = ???
  protected def ptrField_=[T](ptr: Int, v: T): Unit = ???
  
  protected def groupField[T <: Group](): T = ???
  protected def groupField_=[T <: Group](v: T): Unit = ???
  
  protected def textField(ptr: Int): Option[String] = ???/*pointable(ptr) map {
    case p: PrimitiveListBuf if p.size == ListSize.Byte => p.asString
    case _ => ???
  }*/
  protected def textField_=(ptr: Int, v: Option[String]): Unit =
    pointables += ptr -> v.map(TextPointable(_))
  
  protected def structSeq[T <: Struct](ptr: Int, obj: StructObject[T]): Seq[T] =
    pointables.getOrElse(ptr, {
      val s = seg.map(_.getCompSeq(ptrBuf(ptr), obj))
      pointables += ptr -> s
      Some(s.getOrElse(Seq.empty[T]))
    }).get.asInstanceOf[Seq[T]]
    
  protected def structSeq_=[T <: Struct](ptr: Int, v: Seq[T]): Unit = {
    pointables += ptr -> (if (v.isEmpty) None else Some(PointableSeq.fromSeq(v)))
  }
  
  protected def dataField(ptr: Int): Array[Byte] = ???
  protected def dataField_=(ptr: Int, v: Array[Byte]): Unit = ???
  
  protected def enumField[T <: Enumeration#Value](offset: Long, v: (Int) => T): T = {
    v(buf.readUInt16(offset))
  }
  protected def enumField_=(offset: Long, v: Enumeration#Value): Unit = 
    buf.writeUInt16(offset, v.id)
}
trait StructObject[T <: Struct] {
  def apply(): T
}