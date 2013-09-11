package org.capnp.model

import java.nio.ByteBuffer

abstract class Struct(id: BigInt, dataWords: Int, pointerWords: Int) extends Pointable {
  
  private[model] var _buf: Option[ByteBuf] = None
  private[model] def buf = _buf.getOrElse {
    val b = new ByteBuf(dataWords * 64L + pointerWords * 64L)
    _buf = Some(b)
    b
  }
  
  private var pointerContext: Option[PointerContext] = None
  private var pointables = Map.empty[Int, Option[Pointable]]
  
  private def pointable(index: Int) = pointables.getOrElse(index, {
    val p = pointerContext flatMap(_.getPointer(buf.slice(index * 64L + dataWords * 64L, 64)))
    pointables += index -> p
    p
  })
  
  protected def boolField(offset: Long) = buf.readBool(offset)
  protected def boolField_=(offset: Long, v: Boolean) { buf.writeBool(offset, v) }
  
  protected def int8Field(offset: Long): Byte = ???
  protected def int8Field_=(offset: Long, v: Byte): Unit = ???
  
  protected def int16Field(offset: Long): Short = ???
  protected def int16Field_=(offset: Long, v: Short): Unit = ???
  
  protected def int32Field(offset: Long): Int = ???
  protected def int32Field_=(offset: Long, v: Int): Unit = ???
  
  protected def int64Field(offset: Long): Double = ???
  protected def int64Field_=(offset: Long, v: Double): Unit = ???
  
  protected def uint8Field(offset: Long): Short = ???
  protected def uint8Field_=(offset: Long, v: Short): Unit = ???
  
  protected def uint16Field(offset: Long): Int = ???
  protected def uint16Field_=(offset: Long, v: Int): Unit = ???
  
  protected def uint32Field(offset: Long): Long = ???
  protected def uint32Field_=(offset: Long, v: Long): Unit = ???
  
  protected def uint64Field(offset: Long): BigInt = ???
  protected def uint64Field_=(offset: Long, v: BigInt): Unit = ???
  
  protected def float32Field(offset: Long): Float = ???
  protected def float32Field_=(offset: Long, v: Float): Unit = ???
  
  protected def float64Field(offset: Long): Double = ???
  protected def float64Field_=(offset: Long, v: Double): Unit = ???
  
  protected def ptrField[T](ptr: Int): T = ???
  protected def ptrField_=[T](ptr: Int, v: T): Unit = ???
  
  protected def groupField[T <: Group](): T = ???
  protected def groupField_=[T <: Group](v: T): Unit = ???
  
  protected def textField(ptr: Int): Option[String] = pointable(ptr) map {
    case p: PrimitiveListBuf if p.size == ListSize.Byte => p.asString
    case _ => ???
  }
  protected def textField_=(ptr: Int, v: Option[String]): Unit =
    pointables += ptr -> v.map(PrimitiveListBuf(_))
  
  protected def seqField[T](ptr: Int): Seq[T] = ???
  protected def seqField_=[T](ptr: Int, v: Seq[T]): Unit = ???
  
  protected def dataField(ptr: Int): Array[Byte] = ???
  protected def dataField_=(ptr: Int, v: Array[Byte]): Unit = ???
  
  protected def enumField[T <: Enumeration#Value](offset: Long, v: (Int) => T): T = {
    v(buf.readUInt16(offset))
  }
  protected def enumField_=(offset: Long, v: Enumeration#Value): Unit = 
    buf.writeUInt16(offset, v.id)
}