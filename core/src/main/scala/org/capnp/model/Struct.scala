package org.capnp.model

abstract class Struct(id: BigInt) {
  
  protected def boolField(offset: Int): Boolean = ???
  protected def boolField_=(offset: Int, v: Boolean): Unit = ???
  
  protected def int8Field(offset: Int): Byte = ???
  protected def int8Field_=(offset: Int, v: Byte): Unit = ???
  
  protected def int16Field(offset: Int): Short = ???
  protected def int16Field_=(offset: Int, v: Short): Unit = ???
  
  protected def int32Field(offset: Int): Int = ???
  protected def int32Field_=(offset: Int, v: Int): Unit = ???
  
  protected def int64Field(offset: Int): Double = ???
  protected def int64Field_=(offset: Int, v: Double): Unit = ???
  
  protected def uint8Field(offset: Int): Short = ???
  protected def uint8Field_=(offset: Int, v: Short): Unit = ???
  
  protected def uint16Field(offset: Int): Int = ???
  protected def uint16Field_=(offset: Int, v: Int): Unit = ???
  
  protected def uint32Field(offset: Int): Long = ???
  protected def uint32Field_=(offset: Int, v: Long): Unit = ???
  
  protected def uint64Field(offset: Int): BigInt = ???
  protected def uint64Field_=(offset: Int, v: BigInt): Unit = ???
  
  protected def float32Field(offset: Int): Float = ???
  protected def float32Field_=(offset: Int, v: Float): Unit = ???
  
  protected def float64Field(offset: Int): Double = ???
  protected def float64Field_=(offset: Int, v: Double): Unit = ???
  
  protected def ptrField[T](ptr: Int): T = ???
  protected def ptrField_=[T](ptr: Int, v: T): Unit = ???
  
  protected def groupField[T <: Group](): T = ???
  protected def groupField_=[T <: Group](v: T): Unit = ???
  
  protected def textField(ptr: Int): String = ???
  protected def textField_=(ptr: Int, v: String): Unit = ???
  
  protected def seqField[T](ptr: Int): Seq[T] = ???
  protected def seqField_=[T](ptr: Int, v: Seq[T]): Unit = ???
  
  protected def dataField(ptr: Int): Array[Byte] = ???
  protected def dataField_=(ptr: Int, v: Array[Byte]): Unit = ???
  
  protected def enumField[T <: Enumeration#Value](offset: Int): T = ???
  protected def enumField_=[T <: Enumeration#Value](offset: Int, v: T): Unit = ???
}