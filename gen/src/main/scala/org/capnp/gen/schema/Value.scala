package org.capnp.gen.schema

import org.capnp.model._

sealed abstract class Value extends Struct with Union

object Value extends AnonUnionObject[Value] {
  protected val unionTagBitOffset = 0L
  protected val cases = Map(
    0 -> Void,
    1 -> Bool,
    2 -> Int8,
    3 -> Int16,
    4 -> Int32,
    5 -> Int64,
    6 -> UInt8,
    7 -> UInt16,
    8 -> UInt32,
    9 -> UInt64,
    10 -> Float32,
    11 -> Float64,
    12 -> Text,
    13 -> Data,
    14 -> List,
    15 -> Enum,
    16 -> Struct,
    17 -> Interface,
    18 -> Object
  )
  
  case class Void(ptr: StructPtr) extends Value {
    protected val unionTag = 0
  }
  
  object Void extends StructObject[Void](16, 1)
  
  case class Bool(ptr: StructPtr) extends Value {
    protected val unionTag = 1
    
    def value = boolField(16)
    def value_=(v: Boolean) = boolField_=(16, v)
  }
  
  object Bool extends StructObject[Bool](16, 1)
  
  case class Int8(ptr: StructPtr) extends Value {
    protected val unionTag = 2
    
    def value = int8Field(16)
    def value_=(v: Byte) = int8Field_=(16, v)
  }
  
  object Int8 extends StructObject[Int8](16, 1)
  
  case class Int16(ptr: StructPtr) extends Value {
    protected val unionTag = 3
    
    def value = int16Field(16)
    def value_=(v: Short) = int16Field_=(16, v)
  }
  
  object Int16 extends StructObject[Int16](16, 1)
  
  case class Int32(ptr: StructPtr) extends Value {
    protected val unionTag = 4
    
    def value = int32Field(32)
    def value_=(v: Int) = int32Field_=(32, v)
  }
  
  object Int32 extends StructObject[Int32](16, 1)
  
  case class Int64(ptr: StructPtr) extends Value {
    protected val unionTag = 5
    
    def value = int64Field(64)
    def value_=(v: Long) = int64Field_=(64, v)
  }
  
  object Int64 extends StructObject[Int64](16, 1)
  
  case class UInt8(ptr: StructPtr) extends Value {
    protected val unionTag = 6
    
    def value = uint8Field(16)
    def value_=(v: Short) = uint8Field_=(16, v)
  }
  
  object UInt8 extends StructObject[UInt8](16, 1)
  
  case class UInt16(ptr: StructPtr) extends Value {
    protected val unionTag = 7
    
    def value = uint16Field(16)
    def value_=(v: Int) = uint16Field_=(16, v)
  }
  
  object UInt16 extends StructObject[UInt16](16, 1)
  
  case class UInt32(ptr: StructPtr) extends Value {
    protected val unionTag = 8
    
    def value = uint32Field(32)
    def value_=(v: Long) = uint32Field_=(32, v)
  }
  
  object UInt32 extends StructObject[UInt32](16, 1)
  
  case class UInt64(ptr: StructPtr) extends Value {
    protected val unionTag = 9
    
    def value = uint64Field(64)
    def value_=(v: BigInt) = uint64Field_=(64, v)
  }
  
  object UInt64 extends StructObject[UInt64](16, 1)
  
  case class Float32(ptr: StructPtr) extends Value {
    protected val unionTag = 10
    
    def value = float32Field(32)
    def value_=(v: Float) = float32Field_=(32, v)
  }
  
  object Float32 extends StructObject[Float32](16, 1)
  
  case class Float64(ptr: StructPtr) extends Value {
    protected val unionTag = 11
    
    def value = float64Field(64)
    def value_=(v: Double) = float64Field_=(64, v)
  }
  
  object Float64 extends StructObject[Float64](16, 1)
  
  case class Text(ptr: StructPtr) extends Value {
    protected val unionTag = 12
    
    def value = textField(0)
    def value_=(v: String) = textField_=(0, v)
  }
  
  object Text extends StructObject[Text](16, 1)
  
  case class Data(ptr: StructPtr) extends Value {
    protected val unionTag = 13
    
    def value = dataField(0)
    def value_=(v: Array[Byte]) = dataField_=(0, v)
  }
  
  object Data extends StructObject[Data](16, 1)
  
  case class List(ptr: StructPtr) extends Value {
    protected val unionTag = 14
    
    def value = dynField(0)
    def value_=(v: Option[DynObject]) = dynField_=(0, v)
  }
  
  object List extends StructObject[List](16, 1)
  
  case class Enum(ptr: StructPtr) extends Value {
    protected val unionTag = 15
    
    def value = uint16Field(16)
    def value_=(v: Int) = uint16Field_=(16, v)
  }
  
  object Enum extends StructObject[Enum](16, 1)
  
  case class Struct(ptr: StructPtr) extends Value {
    protected val unionTag = 16
    
    def value = dynField(0)
    def value_=(v: Option[DynObject]) = dynField_=(0, v)
  }
  
  object Struct extends StructObject[Struct](16, 1)
  
  case class Interface(ptr: StructPtr) extends Value {
    protected val unionTag = 17
  }
  
  object Interface extends StructObject[Interface](16, 1)
  
  case class Object(ptr: StructPtr) extends Value {
    protected val unionTag = 18
    
    def value = dynField(0)
    def value_=(v: Option[DynObject]) = dynField_=(0, v)
  }
  
  object Object extends StructObject[Object](16, 1)
}