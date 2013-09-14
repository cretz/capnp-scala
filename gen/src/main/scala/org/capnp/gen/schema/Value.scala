package org.capnp.gen.schema

import org.capnp.model._

sealed abstract class Value extends Struct(0xce23dcd2d7b00c9bL, 16, 1) with Union

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
  
  case class Void() extends Value {
    protected val unionTag = 0
  }
  
  case class Bool() extends Value {
    protected val unionTag = 1
    
    def value = boolField(16)
    def value_=(v: Boolean) = boolField_=(16, v)
  }
  
  case class Int8() extends Value {
    protected val unionTag = 2
    
    def value = int8Field(16)
    def value_=(v: Byte) = int8Field_=(16, v)
  }
  
  case class Int16() extends Value {
    protected val unionTag = 3
    
    def value = int16Field(16)
    def value_=(v: Short) = int16Field_=(16, v)
  }
  
  case class Int32() extends Value {
    protected val unionTag = 4
    
    def value = int32Field(32)
    def value_=(v: Int) = int32Field_=(32, v)
  }
  
  case class Int64() extends Value {
    protected val unionTag = 5
    
    def value = int64Field(64)
    def value_=(v: Long) = int64Field_=(64, v)
  }
  
  case class UInt8() extends Value {
    protected val unionTag = 6
    
    def value = uint8Field(16)
    def value_=(v: Short) = uint8Field_=(16, v)
  }
  
  case class UInt16() extends Value {
    protected val unionTag = 7
    
    def value = uint16Field(16)
    def value_=(v: Int) = uint16Field_=(16, v)
  }
  
  case class UInt32() extends Value {
    protected val unionTag = 8
    
    def value = uint32Field(32)
    def value_=(v: Long) = uint32Field_=(32, v)
  }
  
  case class UInt64() extends Value {
    protected val unionTag = 9
    
    def value = uint64Field(64)
    def value_=(v: BigInt) = uint64Field_=(64, v)
  }
  
  case class Float32() extends Value {
    protected val unionTag = 10
    
    def value = float32Field(32)
    def value_=(v: Float) = float32Field_=(32, v)
  }
  
  case class Float64() extends Value {
    protected val unionTag = 11
    
    def value = float64Field(64)
    def value_=(v: Double) = float64Field_=(64, v)
  }
  
  case class Text() extends Value {
    protected val unionTag = 12
    
    def value = textField(0)
    def value_=(v: String) = textField_=(0, v)
  }
  
  case class Data() extends Value {
    protected val unionTag = 13
    
    def value = dataField(0)
    def value_=(v: Array[Byte]) = dataField_=(0, v)
  }
  
  case class List() extends Value {
    protected val unionTag = 14
    
    def value = dynField(0)
    def value_=(v: Option[DynObject]) = dynField_=(0, v)
  }
  
  case class Enum() extends Value {
    protected val unionTag = 15
    
    def value = uint16Field(16)
    def value_=(v: Int) = uint16Field_=(16, v)
  }
  
  case class Struct() extends Value {
    protected val unionTag = 16
    
    def value = dynField(0)
    def value_=(v: Option[DynObject]) = dynField_=(0, v)
  }
  
  case class Interface() extends Value {
    protected val unionTag = 17
  }
  
  case class Object() extends Value {
    protected val unionTag = 18
    
    def value = dynField(0)
    def value_=(v: Option[DynObject]) = dynField_=(0, v)
  }
}