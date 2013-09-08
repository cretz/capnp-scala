package org.capnp.gen.schema

import org.capnp.model
import org.capnp.model._

sealed abstract class Value extends Struct(0xce23dcd2d7b00c9bL)
object Value {
  case class Void extends Value with Union {
    protected val tag = 0
  }
  
  case class Bool extends Value with Union {
    protected val tag = 1
    
    def value = boolField(16)
    def value_=(v: Boolean) = boolField_=(16, v)
  }
  
  case class Int8 extends Value with Union {
    protected val tag = 2
    
    def value = int8Field(16)
    def value_=(v: Byte) = int8Field_=(16, v)
  }
  
  case class Int16 extends Value with Union {
    protected val tag = 3
    
    def value = int16Field(16)
    def value_=(v: Short) = int16Field_=(16, v)
  }
  
  case class Int32 extends Value with Union {
    protected val tag = 4
    
    def value = int32Field(32)
    def value_=(v: Int) = int32Field_=(32, v)
  }
  
  case class Int64 extends Value with Union {
    protected val tag = 5
    
    def value = int64Field(64)
    def value_=(v: Long) = int64Field_=(64, v)
  }
  
  case class UInt8 extends Value with Union {
    protected val tag = 6
    
    def value = uint8Field(16)
    def value_=(v: Short) = uint8Field_=(16, v)
  }
  
  case class UInt16 extends Value with Union {
    protected val tag = 7
    
    def value = uint16Field(16)
    def value_=(v: Int) = uint16Field_=(16, v)
  }
  
  case class UInt32 extends Value with Union {
    protected val tag = 8
    
    def value = uint32Field(32)
    def value_=(v: Long) = uint32Field_=(32, v)
  }
  
  case class UInt64 extends Value with Union {
    protected val tag = 9
    
    def value = uint64Field(64)
    def value_=(v: BigInt) = uint64Field_=(64, v)
  }
  
  case class Float32 extends Value with Union {
    protected val tag = 10
    
    def value = float32Field(32)
    def value_=(v: Float) = float32Field_=(32, v)
  }
  
  case class Float64 extends Value with Union {
    protected val tag = 11
    
    def value = float64Field(64)
    def value_=(v: Double) = float64Field_=(64, v)
  }
  
  case class Text extends Value with Union {
    protected val tag = 12
    
    def value = textField(0)
    def value_=(v: String) = textField_=(0, v)
  }
  
  case class Data extends Value with Union {
    protected val tag = 13
    
    def value = dataField(0)
    def value_=(v: Array[Byte]) = dataField_=(0, v)
  }
  
  case class List extends Value with Union {
    protected val tag = 14
    
    def value = ptrField[AnyRef](0)
    def value_=(v: AnyRef) = ptrField_=(0, v)
  }
  
  case class Enum extends Value with Union {
    protected val tag = 15
    
    def value = uint16Field(16)
    def value_=(v: Int) = uint16Field_=(16, v)
  }
  
  case class Struct extends Value with Union {
    protected val tag = 16
    
    def value = ptrField[AnyRef](0)
    def value_=(v: AnyRef) = ptrField_=(0, v)
  }
  
  case class Interface extends Value with Union {
    protected val tag = 17
  }
  
  case class Object extends Value with Union {
    protected val tag = 18
    
    def value = ptrField[AnyRef](0)
    def value_=(v: AnyRef) = ptrField_=(0, v)
  }
}