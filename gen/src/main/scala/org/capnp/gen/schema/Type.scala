package org.capnp.gen.schema

import org.capnp.model._

sealed abstract class Type extends Struct with Union

object Type extends AnonUnionObject[Type] {
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
  
  case class Void(ptr: StructPtr) extends Type {
    protected val unionTag = 0
  }
  
  object Void extends StructObject[Void](16, 1)
  
  case class Bool(ptr: StructPtr) extends Type {
    protected val unionTag = 1
  }
  
  object Bool extends StructObject[Bool](16, 1)
  
  case class Int8(ptr: StructPtr) extends Type {
    protected val unionTag = 2
  }
  
  object Int8 extends StructObject[Int8](16, 1)
  
  case class Int16(ptr: StructPtr) extends Type {
    protected val unionTag = 3
  }
  
  object Int16 extends StructObject[Int16](16, 1)
  
  case class Int32(ptr: StructPtr) extends Type {
    protected val unionTag = 4
  }
  
  object Int32 extends StructObject[Int32](16, 1)
  
  case class Int64(ptr: StructPtr) extends Type {
    protected val unionTag = 5
  }
  
  object Int64 extends StructObject[Int64](16, 1)
  
  case class UInt8(ptr: StructPtr) extends Type {
    protected val unionTag = 6
  }
  
  object UInt8 extends StructObject[UInt8](16, 1)
  
  case class UInt16(ptr: StructPtr) extends Type {
    protected val unionTag = 7
  }
  
  object UInt16 extends StructObject[UInt16](16, 1)
  
  case class UInt32(ptr: StructPtr) extends Type {
    protected val unionTag = 8
  }
  
  object UInt32 extends StructObject[UInt32](16, 1)
  
  case class UInt64(ptr: StructPtr) extends Type {
    protected val unionTag = 9
  }
  
  object UInt64 extends StructObject[UInt64](16, 1)
  
  case class Float32(ptr: StructPtr) extends Type {
    protected val unionTag = 10
  }
  
  object Float32 extends StructObject[Float32](16, 1)
  
  case class Float64(ptr: StructPtr) extends Type {
    protected val unionTag = 11
  }
  
  object Float64 extends StructObject[Float64](16, 1)
  
  case class Text(ptr: StructPtr) extends Type {
    protected val unionTag = 12
  }
  
  object Text extends StructObject[Text](16, 1)
  
  case class Data(ptr: StructPtr) extends Type {
    protected val unionTag = 13
  }
  
  object Data extends StructObject[Data](16, 1)
  
  case class List(ptr: StructPtr) extends Type with Group {
    protected val unionTag = 14
    
    def elementType = structField[Type](0, Type)
    def elementType_=(v: Option[Type]) = structField_=(0, v)
  }
  
  object List extends StructObject[List](16, 1)
  
  case class Enum(ptr: StructPtr) extends Type with Group {
    protected val unionTag = 15
    
    def typeId = uint64Field(64)
    def typeId_=(v: BigInt) = uint64Field_=(64, v)
  }
  
  object Enum extends StructObject[Enum](16, 1)
  
  case class Struct(ptr: StructPtr) extends Type with Group {
    protected val unionTag = 16
    
    def typeId = uint64Field(64)
    def typeId_=(v: BigInt) = uint64Field_=(64, v)
  }
  
  object Struct extends StructObject[Struct](16, 1)
  
  case class Interface(ptr: StructPtr) extends Type with Group {
    protected val unionTag = 17
    
    def typeId = uint64Field(64)
    def typeId_=(v: BigInt) = uint64Field_=(64, v)
  }
  
  object Interface extends StructObject[Interface](16, 1)
  
  case class Object(ptr: StructPtr) extends Type {
    protected val unionTag = 18
  }
  
  object Object extends StructObject[Object](16, 1)
}