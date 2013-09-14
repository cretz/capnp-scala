package org.capnp.gen.schema

import org.capnp.model._

sealed abstract class Type extends Struct(0xd07378ede1f9cc60L, 16, 1) with Union
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
  
  case class Void() extends Type {
    protected val unionTag = 0
  }
  
  case class Bool() extends Type {
    protected val unionTag = 1
  }
  
  case class Int8() extends Type {
    protected val unionTag = 2
  }
  
  case class Int16() extends Type {
    protected val unionTag = 3
  }
  
  case class Int32() extends Type {
    protected val unionTag = 4
  }
  
  case class Int64() extends Type {
    protected val unionTag = 5
  }
  
  case class UInt8() extends Type {
    protected val unionTag = 6
  }
  
  case class UInt16() extends Type {
    protected val unionTag = 7
  }
  
  case class UInt32() extends Type {
    protected val unionTag = 8
  }
  
  case class UInt64() extends Type {
    protected val unionTag = 9
  }
  
  case class Float32() extends Type {
    protected val unionTag = 10
  }
  
  case class Float64() extends Type {
    protected val unionTag = 11
  }
  
  case class Text() extends Type {
    protected val unionTag = 12
  }
  
  case class Data() extends Type {
    protected val unionTag = 13
  }
  
  case class List() extends Type with Group {
    protected val unionTag = 14
    
    def elementType = structField[Type](0, Type)
    def elementType_=(v: Option[Type]) = structField_=(0, v)
  }
  
  case class Enum() extends Type with Group {
    protected val unionTag = 15
    
    def typeId = uint64Field(64)
    def typeId_=(v: BigInt) = uint64Field_=(64, v)
  }
  
  case class Struct() extends Type with Group {
    protected val unionTag = 16
    
    def typeId = uint64Field(64)
    def typeId_=(v: BigInt) = uint64Field_=(64, v)
  }
  
  case class Interface() extends Type with Group {
    protected val unionTag = 17
    
    def typeId = uint64Field(64)
    def typeId_=(v: BigInt) = uint64Field_=(64, v)
  }
  
  case class Object() extends Type {
    protected val unionTag = 18
  }
}