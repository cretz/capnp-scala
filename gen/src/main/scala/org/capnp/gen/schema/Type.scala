package org.capnp.gen.schema

import org.capnp.model
import org.capnp.model._

sealed abstract class Type extends Struct(0xd07378ede1f9cc60L)
object Type {
  case class Void extends Type with Union {
    protected val tag = 0
  }
  
  case class Bool extends Type with Union {
    protected val tag = 1
  }
  
  case class Int8 extends Type with Union {
    protected val tag = 2
  }
  
  case class Int16 extends Type with Union {
    protected val tag = 3
  }
  
  case class Int32 extends Type with Union {
    protected val tag = 4
  }
  
  case class Int64 extends Type with Union {
    protected val tag = 5
  }
  
  case class UInt8 extends Type with Union {
    protected val tag = 6
  }
  
  case class UInt16 extends Type with Union {
    protected val tag = 7
  }
  
  case class UInt32 extends Type with Union {
    protected val tag = 8
  }
  
  case class UInt64 extends Type with Union {
    protected val tag = 9
  }
  
  case class Float32 extends Type with Union {
    protected val tag = 10
  }
  
  case class Float64 extends Type with Union {
    protected val tag = 11
  }
  
  case class Text extends Type with Union {
    protected val tag = 12
  }
  
  case class Data extends Type with Union {
    protected val tag = 13
  }
  
  case class List extends Type with Union with Group {
    protected val tag = 14
    
    def elementType = ptrField[Type](0)
    def elementType_=(v: Type) = ptrField_=(0, v)
  }
  
  case class Enum extends Type with Union with Group {
    protected val tag = 15
    
    def typeId = uint64Field(64)
    def typeId_=(v: BigInt) = uint64Field_=(64, v)
  }
  
  case class Struct extends Type with Union with Group {
    protected val tag = 16
    
    def typeId = uint64Field(64)
    def typeId_=(v: BigInt) = uint64Field_=(64, v)
  }
  
  case class Interface extends Type with Union with Group {
    protected val tag = 17
    
    def typeId = uint64Field(64)
    def typeId_=(v: BigInt) = uint64Field_=(64, v)
  }
  
  case class Object extends Type with Union {
    protected val tag = 18
  }
}