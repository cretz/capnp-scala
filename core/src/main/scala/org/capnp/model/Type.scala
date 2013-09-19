package org.capnp.model

object Type extends Enumeration {
  val Bool, Int8, Int16, Int32, Int64, UInt8, UInt16, UInt32, 
    UInt64, Float32, Float64, Ptr = Value
  
  def bitSize(v: Type.Value): Int = v match {
    case Bool => 1
    case Int8 => 8
    case Int16 => 16
    case Int32 => 32
    case Int64 => 64
    case UInt8 => 8
    case UInt16 => 16
    case UInt32 => 32 
    case UInt64 => 64
    case Float32 => 32
    case Float64 => 64
    case Ptr => 64
  }
  
  def primitiveListElementType(v: Type.Value): Byte = v match {
    case Bool => 1
    case Int8 => 2
    case Int16 => 3
    case Int32 => 4
    case Int64 => 5
    case UInt8 => 2
    case UInt16 => 3
    case UInt32 => 4
    case UInt64 => 5
    case Float32 => 4
    case Float64 => 5
    case _ => ???
  }
}