package org.capnp.gen.schema

import org.capnp.model
import org.capnp.model._

sealed abstract class Field extends Struct(0x9aad50a41f4af45fL, 24, 4) with Union {
  def name = textField(0)
  def name_=(v: String) = textField_=(0, v)
  
  def codeOrder = uint16Field(0)
  def codeOrder_=(v: Int) = uint16Field_=(0, v)
  
  def annotations = structSeq[Annotation](1, Annotation)
  def annotations_=(v: Seq[Annotation]) = structSeq_=(1, v)
  
  def discriminantValue = uint16Field(16)
  def discriminantValue_=(v: Int) = uint16Field_=(16, v)
  
  def ordinal = unionField(80, Ordinal)
  def ordinal_=(v: Ordinal) = unionField_=(80, v)
  
  sealed abstract class Ordinal extends Union with Group
  
  object Ordinal extends UnionObject[Ordinal] {
    protected val cases = Map(
      0 -> Implicit,
      1 -> Explicit
    )
    
    case class Implicit() extends Ordinal with Union {
      protected val unionTag = 0
    }
    
    case class Explicit() extends Ordinal with Union {
      protected val unionTag = 1
      
      def value = uint16Field(96)
      def value_=(v: Int) = uint16Field_=(96, v) 
    }
  }
}

object Field extends AnonUnionObject[Field] {
  protected val unionTagBitOffset = 64L
  protected val cases = Map(
    0 -> Slot,
    1 -> Group
  )
  
  case class Slot() extends Field with model.Group {
    protected val unionTag = 0
  
    def offset = uint32Field(32)
    def offset_=(v: Long) = uint32Field_=(32, v)
  
    def `type` = structField[Type](2, Type)
    def type_=(v: Option[Type]) = structField_=(2, v)
  
    def `value` = structField[Value](3, Value)
    def value_=(v: Option[Value]) = structField_=(3, v)
  }
  
  case class Group() extends Field with model.Group {
    protected val unionTag = 1
    
    def typeId = uint64Field(128)
    def typeId_=(v: BigInt) = uint64Field_=(128, v)
  }
}