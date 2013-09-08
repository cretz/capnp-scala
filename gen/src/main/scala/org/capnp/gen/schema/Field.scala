package org.capnp.gen.schema

import org.capnp.model
import org.capnp.model._

sealed abstract class Field extends Struct(0x9aad50a41f4af45fL) {
  def name = textField(0)
  def name_=(v: String) = textField_=(0, v)
  
  def codeOrder = uint16Field(0)
  def codeOrder_=(v: Int) = uint16Field_=(0, v)
  
  def annotations = seqField[Annotation](1)
  def annotations_=(v: Seq[Annotation]) = seqField_=(1, v)
  
  def discriminantValue = uint16Field(16)
  def discriminantValue_=(v: Int) = uint16Field_=(16, v)
  
  def ordinal = groupField[Ordinal]
  def ordinal_=(v: Ordinal) = groupField_=(v)
  
  sealed abstract class Ordinal extends Group
  object Ordinal {
    case class Implicit extends Ordinal with Union {
      protected val tag = 0
    }
    
    case class Explicit extends Ordinal with Union {
      protected val tag = 1
      
      def value = uint16Field(96)
      def value_=(v: Int) = uint16Field_=(96, v) 
    }
  }
}
object Field {
  case class Slot extends Field with Union with model.Group {
    protected val tag = 0
  
    def offset = uint32Field(32)
    def offset_=(v: Long) = uint32Field_=(32, v)
  
    def `type` = ptrField[Type](2)
    def type_=(v: Type) = ptrField_=(2, v)
  
    def `value` = ptrField[Value](3)
    def value_=(v: Value) = ptrField_=(3, v)
  }
  
  case class Group extends Field with Union with model.Group {
    protected val tag = 1
    
    def typeId = uint64Field(128)
    def typeId_=(v: BigInt) = uint64Field_=(128, v)
  }
}