package org.capnp.model.addressbook

import org.capnp.model.Struct
import org.capnp.model.StructObject
import org.capnp.model.Group
import org.capnp.model.Union
import org.capnp.model.UnionObject

case class Person() extends Struct(0x98808e9832e8bc18L, 8, 4) {
  def id = uint32Field(0)
  def id_=(v: Long) = uint32Field_=(0, v)
  
  def name = textField(0)
  def name_=(v: String) = textField_=(0, v)
  
  def email = textField(1)
  def email_=(v: String) = textField_=(1, v)
  
  def phones = structSeq(2, Person.PhoneNumber)
  def phones_=(v: Seq[Person.PhoneNumber]) = structSeq_=(2, v)
  
  def employment = unionField(32, Employment)
  def employment_=(v: Employment) = unionField_=(32, v)
  
  sealed abstract class Employment extends Group with Union
  object Employment extends UnionObject[Employment] {
    protected lazy val cases = Map(
      0 -> Unemployed,
      1 -> Employer,
      2 -> School,
      3 -> SelfEmployed
    )
    
    case class Unemployed() extends Employment {
      protected val unionTag = 0
    }
    
    case class Employer() extends Employment {
      protected val unionTag = 1
    
      def value = textField(3)
      def value_=(v: String) = textField_=(3, v)
    }
    
    case class School() extends Employment {
      protected val unionTag = 2
    
      def value = textField(3)
      def value_=(v: String) = textField_=(3, v)
    }
    
    case class SelfEmployed() extends Employment {
      protected val unionTag = 3
    }
  }
}
object Person extends StructObject[Person] {
  case class PhoneNumber() extends Struct(0x814e90b29c9e8ad0L, 8, 1) {
    def number = textField(0)
    def number_=(v: String) = textField_=(0, v)
    
    def `type` = enumField(0, PhoneNumber.Type.apply)
    def type_=(v: PhoneNumber.Type.Value) = enumField_=(0, v)
  }
  object PhoneNumber extends StructObject[PhoneNumber] {
    object Type extends Enumeration {
      val mobile = Value(0)
      val home = Value(1)
      val work = Value(2)
    }
  }
}