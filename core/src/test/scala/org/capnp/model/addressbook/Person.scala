package org.capnp.model.addressbook

import org.capnp.model._

case class Person(ptr: StructPtr) extends Struct {
  def id = uint32Field(0)
  def id_=(v: Long) = uint32Field_=(0, v)
  
  def name = textField(0)
  def nameOption = textFieldOption(0)
  def name_=(v: String) = textField_=(0, v)
  
  def email = textField(1)
  def emailOption = textFieldOption(1)
  def email_=(v: String) = textField_=(1, v)
  
  def phones = structSeq(2, Person.PhoneNumber)
  def phonesOption = structSeqOption(0, Person.PhoneNumber)
  def newPhones(size: Int) = newStructSeq(0, size, Person.PhoneNumber)
  def withPhones(size: Int, assertSize: Boolean = true) = withStructSeq(0, size, assertSize, Person.PhoneNumber)
  
  def employment = unionField(32, Employment)
  def employmentAs[T <: Employment](u: () => T) = unionFieldAs(32, u)
  
  sealed abstract class Employment extends Group with Union

  object Employment extends UnionObject[Employment] {
    protected val unionTagBitOffset = 32
    protected lazy val cases = Map(
      0 -> Unemployed,
      1 -> Employer,
      2 -> School,
      3 -> SelfEmployed
    )
    
    case class Unemployed() extends Employment {
      val unionTag = 0
    }
    
    case class Employer() extends Employment {
      val unionTag = 1
    
      def value = textField(3)
      def value_=(v: String) = textField_=(3, v)
    }
    
    case class School() extends Employment {
      val unionTag = 2
    
      def value = textField(3)
      def value_=(v: String) = textField_=(3, v)
    }
    
    case class SelfEmployed() extends Employment {
      val unionTag = 3
    }
  }
}

object Person extends StructObject[Person](8, 4) {
  case class PhoneNumber(ptr: StructPtr) extends Struct {
    def number = textField(0)
    def number_=(v: String) = textField_=(0, v)
    
    def `type` = enumField(0, PhoneNumber.Type.apply)
    def type_=(v: PhoneNumber.Type.Value) = enumField_=(0, v)
  }

  object PhoneNumber extends StructObject[PhoneNumber](8, 1) {
    object Type extends Enumeration {
      val mobile = Value(0)
      val home = Value(1)
      val work = Value(2)
    }
  }
}