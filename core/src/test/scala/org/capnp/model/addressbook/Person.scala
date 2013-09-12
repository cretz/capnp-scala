package org.capnp.model.addressbook

import org.capnp.model.Struct
import org.capnp.model.StructObject

case class Person() extends Struct(0x98808e9832e8bc18L, 1, 1) {
  def id = uint32Field(0)
  def id_=(v: Long) = uint32Field_=(0, v)
  
  def name = textField(0)
  def name_=(v: Option[String]) = textField_=(0, v)
  def name_=(v: String) { name_=(Some(v)) }
  
  def email = textField(1)
  def email_=(v: Option[String]) = textField_=(1, v)
  def email_=(v: String) { email_=(Some(v)) }
  
  def people = structSeq(0, Person.PhoneNumber)
  def people_=(v: Seq[Person.PhoneNumber]) = structSeq_=(0, v)
}
object Person extends StructObject[Person] {
  case class PhoneNumber() extends Struct(0x814e90b29c9e8ad0L, 8, 1) {
    def number = textField(0)
    def number_=(v: Option[String]) = textField_=(0, v)
    def number_=(v: String) { number_=(Some(v)) }
    
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