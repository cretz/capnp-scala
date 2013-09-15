package org.capnp.model.addressbook

import org.capnp.model._

case class AddressBook(ptr: StructPtr) extends Struct {
  def people = structSeq(0, Person)
  def people_=(v: Seq[Person]) = structSeq_=(0, v)
}

object AddressBook extends StructObject[AddressBook](0, 4)