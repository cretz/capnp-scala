package org.capnp.model.addressbook

import org.capnp.model._

case class AddressBook(ptr: StructPtr) extends Struct {
  def people = structSeq(0, Person)
  def peopleOption = structSeqOption(0, Person)
  def newPeople(size: Int) = newStructSeq(0, size, Person)
  def withPeople(size: Int, assertSize: Boolean = true) = withStructSeq(0, size, assertSize, Person)
}

object AddressBook extends StructObject[AddressBook](0, 4)