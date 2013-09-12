package org.capnp.model.addressbook

import org.capnp.model.Struct
import java.nio.ByteBuffer
import org.capnp.model.StructObject

case class AddressBook() extends Struct(0xf934d9b354a8a134L, 0, 4) {
  def people = structSeq(0, Person)
  def people_=(v: Seq[Person]) = structSeq_=(0, v)
}

object AddressBook extends StructObject[AddressBook]
