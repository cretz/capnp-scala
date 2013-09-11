package org.capnp.model.addressbook

import org.capnp.model.Struct
import java.nio.ByteBuffer

case class AddressBook extends Struct(0xf934d9b354a8a134L, 1, 4) {
  def people = seqField[Person](0)
  def people_=(v: Seq[Person]) = seqField_=(0, v)
}
