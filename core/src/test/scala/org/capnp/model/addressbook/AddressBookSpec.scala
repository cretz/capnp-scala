package org.capnp.model.addressbook

import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner
import org.capnp.model.Message

@RunWith(classOf[JUnitRunner])
class AddressBookSpec extends Specification {
  "AddressBook model"should {
    "read normal model properly" in {
      val m = Message.readAll(getClass.getResourceAsStream("../addressbook.capnp.bin"), true)
      val a = m.root(AddressBook)
      a.people.size mustEqual 2
      1 mustEqual 1
    }
  }
}