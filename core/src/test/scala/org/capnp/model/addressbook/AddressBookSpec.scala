package org.capnp.model.addressbook

import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner
import org.capnp.model.Message
import org.capnp.model.addressbook.Person.PhoneNumber.Type

@RunWith(classOf[JUnitRunner])
class AddressBookSpec extends Specification {
  "AddressBook model"should {
    "read normal model properly" in {
      /* Here's what the data looks like (in order):
       * {
       *   "people": [
       *     {
       *       "id": 123,
       *       "name": "Alice",
       *       "email": "alice@example.com",
       *       "phones": [
       *         { "number": "555-1212" }
       *       ],
       *       "employment": {
       *         "school": "MIT"
       *       }
       *     },
       *     {
       *       "id": 456,
       *       "name": "Bob",
       *       "email": "bob@example.com",
       *       "phones": [
       *         { "number": "555-4567", "type": home },
       *         { "number": "555-7654", "type": work }
       *       ]
       *     }
       *   ]
       * }
       */
      val m = Message.readAll(getClass.getResourceAsStream("../addressbook.capnp.bin"), true)
      val book = m.root(AddressBook)
      book.people.size mustEqual 2
      val alice = book.people.head
      alice.id mustEqual 123
      alice.name mustEqual "Alice"
      alice.email mustEqual "alice@example.com"
      alice.phones.size mustEqual 1
      alice.phones.head.number mustEqual "555-1212"
      alice.phones.head.`type` mustEqual Type.mobile
      alice.employment.asInstanceOf[alice.Employment.School].value mustEqual "MIT"
      val bob = book.people(1)
      bob.id mustEqual 456
      bob.name mustEqual "Bob"
      bob.email mustEqual "bob@example.com"
      bob.phones map(p => (p.number, p.`type`)) mustEqual Seq("555-4567" -> Type.home, "555-7654" -> Type.work)
    }
  }
}