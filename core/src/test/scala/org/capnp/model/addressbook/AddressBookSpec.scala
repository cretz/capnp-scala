package org.capnp.model.addressbook

import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner
import org.capnp.model.Message
import org.capnp.model.addressbook.Person.PhoneNumber.Type
import org.capnp.model.ReadOnlyMessage

@RunWith(classOf[JUnitRunner])
class AddressBookSpec extends Specification {
  "AddressBook model"should {
    
    def assertAddressBook(book: AddressBook) = {
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
      assertAddressBook(
          Message.readAll(getClass.getResourceAsStream("../addressbook.capnp.bin"), true).
          root(AddressBook).get)
    }
    
    "write model imperatively" in {
      val m = Message.forWrite(AddressBook)
      val alice = m.root.withPeople(2).head
      alice.id = 123
      alice.name = "Alice"
      alice.email = "alice@example.com"
      alice.withPhones(1).head.number = "555-1212"
      alice.phones.head.`type` = Type.mobile
      alice.employmentAs(alice.Employment.School).value = "MIT"
      val bob = m.root.people.last
      bob.id = 456
      bob.name = "Bob"
      bob.email = "bob@example.com"
      val bobPhones = bob.withPhones(2)
      bobPhones.head.number = "555-4567"
      bobPhones.head.`type` = Type.home
      bobPhones.last.number = "555-7654"
      bobPhones.last.`type` = Type.work
      // Make sure it equals right away
      assertAddressBook(m.root)
      // Now extract the segments and try
      assertAddressBook(ReadOnlyMessage(m.segments).root(AddressBook).get)
    }
    
    "write model functionally" in {
      val m = Message.forWrite(AddressBook)
      m.root.withPeople(2).zipWithIndex map {
        case (alice, 0) =>
          alice id = 123
          alice name = "Alice"
          alice email = "alice@example.com"
          alice withPhones(1) map { p =>
            p number = "555-1212"
            p `type` = Type.mobile
          }
          alice employmentAs(alice.Employment.School) value = "MIT"
        case (bob, 1) =>
          bob id = 456
          bob name = "Bob"
          bob email = "bob@example.com"
          bob.withPhones(2).zipWithIndex map {
            case (p, 0) =>
              p number = "555-4567"
              p `type` = Type.home
            case (p, 1) =>
              p number = "555-7654"
              p `type` = Type.work
          }
      }
      // Make sure it equals right away
      assertAddressBook(m.root)
      // Now extract the segments and try
      assertAddressBook(ReadOnlyMessage(m.segments).root(AddressBook).get)
    }
  }
}