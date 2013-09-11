package org.capnp.model

import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class MessageSpec extends Specification {
  "MessageSpec" should {
    "see all proper values in addressbook bin" in {
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
      val m = Message.readAll(getClass.getResourceAsStream("addressbook.capnp.bin"), true)
      m.rootBuf.dataStart mustEqual 0
      m.rootBuf.dataSize mustEqual 0
      m.rootBuf.pointerSize mustEqual 64
      m.rootBuf.pointables.size mustEqual 1
      val people = m.rootBuf.pointables(0).asInstanceOf[CompositeListBuf]
      people.count mustEqual 2
      val alice = people.get(0).asInstanceOf[StructBuf]
      alice.buf.readUInt32(0) mustEqual 123
      alice.pointables(0).asInstanceOf[PrimitiveListBuf].asString mustEqual "Alice"
      alice.pointables(1).asInstanceOf[PrimitiveListBuf].asString mustEqual "alice@example.com"
      // Check union tag bits
      alice.buf.readUInt16(32) mustEqual 2
      alice.pointables(3).asInstanceOf[PrimitiveListBuf].asString mustEqual "MIT"
      // Phones
      val alicePhones = alice.pointables(2).asInstanceOf[CompositeListBuf]
      alicePhones.count mustEqual 1
      val alicePhone1 = alicePhones.get(0).asInstanceOf[StructBuf]
      alicePhone1.pointables(0).asInstanceOf[PrimitiveListBuf].asString mustEqual "555-1212"
      // Type
      alicePhone1.buf.readUInt16(0) mustEqual 0
      val bob = people.get(1).asInstanceOf[StructBuf]
      bob.buf.readUInt32(0) mustEqual 456
      bob.pointables(0).asInstanceOf[PrimitiveListBuf].asString mustEqual "Bob"
      bob.pointables(1).asInstanceOf[PrimitiveListBuf].asString mustEqual "bob@example.com"
      bob.buf.readUInt16(32) mustEqual 0
      val bobPhones = bob.pointables(2).asInstanceOf[CompositeListBuf]
      bobPhones.count mustEqual 2
      val bobPhone1 = bobPhones.get(0).asInstanceOf[StructBuf]
      bobPhone1.pointables(0).asInstanceOf[PrimitiveListBuf].asString mustEqual "555-4567"
      bobPhone1.buf.readUInt16(0) mustEqual 1
      val bobPhone2 = bobPhones.get(1).asInstanceOf[StructBuf]
      bobPhone2.pointables(0).asInstanceOf[PrimitiveListBuf].asString mustEqual "555-7654"
      bobPhone2.buf.readUInt16(0) mustEqual 2
      1 mustEqual 1
    }
  }
}