package org.capnp.model

import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class MessageSpec extends Specification {
  "MessageSpec" should {
//    "see segments in schema bin" in {
//      // It has 3
//      val m = Message.readAll(getClass.getResourceAsStream("schema.capnp.bin"))
//      m.segments must have length(3)
//      // Check offsets
//      println(m.segments(2).arrayOffset, m.segments(2).capacity)
//      println(m.segments(0).capacity, m.segments(0).readBool(124544))
//      println(m.root.dataSize, m.root.dataStart, m.root.pointerSize)
//      println(m.root.pointables)
//      val c = m.root.pointables.head.asInstanceOf[CompositeListBuf]
//      println(c.pointable.asInstanceOf[FarPointer].segIndex, 
//          c.pointable.asInstanceOf[FarPointer].segOffset)
//      m.segments.map(_.arrayOffset) mustEqual Seq(128, 65600, 130944)
//    }
    "see segments in addressbook bin" in {
      val m = Message.readAll(
        getClass.getResourceAsStream("addressbook.capnp.bin"), true
      )
      println(m.segments)
      1 mustEqual 1
    }
  }
}