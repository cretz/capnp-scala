package org.capnp.model

import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner
import scala.collection.immutable.BitSet

@RunWith(classOf[JUnitRunner])
class PackerSpec extends Specification {
  "PackerSpec" should {
    def signed(i: Int*) = i map(m => (m & 0xff).toByte)
    
    "unpack simple example from documentation" in {
      val packed = signed(0x51, 0x08, 0x03, 0x02, 0x31, 0x19, 0xaa, 0x01)
      val unpacked = signed(
          0x08, 0x00, 0x00, 0x00, 0x03, 0x00, 0x02, 0x00,
          0x19, 0x00, 0x00, 0x00, 0xaa, 0x01, 0x00, 0x00
      )
      Packer.unpack(packed.iterator).toIndexedSeq mustEqual unpacked
    }
    
    "unpack 0-flag example from documentation" in {
      val packed = signed(0x00, 0x03)
      val unpacked = signed(Seq.fill(32)(0x00):_*)
      Packer.unpack(packed.iterator).toIndexedSeq mustEqual unpacked
    }
    
    "unpack f-flag example from documentation" in {
      val packed = signed(
        (Seq(0xff) ++ Seq.fill(8)(0x8a) ++ Seq(0x03) ++ Seq.fill(24)(0x8a)):_*
      )
      val unpacked = signed(Seq.fill(32)(0x8a):_*)
      Packer.unpack(packed.iterator).toIndexedSeq mustEqual unpacked
    }
  }
}