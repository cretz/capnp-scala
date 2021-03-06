package org.capnp.model

import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner
import java.nio.ByteBuffer

@RunWith(classOf[JUnitRunner])
class ByteBufSpec extends Specification {
  "ByteBuf" should {
    def withBuf[T](f: (ByteBuf) => T): T = f(new ByteBuf(128 * 64L))

    def checkReadWrite[T](v: T, w: (Long, T) => ByteBuf,
        r: (Long) => T, offset: Int = 16) = {
      w(offset, v)
      r(offset) mustEqual v
    }
    
    def binaryBuf(str: String): ByteBuf = {
      val buf = new ByteBuf(str.length)
      augmentString(str).zipWithIndex.map(c => buf.writeBool(c._2, c._1 == '1'))
      buf
    }
    
    "toBinaryString" in {
      binaryBuf("1010101010101010").toBinaryString mustEqual "1010101010101010"
      binaryBuf("1101111001111111").toBinaryString mustEqual "1101111001111111"
    }

    "read and write normal primitives properly" in {
      withBuf(b => checkReadWrite(true, b.writeBool, b.readBool))
      withBuf(b => checkReadWrite(false, b.writeBool, b.readBool))
      // Strange offsets
      withBuf(b => checkReadWrite(true, b.writeBool, b.readBool, 13))
      withBuf(b => checkReadWrite(false, b.writeBool, b.readBool, 17))
      // More primitives
      withBuf(b => checkReadWrite(1.23F, b.writeFloat32, b.readFloat32))
      withBuf(b => checkReadWrite(1.23, b.writeFloat64, b.readFloat64))
      withBuf(b => checkReadWrite(12.toByte, b.writeInt8, b.readInt8))
      withBuf(b => checkReadWrite(12.toShort, b.writeInt16, b.readInt16))
      withBuf(b => checkReadWrite(12, b.writeInt32, b.readInt32))
      withBuf(b => checkReadWrite(12.toLong, b.writeInt64, b.readInt64))
      withBuf(b => checkReadWrite(12.toShort, b.writeUInt8, b.readUInt8))
      withBuf(b => checkReadWrite(12, b.writeUInt16, b.readUInt16))
      withBuf(b => checkReadWrite(12.toLong, b.writeUInt32, b.readUInt32))
      withBuf(b => checkReadWrite(BigInt(12), b.writeUInt64, b.readUInt64))
    }
    
    "readFirstUInt2 from the beginning of a byte" in {
      binaryBuf("1111111100111111").readFirstUInt2(8) mustEqual 0
      binaryBuf("1111111110111111").readFirstUInt2(8) mustEqual 1
      binaryBuf("1111111101111111").readFirstUInt2(8) mustEqual 2
      binaryBuf("1111111111111111").readFirstUInt2(8) mustEqual 3
    }
    
    "writeFirstUInt2" in {
      binaryBuf("1111111111111111").writeFirstUInt2(8, 0).toBinaryString mustEqual
        "1111111100111111"
      binaryBuf("1111111111111111").writeFirstUInt2(8, 1).toBinaryString mustEqual
        "1111111110111111"
      binaryBuf("1111111111111111").writeFirstUInt2(8, 2).toBinaryString mustEqual
        "1111111101111111"
      binaryBuf("0000000000000000").writeFirstUInt2(8, 3).toBinaryString mustEqual
        "0000000011000000"
      binaryBuf("0101010101010101").writeFirstUInt2(8, 3).toBinaryString mustEqual
        "0101010111010101"
      withBuf(_.writeFirstUInt2(16, 0).readFirstUInt2(16)) mustEqual 0
      withBuf(_.writeFirstUInt2(16, 1).readFirstUInt2(16)) mustEqual 1
      withBuf(_.writeFirstUInt2(16, 2).readFirstUInt2(16)) mustEqual 2
      withBuf(_.writeFirstUInt2(16, 3).readFirstUInt2(16)) mustEqual 3
    }
    
    "readFirstUInt3 from the beginning of a byte" in {
      binaryBuf("1111111100011111").readFirstUInt3(8) mustEqual 0
      binaryBuf("1111111101011111").readFirstUInt3(8) mustEqual 2
      binaryBuf("1111111110111111").readFirstUInt3(8) mustEqual 5
      binaryBuf("1111111101111111").readFirstUInt3(8) mustEqual 6
      binaryBuf("1111111011101111").readFirstUInt3(8) mustEqual 7
    }

    "writeFirstUInt3" in {
      binaryBuf("1111111111111111").writeFirstUInt3(8, 0).toBinaryString mustEqual
        "1111111100011111"
      binaryBuf("1111111111111111").writeFirstUInt3(8, 1).toBinaryString mustEqual
        "1111111110011111"
      binaryBuf("1111111111111111").writeFirstUInt3(8, 2).toBinaryString mustEqual
        "1111111101011111"
      binaryBuf("0000000000000011").writeFirstUInt3(8, 3).toBinaryString mustEqual
        "0000000011000011"
      binaryBuf("0101010101010101").writeFirstUInt3(8, 3).toBinaryString mustEqual
        "0101010111010101"
      withBuf(_.writeFirstUInt3(16, 0).readFirstUInt3(16)) mustEqual 0
      withBuf(_.writeFirstUInt3(16, 2).readFirstUInt3(16)) mustEqual 2
      withBuf(_.writeFirstUInt3(16, 5).readFirstUInt3(16)) mustEqual 5
      withBuf(_.writeFirstUInt3(16, 6).readFirstUInt3(16)) mustEqual 6
      withBuf(_.writeFirstUInt3(16, 7).readFirstUInt3(16)) mustEqual 7
    }

    "readLastInt30 from the end of byte array" in {
      binaryBuf("11011111111111111111111111111111").readLastInt30(0) mustEqual
        binaryBuf("01111111111111111111111111111100").readInt32(0)
      binaryBuf("01011111111111000000001111111111").readLastInt30(0) mustEqual
        binaryBuf("01111111111100000000111111111100").readInt32(0)
      binaryBuf("00010000000000000000000000000000").readLastInt30(0) mustEqual
        binaryBuf("01000000000000000000000000000000").readInt32(0)
      binaryBuf("11100000000000000000000000000000").readLastInt30(0) mustEqual
        binaryBuf("10000000000000000000000000000000").readInt32(0)
    }
    
    "readLastUInt29 from the end of byte array" in {
      binaryBuf("11101111111111111111111111111111").readLastUInt29(0) mustEqual
        binaryBuf("01111111111111111111111111111000").readUInt32(0)
      binaryBuf("11010111111111000000001111111111").readLastUInt29(0) mustEqual
        binaryBuf("10111111111000000001111111111000").readUInt32(0)
      binaryBuf("00001000000000000000000000000000").readLastUInt29(0) mustEqual
        binaryBuf("01000000000000000000000000000000").readUInt32(0)
    }
  }
}