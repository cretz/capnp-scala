package org.capnp.model

import org.specs2.mutable.SpecificationWithJUnit
import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ByteBufSpec extends Specification {
  "ByteBuf" should {
    def withBuf[T](f: (ByteBuf) => T): T = f(new ByteBuf(128))
    def checkReadWrite[T](v: T, w: (Int, T) => ByteBuf,
        r: (Int) => T, offset: Int = 16) = {
      w(offset, v)
      r(offset) mustEqual v
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
  }
}