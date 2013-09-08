package org.capnp.model

import java.nio.ByteBuffer
import java.nio.ByteOrder
import java.math.BigInteger

// All offsets in bits
class ByteBuf(buf: ByteBuffer) {
  assert(buf.order() == ByteOrder.LITTLE_ENDIAN)
  
  def this(size: Int) = {
    this(ByteBuffer.allocate(size))
    buf.order(ByteOrder.LITTLE_ENDIAN)
  }
  
  def readBool(offset: Int) =
    (buf.get(offset / 8) >> (offset % 8) & 1) == 1
  def writeBool(offset: Int, v: Boolean) = {
    val byte = buf.get(offset / 8)
    buf.put(
      offset / 8,
      if (v)
        (byte | (1 << (offset % 8).toByte)).toByte
      else
        (byte & ~(1 << (offset % 8))).toByte
    )
    this
  }
  
  def readInt8(offset: Int) = {
    assert(offset % 8 == 0)
    buf.get(offset / 8)
  }
  def writeInt8(offset: Int, v: Byte) = {
    assert(offset % 8 == 0)
    buf.put(offset / 8, v)
    this
  }
  
  def readInt16(offset: Int) = {
    assert(offset % 8 == 0)
    buf.getShort(offset / 8)
  }
  def writeInt16(offset: Int, v: Short) = {
    assert(offset % 8 == 0)
    buf.putShort(offset / 8, v)
    this
  }
  
  def readInt30(offset: Int): Int = ???
  
  def readInt32(offset: Int) = {
    assert(offset % 8 == 0)
    buf.getInt(offset / 8)
  }
  def writeInt32(offset: Int, v: Int) = {
    assert(offset % 8 == 0)
    buf.putInt(offset / 8, v)
    this
  }
  
  def readInt64(offset: Int) = {
    assert(offset % 8 == 0)
    buf.getLong(offset / 8)
  }
  def writeInt64(offset: Int, v: Long) = {
    assert(offset % 8 == 0)
    buf.putLong(offset / 8, v)
    this
  }
  
  def readUInt2(offset: Int): Byte = {
    ???
  }
  
  def readUInt3(offset: Int): Short = ???
  
  def readUInt8(offset: Int) = {
    assert(offset % 8 == 0)
    (buf.get(offset / 8) & 0xff).toShort
  }
  def writeUInt8(offset: Int, v: Short) = {
    assert(offset % 8 == 0)
    buf.put(offset / 8, (v & 0xff).toByte)
    this
  }
  
  def readUInt16(offset: Int) = {
    assert(offset % 8 == 0)
    buf.getShort(offset / 8) & 0xffff
  }
  def writeUInt16(offset: Int, v: Int) = {
    assert(offset % 8 == 0)
    buf.putShort(offset / 8, (v & 0xffff).toShort)
    this
  }
  
  def readUInt29(offset: Int) = ???
  
  def readUInt32(offset: Int) = {
    assert(offset % 8 == 0)
    buf.getInt(offset / 8) & 0xffffffffL
  }
  def writeUInt32(offset: Int, v: Long) = {
    assert(offset % 8 == 0)
    buf.putInt(offset / 8, (v & 0xffffffff).toInt)
    this
  }
  
  def readUInt64(offset: Int) = {
    assert(offset % 8 == 0)
    BigInt(
      1,
      (0 until 8).reverse.map(i => buf.get(offset / 8 + i)).toArray
    )
  }
  def writeUInt64(offset: Int, v: BigInt) = {
    assert(offset % 8 == 0)
    buf.putLong(offset / 8, v.longValue)
    this
  }
  
  def readFloat32(offset: Int) = {
    assert(offset % 8 == 0)
    buf.getFloat(offset / 8)
  }
  def writeFloat32(offset: Int, v: Float) = {
    assert(offset % 8 == 0)
    buf.putFloat(offset / 8, v)
    this
  }
  
  def readFloat64(offset: Int) = {
    assert(offset % 8 == 0)
    buf.getDouble(offset / 8)
  }
  def writeFloat64(offset: Int, v: Double) = {
    assert(offset % 8 == 0)
    buf.putDouble(offset / 8, v)
    this
  }
}