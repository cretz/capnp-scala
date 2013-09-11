package org.capnp.model

import java.nio.ByteBuffer
import java.nio.ByteOrder
import java.math.BigInteger
import scala.collection.mutable
import scala.collection.immutable.BitSet

// All offsets in bits
class ByteBuf(val buf: ByteBuffer, val size: Long) {
  assert(size % 8 == 0)
  buf.order(ByteOrder.LITTLE_ENDIAN)
  
  def this(size: Long) = this(ByteBuffer.allocate(size / 8 toInt), size)
  def this(buf: ByteBuffer) = this(buf, (buf.capacity - buf.arrayOffset) * 8)
  
  def capacity = buf.capacity * 8L
  def arrayOffset = buf.arrayOffset * 8L
  
  @inline
  def toBytes(bits: Long, mustBeAtByteBoundary: Boolean = true) = {
    if (mustBeAtByteBoundary) assert(bits % 8 == 0)
    bits / 8 toInt
  }
  
  def readBool(offset: Long) =
    (buf.get(toBytes(offset, false)) >> (offset % 8) & 1) == 1
  def writeBool(offset: Long, v: Boolean) = {
    val byteOffset = toBytes(offset, false)
    val byte = buf.get(byteOffset)
    buf.put(
      byteOffset,
      if (v)
        (byte | (1 << (offset % 8).toByte)).toByte
      else
        (byte & ~(1 << (offset % 8))).toByte
    )
    this
  }
  
  def readInt8(offset: Long) = buf.get(toBytes(offset))
  def writeInt8(offset: Long, v: Byte) = {
    buf.put(toBytes(offset), v)
    this
  }
  
  def readInt16(offset: Long) = buf.getShort(toBytes(offset))
  def writeInt16(offset: Long, v: Short) = {
    buf.putShort(toBytes(offset), v)
    this
  }
  
  def readInt32(offset: Long) = buf.getInt(toBytes(offset))
  def writeInt32(offset: Long, v: Int) = {
    buf.putInt(toBytes(offset), v)
    this
  }
  
  def readInt64(offset: Long) = buf.getLong(toBytes(offset))
  def writeInt64(offset: Long, v: Long) = {
    buf.putLong(toBytes(offset), v)
    this
  }
  
  def readUInt8(offset: Long) = (buf.get(toBytes(offset)) & 0xff).toShort
  def writeUInt8(offset: Long, v: Short) = {
    buf.put(toBytes(offset), (v & 0xff).toByte)
    this
  }
  
  def readUInt16(offset: Long) = buf.getShort(toBytes(offset)) & 0xffff
  def writeUInt16(offset: Long, v: Int) = {
    buf.putShort(toBytes(offset), (v & 0xffff).toShort)
    this
  }
  
  def readUInt32(offset: Long) = buf.getInt(toBytes(offset)) & 0xffffffffL
  def writeUInt32(offset: Long, v: Long) = {
    buf.putInt(toBytes(offset), (v & 0xffffffff).toInt)
    this
  }
  
  def readUInt64(offset: Long) = BigInt(
    1,
    (0 until 8).reverse.map(i => buf.get(toBytes(offset) + i)).toArray
  )
  def writeUInt64(offset: Long, v: BigInt) = {
    buf.putLong(toBytes(offset), v.longValue)
    this
  }
  
  def readFloat32(offset: Long) = buf.getFloat(toBytes(offset))
  def writeFloat32(offset: Long, v: Float) = {
    buf.putFloat(toBytes(offset), v)
    this
  }
  
  def readFloat64(offset: Long) = buf.getDouble(toBytes(offset))
  def writeFloat64(offset: Long, v: Double) = {
    buf.putDouble(toBytes(offset), v)
    this
  }
  
  def slice(offset: Long): ByteBuf = slice(offset, size - offset)
  
  def slice(offset: Long, size: Long) = {
    buf.position(toBytes(offset))
    new ByteBuf(buf.slice(), size)
  }
  
  def rotr(b: Byte, c: Int) = (Integer.rotateRight(b, c) >> 24).toByte
  
  def toBinaryString() = {
    buf.array.drop(buf.arrayOffset).foldLeft("") { (s, b) =>
      s + ("%8s" format(Integer.toBinaryString(b & 0xff)) replace(' ', '0')) + ' '
    }
  }
  
  // Special
  
  def readFirstUInt2(offset: Long) = {
    ((rotr(readInt8(offset), 2) & 0xff) >> 6).toByte
  }
  
  def readFirstUInt3(offset: Long) = {
    ((rotr(readInt8(offset), 3) & 0xff) >> 5).toByte
  }
  
  def readLastUInt29(offset: Long) = {
    (readInt32(offset) >>> 3) & 0xffffffffL
  }
  
  def readLastInt30(offset: Long) = {
    readInt32(offset) >>> 2
  }
}