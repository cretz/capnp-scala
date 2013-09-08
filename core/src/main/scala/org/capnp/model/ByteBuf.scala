package org.capnp.model

import java.nio.ByteBuffer
import java.nio.ByteOrder
import java.math.BigInteger
import scala.collection.mutable
import scala.collection.immutable.BitSet

// All offsets in bits
class ByteBuf(buf: ByteBuffer) {
  buf.order(ByteOrder.LITTLE_ENDIAN)
  
  def capacity = buf.capacity * 8L
  def arrayOffset = buf.arrayOffset * 8L
  
  @inline
  def toBytes(bits: Long, mustBeAtByteBoundary: Boolean = true) = {
    if (mustBeAtByteBoundary) assert(bits % 8 == 0)
    (bits / 8).toInt
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
  
  def readInt30(offset: Long): Int = {
    // Just read and zero out first 2 for now :-(
    (BitSet.fromBitMask(Array(readInt32(offset - 2))) - 30 - 31).toBitMask.head.toInt
  }
  def writeInt30(offset: Long, v: Int) = {
    // Write the full 32 bit, putting the two bool bits what they were
    val b = mutable.BitSet.fromBitMask(Array(v))
    b(30) = readBool(offset - 1)
    b(31) = readBool(offset - 2)
    writeInt32(offset - 2, b.toBitMask.head.toInt)
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
  
  def readUInt2(offset: Long): Byte = {
    val b = mutable.BitSet(8)
    b(0) = readBool(offset)
    b(1) = readBool(offset + 1)
    b.toBitMask.head.toByte
  }
  def writeUInt2(offset: Long, v: Byte) = { 
    writeBool(offset, v == 1 || v == 3)
    writeBool(offset + 1, v >= 2)
    this
  }
  
  def readUInt3(offset: Long): Byte = {
    val b = mutable.BitSet(8)
    b(0) = readBool(offset)
    b(1) = readBool(offset + 1)
    b(2) = readBool(offset + 2)
    b.toBitMask.head.toByte
  }
  def writeUInt3(offset: Long, v: Byte) = { 
    writeBool(offset, v % 2 == 1)
    writeBool(offset + 1, v == 2 || v == 3 || v == 6 || v == 7)
    writeBool(offset + 2, v >= 4)
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
  
//  def readInt30(offset: Long): Int = {
//    // Just read and zero out first 2 for now :-(
//    (BitSet.fromBitMask(Array(readInt32(offset - 2))) - 30 - 31).toBitMask.head.toInt
//  }
//  def writeInt30(offset: Long, v: Int) = {
//    // Write the full 32 bit, putting the two bool bits what they were
//    val b = mutable.BitSet.fromBitMask(Array(v))
//    b(30) = readBool(offset - 1)
//    b(31) = readBool(offset - 2)
//    writeInt32(offset - 2, b.toBitMask.head.toInt)
//  }
  
  def readUInt29(offset: Long): Int = {
    ((BitSet.fromBitMask(Array(readInt32(offset - 3))) -
      29 - 30 - 31).toBitMask.head & 0xffffffffL).toInt
  }
  def writeUInt29(offset: Long, v: Int) = {
    val b = mutable.BitSet.fromBitMask(Array(v & 0xffffffffL))
    b(29) = readBool(offset - 1)
    b(30) = readBool(offset - 2)
    b(31) = readBool(offset - 3)
    writeInt32(offset - 3, b.toBitMask.head.toInt)
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
  
  def slice(offset: Long) = {
    buf.position(toBytes(offset))
    new ByteBuf(buf.slice())
  }
}