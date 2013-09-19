package org.capnp.model

import java.nio.ByteBuffer
import java.nio.ByteOrder

// All offsets/sizes are in bits
trait ByteBuf {
  def readBool(offset: Long): Boolean
  def writeBool(offset: Long, v: Boolean): this.type
  def readInt8(offset: Long): Byte
  def writeInt8(offset: Long, v: Byte): this.type
  def readInt16(offset: Long): Short
  def writeInt16(offset: Long, v: Short): this.type
  def readInt32(offset: Long): Int
  def writeInt32(offset: Long, v: Int): this.type
  def readInt64(offset: Long): Long
  def writeInt64(offset: Long, v: Long): this.type
  def readUInt8(offset: Long): Short
  def writeUInt8(offset: Long, v: Short): this.type
  def readUInt16(offset: Long): Int
  def writeUInt16(offset: Long, v: Int): this.type
  def readUInt32(offset: Long): Long
  def writeUInt32(offset: Long, v: Long): this.type
  def readUInt64(offset: Long): BigInt
  def writeUInt64(offset: Long, v: BigInt): this.type
  def readFloat32(offset: Long): Float
  def writeFloat32(offset: Long, v: Float): this.type
  def readFloat64(offset: Long): Double
  def writeFloat64(offset: Long, v: Double): this.type
  def slice(offset: Long): ByteBuf
  def slice(offset: Long, size: Long): ByteBuf
  
  // Special
  def readFirstUInt2(offset: Long): Byte
  def writeFirstUInt2(offset: Long, v: Byte): this.type
  def readFirstUInt3(offset: Long): Byte
  def writeFirstUInt3(offset: Long, v: Byte): this.type
  def readLastUInt29(offset: Long): Long
  def writeLastUInt29(offset: Long, v: Long): this.type
  def readLastInt30(offset: Long): Int
  def writeLastInt30(offset: Long, v: Int): this.type
}

class ByteBufImpl(
    private val buf: ByteBuffer,
    private val start: Long,
    private val size: Long)
    extends ByteBuf {
  assert(start % 8 == 0)
  assert(size % 8 == 0)
  buf.order(ByteOrder.LITTLE_ENDIAN)
  
  def this(size: Long) = this(ByteBuffer.allocate((size / 8).toInt), 0, size)
  
  def this(buf: ByteBuffer) = this(buf, 0, buf.limit * 8L)
  
  private def toBytes(bits: Long, mustBeAtByteBoundary: Boolean = true): Int = {
    if (mustBeAtByteBoundary) assert(bits % 8 == 0)
    if (bits >= size) throw new IndexOutOfBoundsException
    (start / 8 + bits / 8).toInt
  }
  
  private def rotr(b: Byte, c: Int): Byte = (Integer.rotateRight(b, c) >> 24).toByte
  
  private def toBinaryString(): String = {
    buf.array.drop(buf.arrayOffset).foldLeft("") { (s, b) =>
      s + ("%8s" format(Integer.toBinaryString(b & 0xff)) replace(' ', '0')) + ' '
    }
  }
  
  def readBool(offset: Long): Boolean =
    (buf.get(toBytes(offset, false)) >> (offset % 8) & 1) == 1
    
  def writeBool(offset: Long, v: Boolean): this.type = {
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
  
  def readInt8(offset: Long): Byte = buf.get(toBytes(offset))
  
  def writeInt8(offset: Long, v: Byte): this.type = {
    buf.put(toBytes(offset), v)
    this
  }
  
  def readInt16(offset: Long): Short = buf.getShort(toBytes(offset))
  
  def writeInt16(offset: Long, v: Short): this.type = {
    buf.putShort(toBytes(offset), v)
    this
  }
  
  def readInt32(offset: Long): Int = buf.getInt(toBytes(offset))
  
  def writeInt32(offset: Long, v: Int): this.type = {
    buf.putInt(toBytes(offset), v)
    this
  }
  
  def readInt64(offset: Long): Long = buf.getLong(toBytes(offset))
  
  def writeInt64(offset: Long, v: Long): this.type = {
    buf.putLong(toBytes(offset), v)
    this
  }
  
  def readUInt8(offset: Long): Short = (buf.get(toBytes(offset)) & 0xff).toShort
  
  def writeUInt8(offset: Long, v: Short): this.type = {
    buf.put(toBytes(offset), (v & 0xff).toByte)
    this
  }
  
  def readUInt16(offset: Long): Int = buf.getShort(toBytes(offset)) & 0xffff
  
  def writeUInt16(offset: Long, v: Int): this.type = {
    buf.putShort(toBytes(offset), (v & 0xffff).toShort)
    this
  }
  
  def readUInt32(offset: Long): Long = buf.getInt(toBytes(offset)) & 0xffffffffL
  
  def writeUInt32(offset: Long, v: Long): this.type = {
    buf.putInt(toBytes(offset), (v & 0xffffffff).toInt)
    this
  }
  
  def readUInt64(offset: Long): BigInt =
    BigInt(1, (0 until 8).reverse.map(i => buf.get(toBytes(offset) + i)).toArray)

  def writeUInt64(offset: Long, v: BigInt): this.type = {
    buf.putLong(toBytes(offset), v.longValue)
    this
  }
  
  def readFloat32(offset: Long): Float = buf.getFloat(toBytes(offset))
  
  def writeFloat32(offset: Long, v: Float): this.type = {
    buf.putFloat(toBytes(offset), v)
    this
  }
  
  def readFloat64(offset: Long): Double = buf.getDouble(toBytes(offset))
  
  def writeFloat64(offset: Long, v: Double): this.type = {
    buf.putDouble(toBytes(offset), v)
    this
  }
  
  def slice(offset: Long): ByteBuf = slice(offset, size - offset)
  
  def slice(offset: Long, size: Long): ByteBuf =
    new ByteBufImpl(buf.duplicate(), start + offset, size)
  
  // Special
  
  def readFirstUInt2(offset: Long): Byte =
    ((rotr(readInt8(offset), 2) & 0xff) >> 6).toByte
  
  def writeFirstUInt2(offset: Long, v: Byte): this.type = ???
  
  def readFirstUInt3(offset: Long): Byte =
    ((rotr(readInt8(offset), 3) & 0xff) >> 5).toByte
  
  def writeFirstUInt3(offset: Long, v: Byte): this.type = ???
  
  def readLastUInt29(offset: Long): Long = (readInt32(offset) >>> 3) & 0xffffffffL
  
  def writeLastUInt29(offset: Long, v: Long): this.type = ???
  
  def readLastInt30(offset: Long): Int = readInt32(offset) >>> 2
  
  def writeLastInt30(offset: Long, v: Int): this.type = ???
}

class DefaultByteBuf extends ByteBuf {
  private def writeFail() = throw new RuntimeException("Writes not allowed")
  
  def readBool(offset: Long): Boolean = false
  def writeBool(offset: Long, v: Boolean): this.type = writeFail()
  def readInt8(offset: Long): Byte = 0
  def writeInt8(offset: Long, v: Byte): this.type = writeFail()
  def readInt16(offset: Long): Short = 0
  def writeInt16(offset: Long, v: Short): this.type = writeFail()
  def readInt32(offset: Long): Int = 0
  def writeInt32(offset: Long, v: Int): this.type = writeFail()
  def readInt64(offset: Long): Long = 0
  def writeInt64(offset: Long, v: Long): this.type = writeFail()
  def readUInt8(offset: Long): Short = 0
  def writeUInt8(offset: Long, v: Short): this.type = writeFail()
  def readUInt16(offset: Long): Int = 0
  def writeUInt16(offset: Long, v: Int): this.type = writeFail()
  def readUInt32(offset: Long): Long = 0
  def writeUInt32(offset: Long, v: Long): this.type = writeFail()
  def readUInt64(offset: Long): BigInt = 0
  def writeUInt64(offset: Long, v: BigInt): this.type = writeFail()
  def readFloat32(offset: Long): Float = 0
  def writeFloat32(offset: Long, v: Float): this.type = writeFail()
  def readFloat64(offset: Long): Double = 0
  def writeFloat64(offset: Long, v: Double): this.type = writeFail()
  def slice(offset: Long): ByteBuf = this
  def slice(offset: Long, size: Long): ByteBuf = this
  
  // Special
  def readFirstUInt2(offset: Long): Byte = 0
  def writeFirstUInt2(offset: Long, v: Byte): this.type = writeFail()
  def readFirstUInt3(offset: Long): Byte = 0
  def writeFirstUInt3(offset: Long, v: Byte): this.type = writeFail()
  def readLastUInt29(offset: Long): Long = 0
  def writeLastUInt29(offset: Long, v: Long): this.type = writeFail()
  def readLastInt30(offset: Long): Int = 0
  def writeLastInt30(offset: Long, v: Int): this.type = writeFail()
}