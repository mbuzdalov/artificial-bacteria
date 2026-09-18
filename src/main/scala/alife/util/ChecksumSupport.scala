package alife.util

import alife.util.Loops.loopFromUntil

import java.security.MessageDigest

/**
 * Various utilities for checksums.
 */
object ChecksumSupport:
  /**
   * A type for a digest result, which can interpreted either as an array of bytes, or as a hexadecimal string.
   * Internally, this is an opaque alias for an array of bytes.
   */
  opaque type DigestResult = Array[Byte]
  
  extension (digest: DigestResult)
    /**
     * Returns a byte array representation of a digest result.
     * @return the byte array representation.
     */
    def asByteArray: Array[Byte] = digest
    /**
     * Returns a lowercase-hexadecimal string representation of a digest result.
     * Example: 6d1937d2748e41d14f6debce0510fce2.
     * @return a lowercase-hexadecimal string representation.
     */
    def asString: String =
      val destination = StringBuilder()
      inline def appendNybble(byte: Int): Unit = destination.append:
        if byte < 10 then ('0' + byte).toChar else ('a' + byte - 10).toChar
      loopFromUntil(0, digest.length): i => 
        appendNybble((digest(i) >>> 4) & 0x0F)
        appendNybble(digest(i) & 0x0F)
      destination.result()
  
  /**
   * Computes an MD5 sum of the given array of bytes.
   * @param bytes the bytes to compute the sum for.
   * @return the digest result.
   */ 
  def md5sum(bytes: Array[Byte]): DigestResult =
    MessageDigest.getInstance("MD5").digest(bytes)
