package alife.persistence

import alife.Config
import alife.persistence.SingleSourcePersistor.*

import java.io.ByteArrayInputStream
import java.nio.ByteBuffer
import java.nio.file.{Files, Path, StandardOpenOption}
import java.util.Properties

class SingleSourcePersistor private (val config: Config, access: Accessor, mode: Mode) extends Persistor:
  require(config.actionSequence.size < 255, "Too many actions to fit one byte")
  
  if !access.hasMoreBytes then mode match
    case Mode.Read => throw IllegalArgumentException("Header missing when mode = Read")
    case Mode.Validate => throw IllegalArgumentException("Header missing when mode = Validate")
    case Mode.Append => access.writeLong(magicBytes)
  else
    val header = access.readLong()
    if header != magicBytes then throw IllegalArgumentException("Header magic bytes are incorrect")
  end if
  
  if !access.hasMoreBytes then mode match
    case Mode.Read => throw IllegalArgumentException("Config missing when mode = Read")
    case Mode.Validate => throw IllegalArgumentException("Config missing when mode = Validate")
    case Mode.Append =>
      val bytes = config.toByteArray
      access.writeInt(bytes.length)
      access.writeBytes(bytes, 0, bytes.length)
  else
    val configBytesSize = access.readInt()
    val configBytes = Array.ofDim[Byte](configBytesSize)
    access.readBytes(configBytes, 0, configBytesSize)
    val properties = Properties()
    properties.load(ByteArrayInputStream(configBytes))
    val inStorageConfig = Config.parse(properties, forceCheckIntegrity = true)
    if inStorageConfig != config then throw IllegalArgumentException("The in-storage config does not match the specified config")
  end if
  
  private var outIterationBuffer, currentIteration, nextIteration: Array[Byte] = Array.ofDim[Byte](16)
  private var outIterationIndex: Int = 0

  private var currentActionIndex, currentIterationSize, nextIterationSize: Int = 0
  private var nextIterationAttempted: Boolean = false
  private var currentIterationIsRead: Boolean = false
  
  private def byteToAction(byte: Byte): Int = (byte & 0xFF) - 1
  private def actionToByte(action: Int): Byte = (action + 1).toByte
  
  override def isWritable: Boolean = mode == Mode.Append
  
  override def hasMoreIterations: Boolean =
    if !nextIterationAttempted then
      nextIterationAttempted = true
      if access.hasMoreBytes then
        nextIterationSize = access.readInt()
        nextIteration = ensureCapacity(nextIteration, nextIterationSize)
        access.readBytes(nextIteration, 0, nextIterationSize)
      else nextIterationSize = -1
    nextIterationSize >= 0
  
  override def startIteration(): Unit =
    if hasMoreIterations then
      nextIterationAttempted = false
      val tmpBuf = currentIteration
      currentIteration = nextIteration
      nextIteration = tmpBuf
      val tmpBufSize = currentIterationSize
      currentIterationSize = nextIterationSize
      nextIterationSize = tmpBufSize
      currentIterationIsRead = true
      currentActionIndex = 0
    else mode match
      case Mode.Read => throw IllegalStateException("Attempt to start a non-existing iteration when mode = Read")
      case Mode.Validate => throw IllegalStateException("Attempt to start a non-existing iteration when mode = Validate")
      case Mode.Append =>
        outIterationIndex = 0
        currentIterationIsRead = false
    end if
  
  override def finishIteration(): Unit =
    if !currentIterationIsRead then
      assert(mode == Mode.Append)
      assert(!access.hasMoreBytes)
      access.writeInt(outIterationIndex)
      access.writeBytes(outIterationBuffer, 0, outIterationIndex)
    end if
  
  override def hasNextIndividualAction: Boolean = mode match
    case Mode.Read =>
      // in Read, we allow reading actions
      assert(currentIterationIsRead)
      currentActionIndex < currentIterationSize
    case Mode.Append =>
      // in Append, we allow reading actions when there are any
      if currentIterationIsRead
      then currentActionIndex < currentIterationSize
      else false
    case Mode.Validate =>
      // in Validate, we don't allow reading actions
      assert(currentIterationIsRead)
      false
  
  override def nextIndividualAction: Int =
    assert(hasNextIndividualAction)
    val result = byteToAction(currentIteration(currentActionIndex))
    currentActionIndex += 1
    result
  
  override def writeIndividualAction(action: Int): Unit = mode match
    case Mode.Read => throw IllegalStateException("Cannot write an action when mode = Read")
    case Mode.Append =>
      if currentIterationIsRead
      then throw IllegalStateException("Cannot overwrite an action in the existing frame when mode = Append")
      else
        outIterationBuffer = ensureCapacity(outIterationBuffer, outIterationIndex + 1)
        outIterationBuffer(outIterationIndex) = actionToByte(action)
        outIterationIndex += 1
      end if
    case Mode.Validate =>
      assert(currentIterationIsRead)
      assert(currentActionIndex < currentIterationSize)
      val expectedAction = byteToAction(currentIteration(currentActionIndex))
      currentActionIndex += 1
      if expectedAction != action then throw IllegalArgumentException(s"Expected action $expectedAction, found $action")
  
  override def close(): Unit = access.close()

object SingleSourcePersistor:
  private val magicBytes: Long =
    val buffer = ByteBuffer.allocate(8)
    buffer.put("ALIFEv1\n".getBytes)
    buffer.getLong(0)
  
  private trait Accessor:
    def hasMoreBytes: Boolean
    def readInt(): Int
    def readLong(): Long
    def writeInt(value: Int): Unit
    def writeLong(value: Long): Unit
    def readBytes(destination: Array[Byte], offset: Int, length: Int): Unit
    def writeBytes(source: Array[Byte], offset: Int, length: Int): Unit
    def close(): Unit
  
  private def ensureCapacity(array: Array[Byte], minLength: Int): Array[Byte] =
    if array.length >= minLength then array else
      var nextLength = minLength
      if array.length < Int.MaxValue / 2
      then nextLength = math.max(nextLength, 2 * array.length)
      else nextLength = Int.MaxValue
      java.util.Arrays.copyOf(array, nextLength)
  
  enum Mode:
    case Read, Append, Validate

  def forByteBuffer(config: Config, buffer: ByteBuffer, forValidation: Boolean): Persistor =
    val accessor = new Accessor:
      override def hasMoreBytes: Boolean = buffer.position() < buffer.capacity()
      override def readInt(): Int = buffer.getInt
      override def readLong(): Long = buffer.getLong
      override def writeInt(value: Int): Unit = throw UnsupportedOperationException("This accessor is read-only")
      override def writeLong(value: Long): Unit = throw UnsupportedOperationException("This accessor is read-only")
      override def readBytes(destination: Array[Byte], offset: Int, length: Int): Unit =
        buffer.get(destination, offset, length)
      override def writeBytes(source: Array[Byte], offset: Int, length: Int): Unit =
        throw UnsupportedOperationException("This accessor is read-only")
      override def close(): Unit = ()
    end accessor
    SingleSourcePersistor(config, accessor, if forValidation then Mode.Validate else Mode.Read)
  
  def forByteArray(config: Config, array: Array[Byte], forValidation: Boolean): Persistor =
    forByteBuffer(config, ByteBuffer.wrap(array), forValidation)

  def forFile(config: Config, file: Path, mode: Mode): Persistor =
    val optionSet = java.util.HashSet[StandardOpenOption]()
    optionSet.add(StandardOpenOption.READ)
    if mode == Mode.Append then
      optionSet.add(StandardOpenOption.CREATE)
      optionSet.add(StandardOpenOption.WRITE)
    val channel = Files.newByteChannel(file, optionSet)
    val accessor = new Accessor:
      private val buffer4 = ByteBuffer.allocate(4)
      private val buffer8 = ByteBuffer.allocate(8)
      override def hasMoreBytes: Boolean = channel.position() < channel.size()
      override def readInt(): Int =
        buffer4.clear()
        val nBytes = channel.read(buffer4)
        require(nBytes == 4, s"nBytes = $nBytes != 4")
        buffer4.getInt(0)
      
      override def readLong(): Long =
        buffer8.clear()
        val nBytes = channel.read(buffer8)
        require(nBytes == 8, s"nBytes = $nBytes != 8")
        buffer8.getLong(0)
      
      override def writeInt(value: Int): Unit =
        buffer4.clear().putInt(value).position(0)
        val nBytes = channel.write(buffer4)
        require(nBytes == 4, s"nBytes = $nBytes != 4")
      
      override def writeLong(value: Long): Unit =
        buffer8.clear().putLong(value).position(0)
        val nBytes = channel.write(buffer8)
        require(nBytes == 8, s"nBytes = $nBytes != 8")
      
      override def readBytes(destination: Array[Byte], offset: Int, length: Int): Unit =
        require(channel.read(ByteBuffer.wrap(destination, offset, length)) == length)
      
      override def writeBytes(source: Array[Byte], offset: Int, length: Int): Unit =
        require(channel.write(ByteBuffer.wrap(source, offset, length)) == length)
      
      override def close(): Unit = channel.close()
    end accessor
    SingleSourcePersistor(config, accessor, mode)
