package alife.persistence

import alife.Config

import java.nio.file.{Files, Path}
import scala.annotation.tailrec

trait PersistorFactory:
  def connect(config: Config): Persistor

object PersistorFactory:
  object Dummy extends PersistorFactory:
    override def connect(config: Config): Persistor =
      val alias = config
      new Persistor:
        override def config: Config = alias
        override def close(): Unit = ()
        override def isWritable: Boolean = true
        override def hasMoreIterations: Boolean = false
        override def startIteration(): Unit = ()
        override def finishIteration(): Unit = ()
        override def hasNextIndividualAction: Boolean = false
        override def nextIndividualAction: Int = throw IllegalStateException("No next individual action")
        override def writeIndividualAction(action: Int): Unit = ()

  class SingleFileFactory(file: Path, mode: SingleSourcePersistor.Mode) extends PersistorFactory:
    override def connect(config: Config): Persistor =
      SingleSourcePersistor.forFile(config, file, mode)

  class ChecksumBasedFileFactory(root: Path, mode: SingleSourcePersistor.Mode) extends PersistorFactory:
    if !Files.exists(root) then Files.createDirectory(root)
    if !Files.isDirectory(root) then throw IllegalArgumentException("The root path is not a directory")

    override def connect(config: Config): Persistor =
      val checksumString = config.checksum
      val filename = s"$checksumString.alf"
      @tailrec
      def tryLevel(localRoot: Path, depth: Int, maxDepth: Int): Persistor =
        val candidate = localRoot.resolve(filename)
        if Files.exists(candidate) then
          SingleSourcePersistor.forFile(config, candidate, mode)
        else
          val substring = checksumString.substring(2 * depth, 2 * depth + 2)
          val maybeDir = localRoot.resolve(substring)
          if Files.exists(maybeDir) then
            if !Files.isDirectory(maybeDir) then throw IllegalStateException("Storage error: a two-letter subdirectory is not a directory")
            tryLevel(maybeDir, depth + 1, maxDepth)
          else if depth < maxDepth then
            Files.createDirectory(maybeDir)
            tryLevel(maybeDir, depth + 1, maxDepth)
          else SingleSourcePersistor.forFile(config, candidate, mode)
      end tryLevel
      tryLevel(root, 0, 2)
