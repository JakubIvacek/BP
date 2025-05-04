package files

import data.GffEntry
import scala.io.Source
import scala.util.Try
import java.io.{File, FileInputStream, InputStream, PushbackInputStream}
import java.util.zip.GZIPInputStream

/**
 * The `GFFReader` object loads an entire GFF3 file into memory.
 */
object GFFReader {
  private var entries: Vector[GffEntry] = Vector.empty
  private var source: Option[Source] = None

  /**
   * Loads a GFF3 file (plain or gzip-compressed) into memory.
   *
   * @param filename Path to the GFF3 file.
   */
  def loadGffFile(filename: String): Unit = {
    val file = new File(filename)
    println(s"Loading entire GFF from: $filename")

    // Detect GZIP by peeking at the first two bytes
    val fis = new FileInputStream(file)
    val pb = new PushbackInputStream(fis, 2)
    val signature = new Array[Byte](2)
    val bytesRead = pb.read(signature)
    if (bytesRead > 0) pb.unread(signature, 0, bytesRead)

    val inputStream: InputStream =
      if (bytesRead == 2 && signature(0) == 0x1f.toByte && signature(1) == 0x8b.toByte) {
        new GZIPInputStream(pb)
      } else {
        pb
      }

    source = Some(Source.fromInputStream(inputStream))

    // Read all non-comment lines and parse into GffEntry
    entries = source.get.getLines()
      .filterNot(_.startsWith("#"))
      .flatMap(parseLine)
      .toVector
  }

  /**
   * Parses a single line of GFF3 text into an Option[GffEntry].
   */
  private def parseLine(line: String): Option[GffEntry] = {
    val fields = line.split("\t", -1)
    if (fields.length < 9) return None

    val contig = fields(0)
    val start = Try(fields(3).toInt).getOrElse(0)
    val end = Try(fields(4).toInt).getOrElse(0)
    val featureType = fields(2)
    val strandPlus = fields(6) == "+"
    val attributes = fields(8)
      .split(";")
      .map { attr =>
        val Array(key, value) = attr.split("=", 2) ++ Array("")
        key -> value
      }
      .toMap

    Some(GffEntry(contig, start, end, strandPlus, featureType, attributes))
  }

  /**
   * @return All loaded GFF entries.
   */
  def getEntries: Seq[GffEntry] = entries

  /**
   * Closes the underlying file source.
   */
  def close(): Unit = {
    source.foreach(_.close())
    source = None
  }
}
