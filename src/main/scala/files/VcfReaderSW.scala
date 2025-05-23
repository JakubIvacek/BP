package files

import data.VCFEntry
import htsjdk.tribble.TribbleException

import scala.io.Source
import scala.collection.mutable
import java.io.*
import scala.util.Try
import java.util.zip.GZIPInputStream

object VcfReaderSW {
  var loadedEntries: mutable.Queue[VCFEntry] = mutable.Queue()
  private var source: Option[Source] = None
  private var iterator: Iterator[String] = Iterator.empty
  private val batchSize = 40000
  private val initialLoadSize = 80000
  private var loadedCount = 0

  /**
   * Loads a VCF file and initializes the iterator.
   *
   * @param filename The path to the VCF file.
   */
  def loadVcfFile(filename: String): Unit = {
    val file = new File(filename)

    // Check if the file is GZIP-compressed
    val inputStream: InputStream =
      if (filename.endsWith(".gz")) new GZIPInputStream(new FileInputStream(file))
      else new FileInputStream(file) // Regular text file
    loadedEntries = mutable.Queue()
    source = Some(Source.fromInputStream(inputStream))
    iterator = source.get.getLines().filterNot(_.startsWith("#"))
    loadNextBatch(initialLoadSize)
  }

  /**
   * Loads the next batch of variants into memory.
   *
   * @param count Number of variants to load.
   */
  private def loadNextBatch(count: Int): Unit = {
    var loaded = 0
    while (iterator.hasNext && loaded < count) {
      val line = iterator.next()
      try {
        val fields = line.split("\t", -1)
        if (fields.length < 10)
          throw new IllegalArgumentException(s"Expected ≥10 fields but got ${fields.length}")

        // parse fields, letting malformed numbers throw
        val chrom = fields(0)
        val pos = fields(1).toInt
        val id = fields(2)
        val ref = fields(3)
        val alt = fields(4)
        val qual = fields(5)
        val filter = fields(6)
        val info = fields(7)

        // create and enqueue
        val vcfEntry = VCFEntry(chrom, pos, id, ref, alt, qual, filter, info)
        loadedEntries.enqueue(vcfEntry)
        loaded += 1
        loadedCount += 1

      } catch {
        case e: NumberFormatException =>
          System.err.println(s"Skipping line due to invalid integer: ${e.getMessage}")
        case e: IllegalArgumentException =>
          System.err.println(s"Skipping line due to bad format: ${e.getMessage}")
        case e: TribbleException.InternalCodecException =>
          System.err.println(s"Skipping entry due to codec error: ${e.getMessage}")
      }
    }
    //println("Loaded new " + loaded)
  }

  /**
   * Ensures that the given variant is within the loaded window.
   * If not, it loads additional variants as needed.
   *
   * @param variantEnd The end position of the variant being annotated.
   */
  def ensureVariantInWindow(variantEnd: Int): Unit = {
    while (iterator.hasNext && (loadedEntries.isEmpty || loadedEntries.last.pos < variantEnd)) {
      loadNextBatch(batchSize)
      //println("Loaded : - " + loadedCount + s" End - ${loadedEntries.last.pos} Variant end - $variantEnd")
      cleanUpWindow(variantEnd)
    }

    cleanUpWindow(variantEnd)
  }

  /**
   * Removes variants that are no longer needed from the start of the queue.
   *
   * @param variantStart The start position of the variant being annotated.
   */
  private def cleanUpWindow(variantStart: Int): Unit = {
    var removed = 0
    while (loadedEntries.size > 1 && (loadedEntries(1).pos < variantStart)) {
      loadedEntries.dequeue()
      loadedCount -= 1
      removed += 1
    }

    if (removed > 0) {
      //println(s"Cleaned up $removed entries.")
    }
  }

  /**
   * Closes the VCF file source when done.
   */
  def close(): Unit = {
    source.foreach(_.close())
  }
}

