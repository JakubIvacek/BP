package files

import data.VCFEntry

import scala.io.Source
import scala.collection.mutable
import java.io.*
import scala.util.Try
import java.util.zip.GZIPInputStream

object VcfReaderSW {
  var loadedEntries: mutable.Queue[VCFEntry] = mutable.Queue()
  private var source: Option[Source] = None
  private var iterator: Iterator[String] = Iterator.empty
  private val batchSize = 20 
  private val initialLoadSize = 100 
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
      val fields = line.split("\t", -1)

      if (fields.length >= 10) { // Ensure it's a valid VCF line
        val chrom = fields(0)
        val pos = Try(fields(1).toInt).getOrElse(0)
        val id = fields(2)
        val ref = fields(3)
        val alt = fields(4)
        val qual = fields(5)
        val filter = fields(6)
        val info = fields(7)

        // Create a VCFEntry object
        val vcfEntry = VCFEntry(chrom, pos, id, ref, alt, qual, filter, info)
        loadedEntries.enqueue(vcfEntry)
        loaded += 1
        loadedCount += 1
      }
    }
  }

  /**
   * Ensures that the given variant is within the loaded window.
   * If not, it loads additional variants as needed.
   *
   * @param variantEnd The end position of the variant being annotated.
   * @param variantChrom The chromosome of the variant being annotated.
   */
  def ensureVariantInWindow(variantEnd: Int, variantChrom: String): Unit = {
    while (iterator.hasNext && (loadedEntries.isEmpty || loadedEntries.last.chrom < variantChrom || (loadedEntries.last.chrom == variantChrom && loadedEntries.last.pos < variantEnd))) {
      loadNextBatch(batchSize)

      // Stop if we reach a new chromosome
      if (loadedEntries.nonEmpty && loadedEntries.last.chrom != variantChrom && loadedEntries.last.chrom > variantChrom) {
        //println(s"Stopped loading because chromosome changed to ${loadedEntries.last.chrom}")
        return
      }

      cleanUpWindow(variantEnd, variantChrom)
    }

    cleanUpWindow(variantEnd, variantChrom)
  }

  /**
   * Removes variants that are no longer needed from the start of the queue.
   *
   * @param variantStart The start position of the variant being annotated.
   */
  private def cleanUpWindow(variantStart: Int, variantChrom: String): Unit = {
    while (loadedEntries.size > 1 && (loadedEntries(1).pos < variantStart || loadedEntries(1).chrom != variantChrom)) {
      loadedEntries.dequeue()
      loadedCount -= 1
    }
  }

  /**
   * Closes the VCF file source when done.
   */
  def close(): Unit = {
    source.foreach(_.close())
  }
}

