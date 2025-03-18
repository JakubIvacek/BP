package files

import data.GffEntry

import scala.io.Source
import scala.util.Try
import scala.collection.mutable
import java.io.{FileInputStream, InputStream, File}
import java.util.zip.GZIPInputStream

/**
 * The `GFFReader2` object implements a sliding window approach for loading GFF3 files dynamically.
 */
object GFFReader2 {
  var loadedEntries: mutable.Queue[GffEntry] = mutable.Queue()
  private var source: Option[Source] = None
  private var iterator: Iterator[String] = Iterator.empty
  private val batchSize = 20 // 20 whole genes
  private val initialLoadSize = 100 // 100 whole genes
  private var loadedCount = 0
  /**
   * Loads a GFF3 file and initializes the iterator.
   *
   * @param filename The path to the GFF3 file.
   */
  def loadGffFile(filename: String): Unit = {
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
   * Loads the next batch of genes into memory.
   *
   * @param count Number of genes to load.
   */
  def loadNextBatch(count: Int): Unit = {
    var loaded = 0
    while (iterator.hasNext && loaded < count) {
      val line = iterator.next()
      val fields = line.split("\t", -1)

      if (fields.length >= 9) {
        val contig = fields(0)
        val start = Try(fields(3).toInt).getOrElse(0)
        val end = Try(fields(4).toInt).getOrElse(0)
        val name = fields(2)
        val strandPlus = fields(6) == "+"
        val attributes = fields(8).split(";").map { attr =>
          val keyValue = attr.split("=")
          keyValue.head -> keyValue.lift(1).getOrElse("")
        }.toMap

        // If it's a "gene"
        if (name == "gene") {
          val gene = GffEntry(contig, start, end, strandPlus, name, attributes)
          loadedEntries.enqueue(gene)
          loaded += 1
          loadedCount += 1
        } else {
          // If it's not a "gene"
          val feature = GffEntry(contig, start, end, strandPlus, name, attributes)
          loadedEntries.enqueue(feature)
          loadedCount += 1
        }
      }
    }
  }

  /**
   * Ensures that the given variant is within the loaded window.
   * If not, it loads additional genes as needed.
   *
   * @param variantEnd The end position of the variant.
   */
  def ensureVariantInWindow(variantEnd: Int, variantContig: String): Unit = {
    while (iterator.hasNext && (loadedEntries.isEmpty || loadedEntries.last.contig < variantContig || (loadedEntries.last.contig == variantContig && loadedEntries.last.end < variantEnd))
    ) {
      loadNextBatch(batchSize)

      // Stop if we reach a new contig
      if (loadedEntries.nonEmpty && loadedEntries.last.contig != variantContig && loadedEntries.last.contig > variantContig) {
        println(s"Stopped loading because contig changed to ${loadedEntries.last.contig}")
        return
      }

      cleanUpWindow(variantEnd, variantContig)
      //println(s"$variantContig $variantEnd new load length - ${loadedEntries.length} start - ${loadedEntries.head.contig} - ${loadedEntries.head.start} , end - ${loadedEntries.last.contig} ${loadedEntries.last.end}")
    }

    cleanUpWindow(variantEnd, variantContig)
  }



  /**
   * Removes genes that are no longer needed from the start of the queue.
   *
   * @param variantStart The start position of the variant.
   */
  private def cleanUpWindow(variantStart: Int, variantContig: String): Unit = {
    while (loadedEntries.size > 1 && (loadedEntries(1).end < variantStart || loadedEntries(1).contig != variantContig)) {
      loadedEntries.dequeue()
      loadedCount -= 1
    }
  }

  /**
   * Closes the GFF3 file source when done.
   */
  def close(): Unit = {
    source.foreach(_.close())
  }

  /**
   * Retrieves the exons for a specific transcript ID.
   *
   * @param transcriptId The transcript ID for which to find exons.
   * @return A list of exons for the given transcript ID.
   */
  def getExonsForTranscriptId(transcriptId: String): List[GffEntry] = {
    // Filter the loaded GFF entries to find those with feature "exon" and matching transcriptId
    loadedEntries.filter(entry =>
        entry.attributes.getOrElse("transcript_id", "") == transcriptId && 
          (entry.name == "exon" || entry.name == "CDS" || entry.name == "start_codon" || entry.name == "stop_codon")
    ).toList
  }
}

