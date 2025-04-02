package files

import scala.io.Source
import java.io.{File, FileInputStream, InputStream}
import java.util.zip.GZIPInputStream

object FastaReaderSW {
  // File paths to reference genome FASTA files
  //val faHg38 = "GRCh38.primary_assembly.genome.fa"
  //val faT2T = "reference/t2t/chm13v2.0.fa"

  // Iterator for the FASTA file
  private var source: Option[Source] = None
  private var iterator: Iterator[String] = Iterator.empty
  private var currentContig = ""
  private var sequenceBuffer = new StringBuilder
  private var fastaCache = scala.collection.mutable.Map[String, (Int, String)]() // Tracks start position

  // Tracks the currently loaded genome build
  var NCBILoaded = ""
  private var currentStartPosition = 1 // Track the first position in sequenceBuffer

  // Window size for loading data in chunks
  val windowSize = 2000000  // Adjust based on memory capacity
  var faPathLoaded = ""
  /**
   * Loads the FASTA file for the specified genome build and initializes the iterator.
   *
   * @param fastaPath Path to .fa file
   */

  /**
   * Loads the FASTA file for the specified genome build and initializes the iterator.
   *
   * @param fastaPath Path to .fa or .fa.gz file
   */
  def loadFastaFile(fastaPath: String): Unit = {
    if (faPathLoaded != fastaPath) {
      faPathLoaded = fastaPath

      // Determine if the file is GZIP-compressed
      val file = new File(fastaPath)
      val inputStream: InputStream =
        if (fastaPath.endsWith(".gz")) new GZIPInputStream(new FileInputStream(file))
        else new FileInputStream(file) // Regular text file

      source = Some(Source.fromInputStream(inputStream))
      iterator = source.get.getLines()
    }
  }
  /**
   * Reloads the FASTA file for the specified genome build and initializes the iterator.
   *
   */
  def reloadFastaFile(): Unit = {
    source = Some(Source.fromFile(faPathLoaded))
    iterator = source.get.getLines()
  }
  /**
   * Loads the next batch of the FASTA sequence into memory.
   */
  def loadNextBatch(contig: String, start: Int, end: Int): Unit = {
    if (currentContig != contig) {
      // Reset everything when changing contigs
      cleanUpWindow(currentContig)
      currentContig = contig
      sequenceBuffer.clear()
      fastaCache.clear()
      currentStartPosition = 1
    }

    //val keepBefore = 50000 ??

    // Remove old sequence data
    if (start > currentStartPosition) {
      val newStart = math.max(currentStartPosition, Math.max(0,start - 1000000))
      val newStartIndex = newStart - currentStartPosition

      if (newStartIndex > 0) {
        if (newStartIndex < sequenceBuffer.length) {
          sequenceBuffer.delete(0, newStartIndex) // Trim older bases but keep `keepBefore`
        } else {
          sequenceBuffer.clear()
        }
        currentStartPosition = newStart
      }
    }

    // Load more data if needed
    var contigFound = currentContig == contig

    while (iterator.hasNext && (!contigFound || sequenceBuffer.length < (end - currentStartPosition + 1))) {
      val line = iterator.next()
      if (line.startsWith(">")) {
        if (sequenceBuffer.nonEmpty && currentContig == contig) {
          fastaCache(currentContig) = (currentStartPosition, sequenceBuffer.toString)
        }
        val newContig = line.substring(1).split(" ")(0)
        if (newContig == contig) {
          currentContig = newContig
          sequenceBuffer.clear()
          currentStartPosition = 1
          contigFound = true
        } else {
          currentContig = newContig
          sequenceBuffer.clear()
        }
      } else if (contigFound) {
        sequenceBuffer.append(line.trim)
      }
    }
    // If the iterator has reached the end of the file, reload it
    if (!iterator.hasNext) {
       reloadFastaFile()
    }
    //println("Loaded -> " + sequenceBuffer.length)
    // Store updated sequence in cache with its start position
    fastaCache(currentContig) = (currentStartPosition, sequenceBuffer.toString)
  }

  
  /**
   * Retrieves a specific sequence from the loaded reference genome.
   */
  def getSequence(faPath: String, contig: String, start: Int, end: Int, strandPlus: Boolean): String = {
    // Ensure the genome build is loaded
    loadFastaFile(faPath)

    // Retrieve cached sequence if available
    val (cachedStart, sequence) = fastaCache.getOrElse(contig, (0, ""))

    // If requested region isn't in cache, load more data
    if (sequence.isEmpty || end > cachedStart + sequence.length) {
      loadNextBatch(contig, start, end)
    }
    // Get the updated cached sequence
    val (newStart, newSequence) = fastaCache.getOrElse(contig, {
      // Try force-loading again if not found yet
      loadNextBatch(contig, start, end)
      fastaCache.getOrElse(contig, (0, ""))
    })

    if (newSequence.isEmpty) {
      throw new IllegalArgumentException(s"Contig $contig not found in the FASTA file.")
    }


    // Ensure range is within bounds
    if (start < newStart) {
      //println(s"Requested start [$start) <  $newStart  - $contig.")
      return ""
    }
    if(end > newStart + newSequence.length){
      //println(s"Requested range [$end) > ${newStart + newSequence.length} - $contig.")
      return ""
    }

    // Extract the requested subsequence
    val subSequence = newSequence.substring(start - newStart, end - newStart)
    if (strandPlus) subSequence else hgvs.Utils.reverseComplement(subSequence)
  }

  /**
   * Cleans up the window (sequence cache) to free up memory.
   */
  def cleanUpWindow(contig: String): Unit = {
    fastaCache.remove(contig)
  }

  /**
   * Closes the FASTA file source.
   */
  def close(): Unit = {
    source.foreach(_.close())
  }
}
