package files

import scala.io.Source

object FastaReader2 {
  // File paths to reference genome FASTA files
  val faHg38 = "GRCh38.primary_assembly.genome.fa"
  val faT2T = "reference/t2t/chm13v2.0.fa"

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
  val windowSize = 1000000  // Adjust based on memory capacity

  /**
   * Loads the FASTA file for the specified genome build and initializes the iterator.
   *
   * @param NCBIBuild The genome build to load (e.g., "hg38" or "t2t").
   */
  def loadFastaFile(NCBIBuild: String): Unit = {
    if (NCBILoaded != NCBIBuild) {
      val fastaFile = NCBIBuild match {
        case "hg38" => faHg38
        case "t2t"  => faT2T
        case _ => throw new IllegalArgumentException(s"Unsupported genome build: $NCBIBuild")
      }

      // Load the FASTA file for the specified build
      source = Some(Source.fromFile(fastaFile))
      iterator = source.get.getLines()
      NCBILoaded = NCBIBuild
    }
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

    // Remove old sequence data to keep only what's needed
    if (start > currentStartPosition + windowSize) {
      val newStartIndex = start - currentStartPosition
      if (newStartIndex < sequenceBuffer.length) {
        sequenceBuffer.delete(0, newStartIndex)  // Trim older bases
      } else {
        sequenceBuffer.clear()
      }
      currentStartPosition = start
    }

    // Load more data if needed
    while (iterator.hasNext && sequenceBuffer.length < (end - currentStartPosition + 1)) {
      val line = iterator.next()
      if (line.startsWith(">")) {
        if (sequenceBuffer.nonEmpty) {
          fastaCache(currentContig) = (currentStartPosition, sequenceBuffer.toString)
        }
        currentContig = line.substring(1).split(" ")(0)  // Remove '>' and normalize contig name
        sequenceBuffer.clear()
      } else {
        sequenceBuffer.append(line.trim)
      }
    }
    //println("Loaded -> " + sequenceBuffer.length)
    // Store updated sequence in cache with its start position
    fastaCache(currentContig) = (currentStartPosition, sequenceBuffer.toString)
  }

  /**
   * Retrieves a specific sequence from the loaded reference genome.
   */
  def getSequence(NCBIBuild: String, contig: String, start: Int, end: Int, strandPlus: Boolean): String = {
    // Ensure the genome build is loaded
    loadFastaFile(NCBIBuild)

    // Retrieve cached sequence if available
    val (cachedStart, sequence) = fastaCache.getOrElse(contig, (0, ""))

    // If requested region isn't in cache, load more data
    if (sequence.isEmpty || start < cachedStart || end > cachedStart + sequence.length) {
      loadNextBatch(contig, start, end)
    }

    // Get the updated cached sequence
    val (newStart, newSequence) = fastaCache.getOrElse(contig, (0, ""))
    if (newSequence.isEmpty) {
      throw new IllegalArgumentException(s"Contig $contig not found in the FASTA file.")
    }

    // Ensure range is within bounds
    if (start < newStart || end > newStart + newSequence.length) {
      throw new IllegalArgumentException(s"Requested range [$start, $end) is out of bounds for contig $contig.")
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
