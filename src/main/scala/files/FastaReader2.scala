package files

import scala.io.Source

object FastaReader2 {
  // File paths to reference genome FASTA files
  val faHg38 = "reference/hg38/GRCh38.primary_assembly.genome.fa"
  val faT2T = "reference/t2t/chm13v2.0.fa"

  // Iterator for the FASTA file
  private var source: Option[Source] = None
  private var iterator: Iterator[String] = Iterator.empty
  private var currentContig = ""
  private var sequenceBuffer = new StringBuilder
  private var fastaCache = scala.collection.mutable.Map[String, String]()

  // Tracks the currently loaded genome build
  var NCBILoaded = ""

  // Window size for loading data in chunks (similar to the batch size in your GFF example)
  val windowSize = 1000000  // Adjust based on your system's memory capacity

  /**
   * Loads the FASTA file for the specified genome build and initializes the iterator.
   *
   * @param NCBIBuild The genome build to load (e.g., "hg38" or "t2t").
   */
  def loadFastaFile(NCBIBuild: String): Unit = {
    // Only load if the build is different from the currently loaded one
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
   *
   * @param contig The contig (chromosome) to load data for.
   * @param start  The start position in the contig.
   * @param end    The end position in the contig.
   */
  def loadNextBatch(contig: String, start: Int, end: Int): Unit = {
    // Ensure we're working with the right contig
    if (currentContig != contig) {
      // If the contig has changed, reset buffers
      currentContig = contig
      sequenceBuffer.clear()
      fastaCache.clear()
    }

    // Load the required window (chunk) of sequence data
    while (iterator.hasNext && sequenceBuffer.length < (end + 1)) {
      val line = iterator.next()
      if (line.startsWith(">")) {
        // If we encounter a new contig, stop loading for the current one
        if (currentContig.nonEmpty && sequenceBuffer.nonEmpty) {
          fastaCache(currentContig) = sequenceBuffer.toString
        }
        //println(s"Found contig header: $line")
        // Normalize the contig name (remove any spaces)
        currentContig = line.split(" ")(0)

        sequenceBuffer.clear()
      } else {
        sequenceBuffer.append(line.trim)
      }
    }

    // Cache the loaded contig's sequence data
    if (currentContig.nonEmpty && sequenceBuffer.nonEmpty) {
      fastaCache(currentContig) = sequenceBuffer.toString
    }
  }

  /**
   * Retrieves a specific sequence from the loaded reference genome.
   *
   * @param contig     The contig (chromosome) to extract the sequence from.
   * @param start      The start position of the sequence.
   * @param end        The end position of the sequence.
   * @param strandPlus A boolean indicating whether to return the sequence as-is or its reverse complement.
   * @return The extracted sequence as a string.
   */
  def getSequence(NCBIBuild: String, contig: String, start: Int, end: Int, strandPlus: Boolean): String = {
    // Ensure the genome build is loaded before extracting the sequence
    loadFastaFile(NCBIBuild)

    // Retrieve the sequence from the cache
    var sequence = fastaCache.getOrElse(contig, "")

    // Keep loading more sequence data if the requested range is not covered yet
    while (sequence.isEmpty || sequence.length < (end + 1)) {
      //println(s"Sequence for contig $contig is incomplete or empty. Loading more data...")
      loadNextBatch(contig, start, end)
      sequence = fastaCache.getOrElse(contig, "")
    }

    // If the sequence is still empty or the length is still not sufficient, throw an error
    if (sequence.isEmpty) {
      throw new IllegalArgumentException(s"Contig $contig not found in the FASTA file.")
    }

    // Ensure the requested range is valid
    if (start < 1 || end > sequence.length) {
      throw new IllegalArgumentException(s"Requested range [$start, $end) is out of bounds for contig $contig with sequence length ${sequence.length}.")
    }
    // Extract the sub-sequence from the loaded data
    val subSequence = sequence.substring(start - 1, end)
    if (strandPlus) subSequence else hgvs.Utils.reverseComplement(subSequence)
  }

  /**
   * Cleans up the window (sequence cache) to free up memory for the next set of data.
   *
   * @param contig The contig to clean up.
   */
  def cleanUpWindow(contig: String): Unit = {
    if (fastaCache.contains(contig)) {
      fastaCache -= contig
    }
  }

  /**
   * Closes the FASTA file source when done.
   */
  def close(): Unit = {
    source.foreach(_.close())
  }
}

