package files

import hgvs.Utils
import scala.io.Source

/**
 * The object `FastaReader` handles the loading of reference FASTA files and
 * provides functionality to retrieve specific coding sequences (CDS) from the genome.
 */
object FastaReader {
  // File paths to reference genome FASTA files
  val faHg38 = "reference/hg38/hg38.fa" // Path to the hg38 reference genome
  val faT2T = "reference/t2t/chm13v2.0.fa" // Path to the T2T reference genome

  // Cache to store loaded sequences for faster access
  val fastaCache = scala.collection.mutable.Map[String, String]()
  // Tracks which genome build is currently loaded
  var NCBILoaded = ""

  /**
   * Loads the specified genome reference into memory.
   *
   * @param NCBIBuild The name of the genome build to load (e.g., "hg38" or "t2t").
   *                  If the requested genome build is already loaded, it skips reloading.
   */
  def loadSequence(NCBIBuild: String): Unit = {
    // Only load if cache is empty
    if (NCBILoaded != NCBIBuild) {
      val fastaFile = if (NCBIBuild == "hg38") faHg38 else faT2T
      val fastaLines = Source.fromFile(fastaFile).getLines()
      NCBILoaded = NCBIBuild
      var currentContig = ""
      var sequenceBuffer = new StringBuilder

      for (line <- fastaLines) {
        if (line.startsWith(">")) {
          // Cache the previous contig's sequence
          if (currentContig.nonEmpty) {
            fastaCache(currentContig) = sequenceBuffer.toString
          }
          currentContig = line
          sequenceBuffer.clear()
        } else {
          sequenceBuffer.append(line.trim)
        }
      }
      // Cache the last contig's sequence
      if (currentContig.nonEmpty) {
        fastaCache(currentContig) = sequenceBuffer.toString
      }
    }
  }

  /**
   * Retrieves a specific sequence from the loaded reference genome.
   *
   * @param NCBIBuild  The genome build to use (e.g., "hg38" or "t2t").
   * @param contig     The contig (chromosome) from which to extract the sequence.
   * @param start      Starting position of the sequence.
   * @param end        Ending position of the sequence.
   * @param strandPlus A boolean indicating whether to return the sequence as-is or its reverse complement
   * @return The extracted sequence as a string.
   * @throws IllegalArgumentException if the specified contig is not found in the FASTA file.
   */
  def getSequence(NCBIBuild: String, contig: String, start: Int, end: Int, strandPlus: Boolean): String = {
    // Load the sequence file if the cache is empty
    loadSequence(NCBIBuild)

    // Get the sequence from the cache
    val sequence = fastaCache.getOrElse(s">$contig", "")
    if (sequence.isEmpty) {
      throw new IllegalArgumentException(s"Contig $contig not found in the FASTA file.")
    }
    val subSequence = sequence.substring(start - 1, end)
    if (strandPlus) subSequence else Utils.reverseComplement(subSequence)
  }
}