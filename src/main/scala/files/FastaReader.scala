package files

import hgvs.Utils
import scala.io.Source

object FastaReader {
  val faHg38 = "reference/hg38/hg38.fa"
  val faT2T = "reference/t2t/chm13v2.0.fa"
  val fastaCache = scala.collection.mutable.Map[String, String]()
  var NCBILoaded = ""
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