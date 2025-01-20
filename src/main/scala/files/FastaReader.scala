package files

import hgvs.Utils
import scala.io.Source

object FastaReader {
  val faHg38 = "reference/hg38/hg38.fa"
  val faT2T = "reference/t2t/chm13v2.0.fa"
  def getSequence(NCBIBuild: String, contig: String, start: Int, end: Int, strandPlus: Boolean): String = {
    val fastaFile = if NCBIBuild == "hg38" then faHg38 else faT2T
    val fastaLines = Source.fromFile(fastaFile).getLines()
    val sequenceBuffer = new StringBuilder
    var isReading = false

    // Extract sequence by matching the contig header
    for (line <- fastaLines) {
      if (line.startsWith(">")) {
        isReading = line.contains(contig)
      } else if (isReading) {
        sequenceBuffer.append(line.trim)
      }
    }

    // Get the sub-sequence and adjust for strand
    val subSequence = sequenceBuffer.toString.substring(start - 1, end) // 1-based indexing
    if (strandPlus) subSequence else Utils.reverseComplement(subSequence)
  }
}
