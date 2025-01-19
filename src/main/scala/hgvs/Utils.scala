package hgvs
import data.GffEntry

object Utils {
  def calculateTranscriptPosition(genomicPos: BigInt, transcript: GffEntry): String = {
    val relativePos = (genomicPos - transcript.start + 1).toInt
    relativePos.toString
  }

  def calculateProteinPosition(genomicPos: BigInt, cds: GffEntry): Int = {
    // Calculate the relative position within the CDS (1-based index)
    val relativePos = (genomicPos - cds.start + 1).toInt
    if (relativePos <= 0 || relativePos > cds.end - cds.start + 1) {
      throw new IllegalArgumentException("Genomic position is out of bounds for the CDS region")
    }
    // Determine the protein position: each codon is 3 bases
    val proteinPosition = (relativePos - 1) / 3 + 1

    proteinPosition
  }

  def reverseComplement(sequence: String): String = {
    sequence.reverse.map {
      case 'A' => 'T'
      case 'T' => 'A'
      case 'C' => 'G'
      case 'G' => 'C'
      case other => other
    }
  }
}
