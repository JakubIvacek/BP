package hgvs
import data.{GffEntry, DnaVariant}
import files.GFFReader

object Utils {
  def calculateTranscriptPosition(genomicPos: BigInt, transcript: GffEntry): String = {
    val relativePos = if (transcript.strandPlus) {
      (genomicPos - transcript.start + 1).toInt
    } else {
      (transcript.end - genomicPos + 1).toInt
    }

    relativePos.toString
  }

  /**
   * Calculates the codon position in the CDS based on the genomic position.
   *
   * @param genomicPosition The genomic position of the variant.
   * @param cds             The CDS entry from the GFF file.
   * @return The codon position (0-based) within the CDS.
   */
  def calculateCodonPosition(genomicPosition: Long, cds: GffEntry): Int = {
    if (cds.strandPlus) {
      // Positive strand: Subtract the CDS start and divide by 3
      ((genomicPosition - cds.start) / 3).toInt
    } else {
      // Negative strand: Subtract the genomic position from CDS end and divide by 3
      ((cds.end - genomicPosition) / 3).toInt
    }
  }

  /**
   * Retrieves the codon sequence from the CDS for the specified codon position.
   *
   * @param codonPosition The 0-based codon position in the CDS.
   * @param cds           The CDS entry from the GFF file.
   * @param sequence      The nucleotide sequence of the CDS.
   * @return The codon sequence (3 nucleotides), or "NNN" if the position is invalid.
   */
  def getCodonAtPosition(codonPosition: Int, cds: GffEntry, sequence: String): String = {
    val start = codonPosition * 3
    val end = start + 3

    if (start >= 0 && end <= sequence.length) {
      val codon = sequence.substring(start, end)
      if (cds.strandPlus) codon else reverseComplement(codon)
    } else {
      "NNN"
    }
  }

  def calculateProteinPosition(genomicPos: BigInt, cds: GffEntry): Int = {
    // Calculate the relative position within the CDS (1-based index)
    val relativePos = (genomicPos - cds.start + 1).toInt

    // Ensure the position is within the CDS boundaries
    if (relativePos <= 0 || relativePos > cds.end - cds.start + 1) {
      return 0
    }

    // Determine the protein position: each codon consists of 3 bases
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

  /**
   * Helper method to find exon pair surrounding the variant
   */
  def findExonPairForVariant(variant: DnaVariant, exons: Seq[GffEntry]): Option[(GffEntry, GffEntry)] = {
    exons.sliding(2).collectFirst {
      case Seq(exon1, exon2) if variant.position > exon1.end && variant.position < exon2.start =>
        (exon1, exon2)
    }
  }

  /**
   * Returns the transcript-relative position of the variant.
   * This checks whether the variant is in an intron, or UTR region.
   * @return The transcript-relative position as a string, or "?" if the position cannot be determined.
   *         - Returns numerical position for exonic locations.
   *         - "pos-off" "pos+off" for intronic variants
   *         - "-X" for 5' UTR.
   *         - "*X" for 3' UTR.
   */
  def getTranscriptPosition(variant: DnaVariant, contig: String, transcriptId: String): String = {
    val transExons = GFFReader.getIntervalTree(contig).getExonsForTranscriptId(contig, transcriptId)
    val sortedExons = transExons.sortBy(_.start)

    if (sortedExons.isEmpty) {
      return "?"
    }

    val isStrandPlus = sortedExons.head.strandPlus
    if (sortedExons.size == 1) {
      val exon = sortedExons.head
      if (isStrandPlus) {
        // UTR handling for positive strand
        if (variant.position < exon.start) {
          val offset = sortedExons.head.start - variant.position
          s"-${offset}" // UTR 5 before first exon
        } else if (variant.position > sortedExons.last.end) {
          val offset = variant.position - sortedExons.last.end
          s"*${offset}" // UTR  3 after last exon
        } else {
          (variant.position - exon.start + 1).toString // Inside exon
        }
      } else {
        // UTR handling for negative strand
        if (variant.position > exon.end) {
          val offset = variant.position - sortedExons.last.end
          s"*${offset}" // UTR after last exon
        } else if (variant.position < sortedExons.head.start) {
          val offset = sortedExons.head.start - variant.position
          s"-${offset}" // UTR before first exon
        } else {
          (exon.end - variant.position + 1).toString // Inside exon
        }
      }
    } else {
      // Check if the variant is intron
      val relativePosition = sortedExons.zip(sortedExons.tail).collectFirst {
        case (exon1, exon2) if variant.position > exon1.end && variant.position < exon2.start =>
          // Calculate the offset from the exon boundary
          val offset = variant.position - exon1.end
          // Determine the position string based on the strand direction
          val position = if (isStrandPlus) {
            s"${exon1.end}+${offset}"  // After the first exon on positive strand
          } else {
            s"${exon2.start}-${offset}" // Before the second exon on negative strand
          }
          position
      }

      relativePosition.getOrElse {
        if (isStrandPlus) {
          // Handling for 5' and 3' UTR on positive strand
          if (variant.position < sortedExons.head.start) {
            val offset = sortedExons.head.start - variant.position
            s"-${offset}" // UTR 5 before first exon
          } else if (variant.position > sortedExons.last.end) {
            val offset = variant.position - sortedExons.last.end
            s"*${offset}" // UTR  3 after last exon
          } else {
            "?"
          }
        } else {
          // Handling for 5' and 3' UTR on negative strand
          if (variant.position > sortedExons.last.end) {
            val offset = variant.position - sortedExons.last.end
            s"*${offset}" // UTR after last exon
          } else if (variant.position < sortedExons.head.start) {
            val offset = sortedExons.head.start - variant.position
            s"-${offset}" // UTR before first exon
          } else {
            "?"
          }
        }
      }
    }
  }
}
