package hgvs

import data.VariantType.{DEL, DUP, EXT, FS, INDEL, INS, INV, Other, RPT, SNP}
import data.{DnaVariant, GffEntry, VariantType}
import files.GFFReader

object Utils {
  
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
   * Identifies the repeated sequence and its repeat count.
   *
   * @param ref The reference sequence
   * @param alt The alternate sequence
   * @return A tuple containing the repeat unit and the total repeat count.
   */
  def repeatedSequence(ref: String, alt: String): String = {

    // Extract the remaining sequence after the reference
    val remaining = alt.substring(ref.length)

    // Find the smallest repeat unit that constructs the remaining sequence
    val repeatUnit = (1 to ref.length).find { unitLength =>
      val candidateUnit = ref.substring(0, unitLength)
      remaining.grouped(unitLength).forall(_ == candidateUnit)
    }.map(ref.substring(0, _)).getOrElse("")

    // Calculate the repeat count, including the initial reference
    val totalRepeatCount = if (repeatUnit.nonEmpty) {
      (alt.length / repeatUnit.length)
    } else {
      1
    }
    s"{$repeatUnit[${totalRepeatCount}])"
  }

  /**
   * Determines the alternate sequence after removing shared prefix and suffix.
   * This is for indel (insertion-deletion) variants to isolate the differing sequence.
   *
   * @param ref The reference sequence
   * @param alt The alternate sequence
   * @return The trimmed alternate sequence with shared prefix and suffix removed.
   */
  def indelSequence(ref: String, alt: String): String = {
    //1. Remove shared prefix
    val prefixLength = ref.zip(alt).takeWhile { case (ref, alt) => ref == alt }.length
    val trimmedRef = ref.substring(prefixLength)
    val trimmedAlt = alt.substring(prefixLength)
    // 2. Remove shared suffix
    val suffixLength = trimmedRef.reverse.zip(trimmedAlt.reverse).takeWhile { case (ref, alt) => ref == alt }.length
    val finalRef = trimmedRef.substring(0, trimmedRef.length - suffixLength)
    val finalAlt = trimmedAlt.substring(0, trimmedAlt.length - suffixLength)
    finalAlt
  }

  /**
   * Retrieves the string representing the type of variant for use in HGVS notation.
   *
   * @param variantType The type of the variant (e.g., DEL, INS, DUP).
   * @return A string indicating the type of sequence change.
   */
  def getPastPositionPart(variantType: VariantType): String = {
    variantType match {
      case DEL => "del"
      case INS => "ins"
      case DUP => "dup"
      case INV => "inv"
      case INDEL => "delins"
      case Other => "="
      // ext, fs
      case FS => "fs"
      case _ => ""
    }
  }
  
  /**
   * Returns the transcript-relative position of the variant.
   * If the variant is inside a transcript, it calculates the relative position.
   * If it's outside, it handles UTRs or intronic regions based on the provided exon information.
   *
   * @param variant The DNA variant.
   * @param exon The exon containing the variant, if any.
   * @param transcriptId id of the matching transcript            
   * @return The transcript-relative position as a string:
   *         - Numerical position for exonic locations.
   *         - "pos-off" or "pos+off" for intronic variants.
   *         - "-X" for 5' UTR.
   *         - "*X" for 3' UTR.
   *         - "?" if the position cannot be determined.
   */
  def calculateTranscriptPosition(variant: DnaVariant, position: Int, exon: Option[GffEntry], transcriptId: String): String = {
    exon match {
      case Some(exonEntry) =>
        // Variant is within an exon
        if (exonEntry.strandPlus) {
          // Positive strand: Calculate position relative to exon start
          (position - exonEntry.start + 1).toString
        } else {
          // Negative strand: Calculate position relative to exon end
          (exonEntry.end - position + 1).toString
        }
      case None =>
        val transExons = GFFReader.getIntervalTree(variant.contig).getExonsForTranscriptId(variant.contig, transcriptId)
        val sortedExons = transExons.sortBy(_.start)

        if (sortedExons.isEmpty) {
          return "?"
        }

        val isStrandPlus = sortedExons.head.strandPlus
        if (sortedExons.size == 1) {
          val exon = sortedExons.head
          if (isStrandPlus) {
            // UTR handling for positive strand
            if (position < exon.start) {
              val offset = sortedExons.head.start - position
              s"-${offset}" // UTR 5 before first exon
            } else if (position > sortedExons.last.end) {
              val offset = position - sortedExons.last.end
              s"*${offset}" // UTR  3 after last exon
            } else {
              (position - exon.start + 1).toString // Inside exon
            }
          } else {
            // UTR handling for negative strand
            if (position > exon.end) {
              val offset = position - sortedExons.last.end
              s"*${offset}" // UTR after last exon
            } else if (position < sortedExons.head.start) {
              val offset = sortedExons.head.start - position
              s"-${offset}" // UTR before first exon
            } else {
              (exon.end - position + 1).toString // Inside exon
            }
          }
        } else {
          // Check if the variant is intron
          val relativePosition = sortedExons.zip(sortedExons.tail).collectFirst {
            case (exon1, exon2) if position > exon1.end && position < exon2.start =>
              // Calculate the offset from the exon boundary
              val offset = position - exon1.end
              // Determine the position string based on the strand direction
              val positionLast = if (isStrandPlus) {
                s"${exon1.end}+${offset}" // After the first exon on positive strand
              } else {
                s"${exon2.start}-${offset}" // Before the second exon on negative strand
              }
              positionLast
          }

          relativePosition.getOrElse {
            if (isStrandPlus) {
              // Handling for 5' and 3' UTR on positive strand
              if (position < sortedExons.head.start) {
                val offset = sortedExons.head.start - position
                s"-${offset}" // UTR 5 before first exon
              } else if (position > sortedExons.last.end) {
                val offset = position - sortedExons.last.end
                s"*${offset}" // UTR  3 after last exon
              } else {
                "?"
              }
            } else {
              // Handling for 5' and 3' UTR on negative strand
              if (position > sortedExons.last.end) {
                val offset = position - sortedExons.last.end
                s"*${offset}" // UTR after last exon
              } else if (position < sortedExons.head.start) {
                val offset = sortedExons.head.start - position
                s"-${offset}" // UTR before first exon
              } else {
                "?"
              }
            }
          }
        }
    }
  }
}
