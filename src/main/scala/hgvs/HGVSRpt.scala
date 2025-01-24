package hgvs

import data.{DnaVariant, GffEntry}
import files.FastaReader

object HGVSRpt {
  /**
   * Generates DNA-level HGVS annotation for RPT
   * sequence_identifier ":" coordinate_type "." position sequence "[" total_copy_number "]"
   * NC_000014.8:g.123_191CAG[23]
   */
  def generateRptHgvsDNA(variant: DnaVariant, transcript: Option[GffEntry], exon: Option[GffEntry]): String = {
    transcript match {
      case Some(transcript) =>
        val positionStart = exon match {
          case Some(exon) => Utils.calculateTranscriptPosition(variant.position, exon)
          case None => Utils.getTranscriptPosition(variant, variant.contig, transcript.attributes("transcript_id"))
        }
        val adjustedPosition = if (transcript.strandPlus) {variant.position + variant.refAllele.length - 1} else {variant.position - variant.refAllele.length + 1}
        val positionEnd = exon match {
          case Some(exon) => Utils.calculateTranscriptPosition(adjustedPosition, exon)
          case None => Utils.getTranscriptPosition(variant.copy(position = adjustedPosition), variant.contig, transcript.attributes("transcript_id"))
        }
        val sequence, copy_num = RepeatedSequence(variant.refAllele, variant.altAllele)
        s"${transcript.attributes("transcript_id")}:c.${positionStart}_${positionEnd}${sequence}[${copy_num}]"
      case None =>
        val positionStart = variant.position
        val positionEnd = variant.position + variant.altAllele.length - 1
        val sequence, copy_num = RepeatedSequence(variant.refAllele, variant.altAllele)
        s"${variant.contig}:g.${positionStart}_${positionEnd}${sequence}[${copy_num}]"
    }
  }

  /**
   * Generates RNA-level HGVS annotation for RPT
   * sequence_identifier ":" coordinate_type "." position sequence "[" total_copy_number "]"
   * NC_000014.8:g.123_191caa[23]
   */
  def generateRptHgvsRNA(variant: DnaVariant, transcript: GffEntry, exon: Option[GffEntry]): String = {
    val positionStart = exon match {
      case Some(exon) => Utils.calculateTranscriptPosition(variant.position, exon)
      case None => Utils.getTranscriptPosition(variant, variant.contig, transcript.attributes("transcript_id"))
    }
    val adjustedPosition = if (transcript.strandPlus) {variant.position + variant.refAllele.length - 1} else {variant.position - variant.refAllele.length + 1}
    val positionEnd = exon match {
      case Some(exon) => Utils.calculateTranscriptPosition(adjustedPosition, exon)
      case None => Utils.getTranscriptPosition(variant.copy(position = adjustedPosition), variant.contig, transcript.attributes("transcript_id"))
    }
    val (sequence, copy_num) = RepeatedSequence(variant.refAllele, variant.altAllele)
    val correctedInsertedSequence = if (transcript.strandPlus) {
      sequence.toLowerCase.replace("t", "u") // Convert to RNA
    } else {
      Utils.reverseComplement(sequence).toLowerCase.replace("t", "u") // Reverse complement and convert to RNA
    }
    s"${transcript.attributes("transcript_id")}:r.${positionStart}_${positionEnd}${sequence}[${copy_num}]"
  }


  /**
   * Identifies the repeated sequence and its repeat count.
   *
   * @param ref The reference sequence (refAllele or refProtein).
   * @param alt The alternate sequence (altAllele or altProtein).
   * @return A tuple containing the repeat unit and the total repeat count.
   */
  def RepeatedSequence(ref: String, alt: String): (String, Int) = {

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

    (repeatUnit, totalRepeatCount)
  }
}
