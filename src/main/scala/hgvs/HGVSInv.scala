package hgvs
import data.{DnaVariant, GffEntry}

/**
 * INV variant HGVS
 * DNA-level HGVS
 * RNA-level HGVS
 * Protein-level HGVS
 */
object HGVSInv {
  /**
   * Generates DNA-level HGVS annotation for INV
   * sequence_identifier ":" coordinate_type "." range "inv"
   * NC_000001.11:g.1234_2345inv
   */
  def generateInvHgvsDNA(variant: DnaVariant, transcript: Option[GffEntry], exon: Option[GffEntry]): String = {
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
        s"${transcript.attributes("transcript_id")}:c.${positionStart}_${positionEnd}inv"
      case None =>
        val positionStart = variant.position
        val positionEnd = variant.position + variant.altAllele.length - 1
        s"${variant.contig}:g.${positionStart}_${positionEnd}inv"
    }
  }

  /**
   * Generates RNA-level HGVS annotation for INV
   * sequence_identifier ":" coordinate_type "." position "inv
   * NM_004006.3:r.123_127inv
   */
  def generateInvHgvsRNA(variant: DnaVariant, transcript: GffEntry, exon: Option[GffEntry]): String = {
    // Calculate RNA start position from exon or transcript information
    val positionStart = exon match {
      case Some(exon) => Utils.calculateTranscriptPosition(variant.position, exon)
      case None => Utils.getTranscriptPosition(variant, variant.contig, transcript.attributes("transcript_id"))
    }
    val adjustedPosition = if (transcript.strandPlus) {variant.position + variant.refAllele.length - 1} else {variant.position - variant.refAllele.length + 1}
    val positionEnd = exon match {
      case Some(exon) => Utils.calculateTranscriptPosition(adjustedPosition, exon)
      case None => Utils.getTranscriptPosition(variant.copy(position = adjustedPosition), variant.contig, transcript.attributes("transcript_id"))
    }
    s"${transcript.attributes("transcript_id")}:r.${positionStart}_${positionEnd}inv"
  }
}
