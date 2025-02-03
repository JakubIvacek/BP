package hgvs
import data.{DnaVariant, GffEntry}
import files.FastaReader
import hgvs2.CodonAmino
/**
 * Generates HGVS annotation for DUP.
 * Duplication: a sequence change where, compared to a reference sequence,
 * a copy of one or more nucleotides is inserted directly
 */
object HGVSDup {
  /**
   * Generates DNA-level HGVS annotation for DUP.
   * sequence_identifier ":" coordinate_type "." position_or_range "dup"
   * Examples: NC_000001.11:g.1234dup NC_000001.11:c.1234_1235dup
   */
  def generateDupHgvsDNA(variant: DnaVariant, transcript: Option[GffEntry], exon: Option[GffEntry]): String = {
    transcript match {
      case Some(transcript) =>
        // c. notation
        val positionStart = exon match {
          case Some(exon) => Utils.calculateTranscriptPosition(variant.position, exon)
          case None => Utils.getTranscriptPosition(variant, variant.contig, transcript.attributes("transcript_id"))
        }
        val positionEnd = exon match {
          case Some(exon) =>
            val adjustedPosition = if (transcript.strandPlus) {variant.position + variant.refAllele.length - 1} else {variant.position - variant.refAllele.length + 1}
            Utils.calculateTranscriptPosition(adjustedPosition, exon)
          case None =>
            val adjustedPosition = if (transcript.strandPlus) {variant.position + variant.refAllele.length - 1} else {variant.position - variant.refAllele.length + 1}
            Utils.getTranscriptPosition(variant.copy(position = adjustedPosition), variant.contig, transcript.attributes("transcript_id"))
        }
        if (variant.refAllele.length == 1) {
          s"${transcript.attributes("transcript_id")}:c.${positionStart}dup"
        } else {
          s"${transcript.attributes("transcript_id")}:c.${positionStart}_${positionEnd}dup"
        }

      case None =>
        // g. notation
        val positionStart = variant.position
        val positionEnd = variant.position + variant.refAllele.length - 1
        if (variant.refAllele.length == 1) {
          s"${variant.contig}:g.${positionStart}dup"
        } else {
          s"${variant.contig}:g.${positionStart}_${positionEnd}dup"
        }
    }
  }

  /**
   * Generates DUP-level HGVS annotation for DEL
   * 	sequence_identifier ":r." position "dup"
   * NM_004006.3:r.123_345dup
   */
  def generateDUPHgvsRNA(variant: DnaVariant, transcript: GffEntry, exon: Option[GffEntry]): String = {
    // Calculate RNA start position from exon or transcript information
    val rnaPositionStart = exon match {
      case Some(exon) => Utils.calculateTranscriptPosition(variant.position, exon)
      case None => Utils.getTranscriptPosition(variant, variant.contig, transcript.attributes("transcript_id"))
    }
    if (variant.refAllele.length == 1) {
      s"${transcript.attributes("transcript_id")}:r.${rnaPositionStart}dup"
    } else {
      val rnaPositionEnd = exon match {
        case Some(exon) =>
          val adjustedPosition = if (transcript.strandPlus) {variant.position + variant.refAllele.length - 1} else {variant.position - variant.refAllele.length + 1}
          Utils.calculateTranscriptPosition(adjustedPosition, exon)
        case None =>
          val adjustedPosition = if (transcript.strandPlus) {variant.position + variant.refAllele.length - 1} else {variant.position - variant.refAllele.length + 1}
          Utils.getTranscriptPosition(variant.copy(position = adjustedPosition), variant.contig, transcript.attributes("transcript_id"))
      }
      s"${transcript.attributes("transcript_id")}:r.${rnaPositionStart}_${rnaPositionEnd}dup"
    }
  }

  /**
   * Generates protein-level HGVS annotation for DUP.
   * - Single position: sequence_identifier ":p." aa_position "dup"
   * Example: NP_003997.2:p.Val7dup
   * - Position range: sequence_identifier ":p." aa_position "_" aa_position "dup"
   * Example: NP_003997.2:p.Lys23_Val25dup
   *
   */
  def generateDupHgvsProtein(variant: DnaVariant, cds: GffEntry): String = {
    val cdsSequence = FastaReader.getSequence(
      variant.NCBIBuild,
      cds.contig,
      cds.start,
      cds.end,
      cds.strandPlus
    )
    if (cdsSequence.isEmpty || variant.position < cds.start || variant.position > cds.end) {
      return s"${cds.attributes("protein_id")}:p.?"
    }

    // Calculate the codon positions in the CDS for the start and end of the duplication
    val codonPositionStart = Utils.calculateCodonPosition(variant.position.toInt, cds)
    val endPosition = if (cds.strandPlus) {variant.position + variant.refAllele.length - 1} else {variant.position - variant.refAllele.length + 1}
    val codonPositionEnd = Utils.calculateCodonPosition(endPosition.toInt, cds)

    // Get the original codons containing the duplication
    val originalCodonStart = Utils.getCodonAtPosition(codonPositionStart, cds, cdsSequence)
    val originalCodonEnd = Utils.getCodonAtPosition(codonPositionEnd, cds, cdsSequence)

    // Validate the codons
    if (originalCodonStart == "NNN" || originalCodonEnd == "NNN") {
      return s"${cds.attributes("protein_id")}:p.?"
    }

    // Translate the codons into amino acids
    val refAminoAcidStart = CodonAmino.codonToAminoAcid(originalCodonStart)
    val refAminoAcidEnd = CodonAmino.codonToAminoAcid(originalCodonEnd)

    // Calculate the protein positions from the codon positions
    val proteinPositionStart = Utils.calculateProteinPosition(codonPositionStart, cds)
    val proteinPositionEnd = Utils.calculateProteinPosition(codonPositionEnd, cds)

    // Construct the HGVS string
    if (proteinPositionStart == proteinPositionEnd) {
      // Single amino acid duplication
      s"${cds.attributes("protein_id")}:p.${refAminoAcidStart}${proteinPositionStart}dup"
    } else {
      // Multiple amino acid duplication
      s"${cds.attributes("protein_id")}:p.${refAminoAcidStart}${proteinPositionStart}_${refAminoAcidEnd}${proteinPositionEnd}dup"
    }
  }
}
