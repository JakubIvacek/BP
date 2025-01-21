package hgvs
import data.{DnaVariant, GffEntry}
import files.FastaReader
/**
 * DEL variant HGVS
 * DNA-level HGVS
 * RNA-level HGVS
 * Protein-level HGVS
 */
object HGVSDel {
  /**
   * Generates DNA-level HGVS annotation for DEL
   * sequence_identifier ":" coordinate_type "." position_or_range "del
   * NC_000001.11:g.1234del
   * NC_000001.11:g.1234_2345del
   * LRG_199t1:c.39+33del
   */
  def generateDelHgvsDNA(variant: DnaVariant, transcript: Option[GffEntry], exon: Option[GffEntry]): String = {
    transcript match{
      case Some(transcript) =>
        val transcriptPosition = exon match {
          case Some(exon) =>
            Utils.calculateTranscriptPosition(variant.position, exon)
          case None =>
            Utils.getTranscriptPosition(variant, variant.contig, transcript.attributes("transcript_id"))
        }
        // Apply the 3' rule for multiple nucleotides in the reference allele
        if (variant.refAllele.length > 1) {
          val positionEnd = if (transcript.strandPlus) {
            variant.position + variant.refAllele.length - 1
          } else {
            variant.position - (variant.refAllele.length - 1)
          }
          s"${transcript.attributes("transcript_id")}:c.${positionEnd}del"  // Use the 3' end position
        } else {
          s"${transcript.attributes("transcript_id")}:c.${transcriptPosition}del"
        }
      case None =>
        if (variant.refAllele.length == 1) {
          s"${variant.contig}:g.${variant.position}del"
        } else {
          val positionEnd = variant.position + variant.refAllele.length - 1
          s"${variant.contig}:g.${variant.position}_${positionEnd}del"  // Use the 3' end position
        }
    }

  }

  /**
   * Generates RNA-level HGVS annotation for DEL
   * 	sequence_identifier ":" coordinate_type "." position "del
   * 	NM_004006.3:r.127del
   *  NM_004006.3:r.123_127del
   */
  def generateDelHgvsRNA(variant: DnaVariant, transcript: GffEntry, exon: Option[GffEntry]): String = {

    val rnaPositionStart = exon match {
      case Some(exon) => Utils.calculateTranscriptPosition(variant.position, exon)
      case None => Utils.getTranscriptPosition(variant, variant.contig, transcript.attributes("transcript_id"))
    }

    val rnaPositionEnd = exon match {
      case Some(exon) => Utils.calculateTranscriptPosition(variant.position + variant.refAllele.length - 1, exon)
      case None =>
        val adjustedPosition = if (transcript.strandPlus) {variant.position + variant.refAllele.length - 1} else {variant.position - variant.refAllele.length + 1}
        Utils.getTranscriptPosition(variant.copy(position = adjustedPosition), variant.contig, transcript.attributes("transcript_id"))
    }
    if (variant.refAllele.length == 1) {
      s"${transcript.attributes("transcript_id")}:r.${rnaPositionStart}del"
    } else {
      s"${transcript.attributes("transcript_id")}:r.${rnaPositionStart}_${rnaPositionEnd}del"
    }
  }

  /**
   * Generates protein-level HGVS annotation for DEL
   * sequence_identifier ":p." aa_position "del" ,  sequence_identifier ":p." aa_position "_" aa_position "del"
   * NP_003997.2:p.Val7del
   * NP_003997.2:p.Lys23_Val25del
   */
  def generateDelHgvsProtein(variant: DnaVariant, cds: GffEntry): String = {
    val cdsSequence = FastaReader.getSequence(
      variant.NCBIBuild,
      cds.contig,
      cds.start,
      cds.end,
      cds.strandPlus
    )
    // Validate the CDS sequence
    if (cdsSequence.isEmpty || variant.position < cds.start || variant.position > cds.end) {
      return s"${cds.attributes("protein_id")}:p.?"
    }
    // Calculate the codon positions in the CDS for start and end of the deletion
    val codonPositionStart = Utils.calculateCodonPosition(variant.position.toInt, cds)
    val codonPositionEnd = Utils.calculateCodonPosition((variant.position + variant.refAllele.length - 1).toInt, cds)

    // Get the original codons containing the deletion
    val originalCodonStart = Utils.getCodonAtPosition(codonPositionStart, cds, cdsSequence)
    val originalCodonEnd = Utils.getCodonAtPosition(codonPositionEnd, cds, cdsSequence)
    // Validate the codons
    if (originalCodonStart == "NNN" || originalCodonEnd  == "NNN") {
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
      // Single amino acid deletion
      s"${cds.attributes("protein_id")}:p.${refAminoAcidStart}${proteinPositionStart}del"
    } else {
      // Multiple amino acid deletion
      s"${cds.attributes("protein_id")}:p.${refAminoAcidStart}${proteinPositionStart}_${refAminoAcidEnd}${proteinPositionEnd}del"
    }
  }
}

