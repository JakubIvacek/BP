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
   */
  def generateDelHgvsDNA(variant: DnaVariant): String = {
    if (variant.refAllele.length == 1) {
      s"${variant.contig}:g.${variant.position}del"
    } else {
      val positionEnd = variant.position + variant.refAllele.length - 1
      s"${variant.contig}:g.${variant.position}_${positionEnd}del"
    }
  }

  /**
   * Generates RNA-level HGVS annotation for DEL
   * 	sequence_identifier ":" coordinate_type "." position "del
   * 	NM_004006.3:r.127del
   *  NM_004006.3:r.123_127del
   */
  def generateDelHgvsRNA(variant: DnaVariant, entry: GffEntry): String = {
    val rnaPositionStart = Utils.calculateTranscriptPosition(variant.position, entry)
    val rnaPositionEnd = Utils.calculateTranscriptPosition(variant.position + variant.refAllele.length - 1, entry)

    if (variant.refAllele.length == 1) {
      s"${entry.attributes("transcript_id")}:r.${rnaPositionStart}del"
    } else {
      s"${entry.attributes("transcript_id")}:r.${rnaPositionStart}_${rnaPositionEnd}del"
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
    // Calculate the codon positions in the CDS for start and end of the deletion
    val codonPositionStart = Utils.calculateCodonPosition(variant.position.toInt, cds)
    val codonPositionEnd = Utils.calculateCodonPosition((variant.position + variant.refAllele.length - 1).toInt, cds)

    // Get the original codons containing the deletion
    val originalCodonStart = Utils.getCodonAtPosition(codonPositionStart, cds, cdsSequence)
    val originalCodonEnd = Utils.getCodonAtPosition(codonPositionEnd, cds, cdsSequence)

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

