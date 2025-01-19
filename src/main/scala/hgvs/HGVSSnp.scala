package hgvs
import data.{GffEntry, DnaVariant}
/**
 * SNP variant HGVS
 * DNA-level HGVS
 * RNA-level HGVS
 * Protein-level HGVS
 */
object HGVSSnp {
  /**
   * Generates protein-level HGVS annotation for SNPs (Experimentally Ascertained).
   * Example: NP_003997.1:p.Trp24Cys
   */
  def generateProteinHgvsSNP(variant: DnaVariant, cds: GffEntry): String = {
    // Calculate the protein position from genomic coordinates
    val proteinPosition = Utils.calculateProteinPosition(variant.position, cds)

    // Get the reference and alternate amino acids (strand-sensitive)
    val refAminoAcid = CodonAmino.codonToAminoAcid(variant.refAllele)
    val altAminoAcid = CodonAmino.codonToAminoAcid(variant.altAllele)

    // Construct the HGVS string for experimentally ascertained mutation
    s"${cds.attributes("protein_id")}:p.${proteinPosition}${altAminoAcid}"
  }

  /**
   * Generate protein-level HGVS annotation (Predicted mutation).
   * Example: NP_003997.1:p.(Trp24Cys)
   */
  def generateProteinHgvsPredictedSNP(variant: DnaVariant, cds: GffEntry): String = {
    // Calculate the protein position from genomic coordinates
    val proteinPosition = Utils.calculateProteinPosition(variant.position, cds)

    // Get the reference and alternate amino acids (strand-sensitive)
    val refAminoAcid = CodonAmino.codonToAminoAcid(variant.refAllele)
    val altAminoAcid = CodonAmino.codonToAminoAcid(variant.altAllele)

    // Construct the HGVS string for predicted mutation
    s"${cds.attributes("protein_id")}:p.(${proteinPosition}${altAminoAcid})"
  }

  /**
   * Generates RNA-level HGVS annotation for SNPs.
   * sequence_identifier ":r." position reference_nucleotide ">" new_nucleotide
   * NM_004006.3:r.123c>g
   */
  def generateRnaHgvsSNP(variant: DnaVariant, transcript: GffEntry): String = {
    // Extract required attributes
    val transcriptId = transcript.attributes.getOrElse("transcript_id", "UNKNOWN_TRANSCRIPT")

    val rnaPosition = Utils.calculateTranscriptPosition(variant.position, transcript)

    // Handle strand correction for alleles
    val refAllele = if (transcript.strandPlus) variant.refAllele.toLowerCase  else Utils.reverseComplement(variant.refAllele).toLowerCase
    val altAllele = if (transcript.strandPlus) variant.altAllele.toLowerCase  else Utils.reverseComplement(variant.altAllele).toLowerCase

    // Construct RNA HGVS annotation
    s"$transcriptId:r.${rnaPosition}${refAllele}>${altAllele}"
  }

  /**
   * Generates SNP DNA LEVEL Simple sequence substitution
   * sequence_identifier ":" coordinate_type "." position reference_sequence ">" alternate_sequence
   * NC_000023.10:g.33038255C>A
   */
  def generateDnaHgvsSNP(variant: DnaVariant): String = {
    s"${variant.contig}:g.${variant.position}${variant.refAllele}>${variant.altAllele}"
  }

  /**
   * Generates SNP DNA LEVEL Genome reference with coordinates from aligned transcript
   * sequence_identifier "(" transcript_identifier "):c." transcript_position reference_sequence ">" alternate_sequence
   * NG_012232.1(NM_004006.2):c.93+1G>T
   */
  def generateTranscriptHgvsSNP(variant: DnaVariant, transcript: GffEntry): String = {
    // Calculate transcript-relative position
    val transcriptPosition = Utils.calculateTranscriptPosition(variant.position, transcript)

    // Handle strand correction for alleles
    val refAllele = if (transcript.strandPlus) variant.refAllele else Utils.reverseComplement(variant.refAllele)
    val altAllele = if (transcript.strandPlus) variant.altAllele else Utils.reverseComplement(variant.altAllele)

    // Construct HGVS annotation
    s"${transcript.attributes("gene_id")}(${transcript.attributes("transcript_id")}):c.${transcriptPosition}${refAllele}>${altAllele}"
  }
}
