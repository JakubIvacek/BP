package hgvs
import data.{DnaVariant, GffEntry}
import files.{FastaReader, GFFReader}
import hgvs2.CodonAmino
/**
 * SNP variant HGVS
 * DNA-level HGVS
 * RNA-level HGVS
 * Protein-level HGVS
 */
object HGVSSnp {
  /**
   * Generates protein-level HGVS annotation for SNPs (Experimentally Ascertained).
   * sequence_identifier ":" coordinate_type "." aa_position alternate_base
   * Example: NP_003997.1:p.Trp24Cys
   */
  def generateProteinHgvsPredictedSNP(variant: DnaVariant, cds: GffEntry): String = {
    val cdsSequence = FastaReader.getSequence(
      variant.NCBIBuild,
      cds.contig,
      cds.start,
      cds.end,
      cds.strandPlus
    )
    // Ensure CDS sequence is valid
    if (cdsSequence.isEmpty || variant.position < cds.start || variant.position > cds.end) {
      return s"${cds.attributes("protein_id")}:p.?"
    }

    // Calculate the codon position in the CDS
    val codonPosition = Utils.calculateCodonPosition(variant.position.toInt, cds)

    // Get the original codon containing the SNP
    val originalCodon = Utils.getCodonAtPosition(codonPosition, cds, cdsSequence)
    if (originalCodon == "NNN") {
      return s"${cds.attributes("protein_id")}:p.?"
    }
    // Replace the affected nucleotide in the codon
    val codonIndex = ((variant.position - cds.start) % 3).toInt
    if (codonIndex < 0 || codonIndex >= originalCodon.length) {
      return s"${cds.attributes.getOrElse("protein_id", "unknown_protein")}:p.?"
    }
    val modifiedCodon = originalCodon.updated(codonIndex.toInt, if (cds.strandPlus) variant.altAllele.head else Utils.reverseComplement(variant.altAllele).head)

    // Translate the codons into amino acids
    val refAminoAcid = CodonAmino.codonToAminoAcid(originalCodon)
    val altAminoAcid = CodonAmino.codonToAminoAcid(modifiedCodon)

    // Calculate the protein position from the codon position
    val proteinPosition = Utils.calculateProteinPosition(codonPosition, cds)

    // Construct the HGVS string
    s"${cds.attributes("protein_id")}:p.(${refAminoAcid}${proteinPosition}${altAminoAcid})"
  }
  
  /**
   * Generates RNA-level HGVS annotation for SNPs.
   * sequence_identifier ":r." position reference_nucleotide ">" new_nucleotide
   * NM_004006.3:r.123c>g
   */
  def generateRnaHgvsSNP(variant: DnaVariant, entry: GffEntry, exon: Option[GffEntry]): String = {
    val transcriptId = entry.attributes("transcript_id")
    val rnaPosition = exon match {
      case Some(exon) =>
        Utils.calculateTranscriptPosition(variant.position, exon)  // Use the exon position if available
      case None =>
        Utils.getTranscriptPosition(variant, variant.contig, transcriptId)  // default calculation
    }

    // Handle strand correction for alleles
    val refAllele = if (entry.strandPlus) {
      variant.refAllele.toLowerCase.replace("t", "u")
    } else {
      Utils.reverseComplement(variant.refAllele).toLowerCase.replace("t", "u")
    }
    val altAllele = if (entry.strandPlus) {
      variant.altAllele.toLowerCase.replace("t", "u")
    } else {
      Utils.reverseComplement(variant.altAllele).toLowerCase.replace("t", "u")
    }
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
  def generateDnaTransHgvsSNP(variant: DnaVariant, entry: GffEntry, exon: Option[GffEntry]): String = {

    val transcriptPosition = exon match {
      case Some(exon) =>
        Utils.calculateTranscriptPosition(variant.position, exon)
      case None =>
        Utils.getTranscriptPosition(variant, variant.contig, entry.attributes("transcript_id"))
    }
    // Handle strand correction for alleles
    val refAllele = if (entry.strandPlus) variant.refAllele else Utils.reverseComplement(variant.refAllele)
    val altAllele = if (entry.strandPlus) variant.altAllele else Utils.reverseComplement(variant.altAllele)

    // Construct HGVS annotation
    s"${entry.attributes("gene_id")}(${entry.attributes("transcript_id")}):c.${transcriptPosition}${refAllele}>${altAllele}"
  }
}
