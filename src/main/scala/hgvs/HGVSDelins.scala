package hgvs
import data.{DnaVariant, GffEntry}
import files.FastaReader
/**
 * DELINS variant HGVS annotation add
 * DNA-level HGVS
 * RNA-level HGVS
 * Protein-level HGVS
 */
object HGVSDelins {
  /**
   * Generates DNA-level HGVS annotation for DELINS
   * Syntax: sequence_identifier ":" coordinate_type "." position "delins" sequence
   * Example: NC_000001.11:g.123delinsAC
   */
  def generateDelinsHgvsDNA(variant: DnaVariant): String = {
    val positionStart = variant.position
    val positionEnd = variant.position + variant.refAllele.length - 1

    // For a single position
    val dnaHgvs = if (variant.refAllele.length == 1) {
      s"${variant.contig}:g.${positionStart}delins${variant.altAllele}"
    } else {
      // For a range
      s"${variant.contig}:g.${positionStart}_${positionEnd}delins${variant.altAllele}"
    }

    dnaHgvs
  }

  /**
   * Generates RNA-level HGVS annotation for DELINS
   * Syntax	sequence_identifier ":r." position "delins" sequence
   * Examples NM_004006.3:r.123_127delinsag
   */
  def generateDelinsHgvsRNA(variant: DnaVariant, entry: GffEntry): String = {
    // Calculate the start and end RNA positions
    val rnaPositionStart = Utils.calculateTranscriptPosition(variant.position, entry)
    val rnaPositionEnd = Utils.calculateTranscriptPosition(variant.position + variant.refAllele.length - 1, entry)

    // Handle strand-specific sequence
    val insertedSequence = if (entry.strandPlus) {
      variant.altAllele.toLowerCase.replace("t", "u") // Convert to RNA (replace T with U)
    } else {
      Utils.reverseComplement(variant.altAllele).toLowerCase.replace("t", "u") // Reverse complement and convert to RNA
    }

    // Construct the RNA-level HGVS annotation
    s"${entry.attributes("transcript_id")}:r.${rnaPositionStart}_${rnaPositionEnd}delins$insertedSequence"
  }

  /**
   * Generates Protein-level HGVS annotation for DELINS
   * Syntax	sequence_identifier ":r." position "delins" sequence
   * Examples NM_004006.3:r.123_127delinsag
   */
  def generateDelinsHgvsProtein(variant: DnaVariant, cds: GffEntry): String = {
    val cdsSequence = FastaReader.getSequence(
      variant.NCBIBuild,
      cds.contig,
      cds.start,
      cds.end,
      cds.strandPlus
    )

    // Calculate the codon position in the CDS where the delins occurs
    val codonPositionStart = Utils.calculateCodonPosition(variant.position.toInt, cds)
    val codonPositionEnd = Utils.calculateCodonPosition(variant.position.toInt + variant.refAllele.length - 1, cds)

    // Get the codons adjacent to the deletion insertion site
    val leftCodon = Utils.getCodonAtPosition(codonPositionStart - 1, cds, cdsSequence)
    val rightCodon = Utils.getCodonAtPosition(codonPositionEnd, cds, cdsSequence)

    // Translate the codons into amino acids
    val leftAminoAcid = CodonAmino.codonToAminoAcid(leftCodon)
    val rightAminoAcid = CodonAmino.codonToAminoAcid(rightCodon)
    
    // Incorporate the inserted sequence into the CDS
    val insertionStart = (variant.position - cds.start).toInt
    val modifiedCdsSequence = cdsSequence.substring(0, insertionStart) +
      variant.altAllele +
      cdsSequence.substring(insertionStart)

    // Extract the inserted amino acids
    val insertedCdsSegment = variant.altAllele + cdsSequence.substring(insertionStart, insertionStart + (3 - variant.altAllele.length % 3))
    val insertedAminoAcids = insertedCdsSegment.grouped(3).map(CodonAmino.codonToAminoAcid).mkString

    // Construct the protein-level HGVS annotation
    if (codonPositionStart == codonPositionEnd) {
      s"${cds.attributes("protein_id")}:p.${leftAminoAcid}${codonPositionStart}delins$insertedAminoAcids"
    } else {
      s"${cds.attributes("protein_id")}:p.${leftAminoAcid}${codonPositionStart}_${rightAminoAcid}${codonPositionEnd}delins$insertedAminoAcids"
    }
  }

}
