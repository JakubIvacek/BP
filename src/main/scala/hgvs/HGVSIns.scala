package hgvs
import data.{DnaVariant, GffEntry}
import files.FastaReader
/**
 * INS variant HGVS
 * DNA-level HGVS
 * RNA-level HGVS
 * Protein-level HGVS
 */
object HGVSIns {
  /**
   * Generates DNA-level HGVS annotation for INS
   * Syntax	sequence_identifier ":" coordinate_type "." range "ins" sequence
   * range must be specified using adjacent positions
   * Examples NC_000001.11:g.1234_1235insACGT
   */
  def generateInsHgvsDNA(variant: DnaVariant): String = {
    val insertedSequence = if (variant.altAllele.startsWith(variant.refAllele)) {
      variant.altAllele.substring(variant.refAllele.length)
    } else {
      variant.altAllele
    }

    val positionStart = variant.position
    val positionEnd = variant.position + 1

    s"${variant.contig}:g.${positionStart}_${positionEnd}ins$insertedSequence"
  }

  /**
   * Generates RNA-level HGVS annotation for INS
   * Syntax	sequence_identifier ":r." positions "ins" sequence
   * Examples NM_004006.3:r.123_124insauc
   */
  def generateInsHgvsRNA(variant: DnaVariant, entry: GffEntry): String = {
    val insertedSequence = if (variant.altAllele.startsWith(variant.refAllele)) {
      variant.altAllele.substring(variant.refAllele.length)
    } else {
      variant.altAllele
    }
    val correctedInsertedSequence = if (entry.strandPlus) {
      insertedSequence.toLowerCase.replace("t", "u") // Convert to RNA
    } else {
      Utils.reverseComplement(insertedSequence).toLowerCase.replace("t", "u") // Reverse complement and convert to RNA
    }

    val rnaPositionStart = Utils.calculateTranscriptPosition(variant.position, entry)
    val rnaPositionEnd = Utils.calculateTranscriptPosition(variant.position + 1, entry)

    // Get the transcript ID
    val transcriptId = entry.attributes("transcript_id")

    // Construct the RNA-level HGVS annotation
    s"$transcriptId:r.${rnaPositionStart}_${rnaPositionEnd}ins$insertedSequence"
  }

  /**
   * Generates Protein-level HGVS annotation for INS
   * sequence_identifier ":p." aa_range "ins" sequence
   * Examples NP_004371.2:p.(Pro46_Asn47insSerSerTer)
   */
  def generateInsHgvsProtein(variant: DnaVariant, cds: GffEntry): String = {
    val cdsSequence = FastaReader.getSequence(
      variant.NCBIBuild,
      cds.contig,
      cds.start,
      cds.end,
      cds.strandPlus
    )

    // Calculate the codon position in the CDS where the insertion occurs
    val codonPositionStart = Utils.calculateCodonPosition(variant.position.toInt, cds)
    val codonPositionEnd = Utils.calculateCodonPosition(variant.position.toInt + variant.refAllele.length - 1, cds)
    // Get the codons adjacent to the insertion site
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
      s"${cds.attributes("protein_id")}:p.${leftAminoAcid}${codonPositionStart}ins$insertedAminoAcids"
    } else {
      s"${cds.attributes("protein_id")}:p.(${leftAminoAcid}${codonPositionStart}_${rightAminoAcid}${codonPositionEnd}ins${insertedAminoAcids})"
    }
  }
}
