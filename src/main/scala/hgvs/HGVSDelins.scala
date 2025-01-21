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
   * Example: NC_000001.11:g.123_125delinsAC
   */
  def generateDelinsHgvsDNA(variant: DnaVariant, transcript: Option[GffEntry], exon: Option[GffEntry]): String = {
    val coordinate = transcript match{
      case Some(transcript) =>
        "c"
      case None =>
       "g"
    }
    val identif = transcript match {
      case Some(transcript) =>
        transcript.attributes("transcript_id")
      case None =>
        variant.contig
    }
    val positionStart = variant.position
    val positionEnd = variant.position + variant.refAllele.length - 1

    // For a single position
    if (variant.refAllele.length == 1) {
      s"${identif}:$coordinate.${positionStart}delins${variant.altAllele}"
    } else {
      // For a range
      s"${identif}:$coordinate.${positionStart}_${positionEnd}delins${variant.altAllele}"
    }
  }

  /**
   * Generates RNA-level HGVS annotation for DELINS
   * Syntax	sequence_identifier ":r." position "delins" sequence
   * Examples NM_004006.3:r.123_127delinsag
   */
  def generateDelinsHgvsRNA(variant: DnaVariant, transcript: GffEntry, exon: Option[GffEntry]): String = {
    val rnaPositionStart = exon match {
      case Some(exon) => Utils.calculateTranscriptPosition(variant.position, exon)
      case None => Utils.getTranscriptPosition(variant, variant.contig, transcript.attributes("transcript_id"))
    }

    val rnaPositionEnd = exon match {
      case Some(exon) => Utils.calculateTranscriptPosition(variant.position + variant.refAllele.length - 1, exon)
      case None =>
        val adjustedPosition = if (transcript.strandPlus) {
          variant.position + variant.refAllele.length - 1
        } else {
          variant.position - variant.refAllele.length + 1
        }
        Utils.getTranscriptPosition(variant.copy(position = adjustedPosition), variant.contig, transcript.attributes("transcript_id"))
    }
    // Handle strand-specific sequence
    val insertedSequence = if (transcript.strandPlus) {
      variant.altAllele.toLowerCase.replace("t", "u") // Convert to RNA (replace T with U)
    } else {
      Utils.reverseComplement(variant.altAllele).toLowerCase.replace("t", "u") // Reverse complement and convert to RNA
    }

    // Construct the RNA-level HGVS annotation
    s"${transcript.attributes("transcript_id")}:r.${rnaPositionStart}_${rnaPositionEnd}delins$insertedSequence"
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
    // Validate the CDS sequence
    if (cdsSequence.isEmpty || variant.position < cds.start || variant.position > cds.end) {
      return s"${cds.attributes("protein_id")}:p.?"
    }
    // Calculate the codon position in the CDS where the delins occurs
    val codonPositionStart = Utils.calculateCodonPosition(variant.position.toInt, cds)
    val codonPositionEnd = Utils.calculateCodonPosition(variant.position.toInt + variant.refAllele.length - 1, cds)

    // Get the codons adjacent to the deletion insertion site
    val leftCodon = Utils.getCodonAtPosition(codonPositionStart - 1, cds, cdsSequence)
    val rightCodon = Utils.getCodonAtPosition(codonPositionEnd, cds, cdsSequence)
    // Validate the codons
    if (leftCodon == "NNN" || rightCodon == "NNN") {
      return s"${cds.attributes("protein_id")}:p.?"
    }
    // Translate the codons into amino acids
    val leftAminoAcid = CodonAmino.codonToAminoAcid(leftCodon)
    val rightAminoAcid = CodonAmino.codonToAminoAcid(rightCodon)
    
    // Incorporate the inserted sequence into the CDS
    val insertionStart = (variant.position - cds.start).toInt
    if (insertionStart < 0 || insertionStart > cdsSequence.length) {
      return s"${cds.attributes("protein_id")}:p.?"
    }
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
