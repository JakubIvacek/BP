package hgvs
import data.{DnaVariant, GffEntry}
import files.FastaReader
import hgvs2.CodonAmino
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
   * Examples NC_000001.11:c.1234_1235insACGT
   */
  def generateInsHgvsDNA(variant: DnaVariant, transcript: Option[GffEntry], exon: Option[GffEntry]): String = {
    val insertedSequence = if (variant.altAllele.startsWith(variant.refAllele)) {
      variant.altAllele.substring(variant.refAllele.length)
    } else {
      variant.altAllele
    }
    transcript match {
      case Some(transcript) =>
        val positionStart = exon match {
          case Some(exon) => Utils.calculateTranscriptPosition(variant.position, exon)
          case None => Utils.getTranscriptPosition(variant, variant.contig, transcript.attributes("transcript_id"))
        }
        val positionEnd = exon match {
          case Some(exon) => Utils.calculateTranscriptPosition(variant.position + 1, exon)
          case None => Utils.getTranscriptPosition(variant.copy(position = variant.position + 1), variant.contig, transcript.attributes("transcript_id"))
        }
        s"${transcript.attributes("transcript_id")}:c.${positionStart}_${positionEnd}ins$insertedSequence"
      case None =>
        val positionStart = variant.position
        val positionEnd = variant.position + 1
        s"${variant.contig}:g.${positionStart}_${positionEnd}ins$insertedSequence"
    }
  }

  /**
   * Generates RNA-level HGVS annotation for INS
   * Syntax	sequence_identifier ":r." positions "ins" sequence
   * Examples NM_004006.3:r.123_124insauc
   */
  def generateInsHgvsRNA(variant: DnaVariant, transcript: GffEntry, exon: Option[GffEntry]): String = {
    val insertedSequence = if (variant.altAllele.startsWith(variant.refAllele)) {
      variant.altAllele.substring(variant.refAllele.length)
    } else {
      variant.altAllele
    }
    val correctedInsertedSequence = if (transcript.strandPlus) {
      insertedSequence.toLowerCase.replace("t", "u") // Convert to RNA
    } else {
      Utils.reverseComplement(insertedSequence).toLowerCase.replace("t", "u") // Reverse complement and convert to RNA
    }

    val positionStart = exon match {
      case Some(exon) => Utils.calculateTranscriptPosition(variant.position, exon)
      case None => Utils.getTranscriptPosition(variant, variant.contig, transcript.attributes("transcript_id"))
    }
    val positionEnd = exon match {
      case Some(exon) => Utils.calculateTranscriptPosition(variant.position + 1, exon)
      case None => Utils.getTranscriptPosition(variant.copy(position = variant.position + 1), variant.contig, transcript.attributes("transcript_id"))
    }

    // Get the transcript ID
    val transcriptId = transcript.attributes("transcript_id")

    // Construct the RNA-level HGVS annotation
    s"$transcriptId:r.${positionStart}_${positionEnd}ins$correctedInsertedSequence"
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
    // Validate the CDS sequence
    if (cdsSequence.isEmpty || variant.position < cds.start || variant.position > cds.end) {
      return s"${cds.attributes("protein_id")}:p.?"
    }
    // Calculate the codon position in the CDS where the insertion occurs
    val codonPositionStart = Utils.calculateCodonPosition(variant.position.toInt, cds)
    val endPosition = if (cds.strandPlus) {variant.position + variant.refAllele.length - 1} else {variant.position - variant.refAllele.length + 1}
    val codonPositionEnd = Utils.calculateCodonPosition(endPosition.toInt, cds)
    
    // Get the codons adjacent to the insertion site
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
      s"${cds.attributes("protein_id")}:p.${leftAminoAcid}${codonPositionStart}ins$insertedAminoAcids"
    } else {
      s"${cds.attributes("protein_id")}:p.(${leftAminoAcid}${codonPositionStart}_${rightAminoAcid}${codonPositionEnd}ins${insertedAminoAcids})"
    }
  }
}
