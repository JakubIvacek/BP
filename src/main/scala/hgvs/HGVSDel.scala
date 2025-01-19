package hgvs
import data.{DnaVariant, GffEntry}
/**
 * DEL variant HGVS
 * DNA-level HGVS
 * RNA-level HGVS
 * Protein-level HGVS
 */
object HGVSDel {
  def generateDelHgvsDNA(variant: DnaVariant): String = {
    if (variant.refAllele.length == 1) {
      s"${variant.contig}:g.${variant.position}del"
    } else {
      val positionEnd = variant.position + variant.refAllele.length - 1
      s"${variant.contig}:g.${variant.position}_${positionEnd}del"
    }
  }

  def generateDelHgvsRNA(variant: DnaVariant, transcript: GffEntry): String = {
    val rnaPositionStart = Utils.calculateTranscriptPosition(variant.position, transcript)
    val rnaPositionEnd = Utils.calculateTranscriptPosition(variant.position + variant.refAllele.length - 1, transcript)

    if (variant.refAllele.length == 1) {
      s"${transcript.attributes("transcript_id")}:r.${rnaPositionStart}del"
    } else {
      s"${transcript.attributes("transcript_id")}:r.${rnaPositionStart}_${rnaPositionEnd}del"
    }
  }

  def generateDelHgvsProtein(variant: DnaVariant, cds: GffEntry): String = {
    val proteinPositionStart = Utils.calculateProteinPosition(variant.position, cds)
    val proteinPositionEnd = Utils.calculateProteinPosition(variant.position + variant.refAllele.length - 1, cds)

    if (variant.refAllele.length == 1) {
      s"${cds.attributes("protein_id")}:p.${CodonAmino.codonToAminoAcid(variant.refAllele)}${proteinPositionStart}del"
    } else {
      s"${cds.attributes("protein_id")}:p.${CodonAmino.codonToAminoAcid(variant.refAllele)}${proteinPositionStart}_${CodonAmino.codonToAminoAcid(variant.altAllele)}${proteinPositionEnd}del"
    }
  }
}

