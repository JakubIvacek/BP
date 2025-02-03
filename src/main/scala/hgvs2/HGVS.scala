package hgvs2
import data.VariantType.{DEL, DUP, INDEL, INS, INV, Other, RPT, SNP}
import data.{DnaVariant, GffEntry, VariantType}
import files.FastaReader

object HGVS {

  /**
   * Generates HGVS notations for DNA, RNA, and protein levels for a given variant.
   *
   * @param variant The DNA variant to be annotated.
   * @param entries GFF entries containing transcript and protein information.
   */
  def variantAddHGVS(variant: DnaVariant, entries: Seq[GffEntry]): Unit = {

    // DNA/RNA LEVEL: Mapped to transcript
    val matchingTranscript = entries.find(_.name == "transcript")
    matchingTranscript match {
      case Some(transcript) =>
        HGVSRNA(variant, entries, transcript)
        HGVSDNAc(variant, entries, transcript)
      case None =>  HGVSDNA(variant, entries)
    }

    // Protein LEVEL: Mapped to protein
    val proteinEntry = entries.find(_.attributes.contains("protein_id"))
    proteinEntry match {
      case Some(entry) => HGVSProtein(variant, entries, entry)
      case None => 
    }
  }

  /**
   * Generates HGVS notation at the genomic level (DNA).
   *
   * @param variant The DNA variant.
   * @param entries GFF entries.
   */
  def HGVSDNA(variant: DnaVariant, entries: Seq[GffEntry]): Unit = {
    // seq : coordinate_type . position/range ppp? sequence
    val seqId = variant.contig
    val position = HGVSDnaRna.getPosition(variant)
    val pastPosPart = Utils.getPastPositionPart(variant.varType)
    val seq = HGVSDnaRna.getSeq(variant, true)
    val hgvs = s"${seqId}:g.${position}$pastPosPart$seq"
    
    variant.HGVSDNA = hgvs
  }

  /**
   * Generates HGVS notation at the coding/noncoding DNA level (cDNA).
   *
   * @param variant    The DNA variant.
   * @param entries    GFF entries containing transcript information.
   * @param transcript The matched transcript GFF entry.
   */
  def HGVSDNAc(variant: DnaVariant, entries: Seq[GffEntry], transcript: GffEntry): Unit = {
    // seq : coordinate_type . position/range ppp? sequence
    // SNP seq (trans) ...
    val seqId = variant.contig
    val transId = transcript.attributes("transcript_id")
    val geneId = transcript.attributes("gene_id")
    val seq = HGVSDnaRna.getSeq(variant, transcript.strandPlus)
    val pos = HGVSDnaRna.getTranscriptPosition(variant, entries.find(_.name == "exon"), transId, transcript.strandPlus)
    val pastPosPart = Utils.getPastPositionPart(variant.varType)
    val coordinateType = if entries.exists(_.attributes.contains("protein_id")) then "c" else "n"

    val hgvs = if variant.varType == SNP then s"${geneId}($transId):$coordinateType.${pos}$pastPosPart$seq"
    else s"${transId}:$coordinateType.${pos}$pastPosPart$seq"
    variant.HGVSDNA = hgvs

  }
  /**
   * Generates HGVS notation at the RNA level.
   *
   * @param variant The DNA variant.
   * @param entries GFF entries containing transcript information.
   * @param transcript The matched transcript GFF entry.
   */
  def HGVSRNA(variant: DnaVariant, entries: Seq[GffEntry], transcript: GffEntry): Unit = {
    // transId : r . position/range ppp? sequence
    val transId = transcript.attributes("transcript_id")
    val seq = HGVSDnaRna.getSeq(variant, transcript.strandPlus).toLowerCase.replace("t", "u")
    val pos = HGVSDnaRna.getTranscriptPosition(variant, entries.find(_.name == "exon"), transId, transcript.strandPlus)
    val pastPosPart = Utils.getPastPositionPart(variant.varType)
    val hgvs = s"${transId}:r.${pos}$pastPosPart$seq"

    variant.HGVSRNA = hgvs
  }

  def HGVSProtein(variant: DnaVariant, entries: Seq[GffEntry], proteinEntry: GffEntry): Unit = {
    // proteinId : p . position/range ppp? sequence
    val proteinId = proteinEntry.attributes("protein_id")
    
  }
}
