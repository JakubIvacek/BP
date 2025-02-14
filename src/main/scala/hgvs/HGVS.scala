package hgvs
import anotation.VariantTypeAnnotation.getProteinSequence
import data.VariantType.{DEL, DUP, INDEL, INS, INV, Other, RPT, SNP}
import data.{DnaVariant, GffEntry, VariantType}
import database.modules.ServiceModules
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

    // Protein LEVEL: Mapped to CDS
    // Check if the variant is mapped to a CDS region
    val cdsEntryOpt = entries.find(entry =>
      entry.attributes.contains("protein_id") && entry.attributes.get("gene_type").contains("protein_coding")
    )
    cdsEntryOpt match {
      case Some(cdsEntry) => HGVSProtein(variant, entries, cdsEntry)
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
    val coordinateType = if (entries.exists(entry => 
      entry.attributes.contains("protein_id") && entry.attributes.get("gene_type").contains("protein_coding"))) "c" else "n"

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

  /**
   * Generates HGVS notation at the Protein level.
   *
   * @param variant    The DNA variant.
   * @param entries    GFF entries containing transcript information.
   * @param cdsEntry   The matched CDS GFF entry.
   */
  def HGVSProtein(variant: DnaVariant, entries: Seq[GffEntry], cdsEntry: GffEntry): Unit = {
    // proteinId : p . position/range ppp? sequence
    val proteinId = cdsEntry.attributes("protein_id")
    val coordinate = "p"
    val pastPosPart = Utils.getPastPositionPart(variant.proteinVarType)
    //val faPath = ServiceModules.getReferenceFilePathGenCode(variant.NCBIBuild)
    //val cdsSequence = FastaReader.getSequence(variant.NCBIBuild, cdsEntry.contig, cdsEntry.start, cdsEntry.end, cdsEntry.strandPlus, faPath)
    val cdsSequence = FastaReader.getSequence(variant.NCBIBuild, cdsEntry.contig, cdsEntry.start, cdsEntry.end, cdsEntry.strandPlus)
    // Calculate the variant offset within the CDS based on strand orientation
    val variantOffset = if (cdsEntry.strandPlus) {
      (variant.position - cdsEntry.start).toInt
    } else {
      (cdsEntry.end - variant.position).toInt
    }
    val refProtein = getProteinSequence(cdsSequence, variant.refAllele, variantOffset)
    val altProtein = getProteinSequence(cdsSequence, variant.altAllele, variantOffset)

    val (pos, altAA) = HGVSp.returnProteinHGVS(variant, refProtein, altProtein, variantOffset, cdsEntry.strandPlus, cdsSequence.length)
    val hgvs = if (pos.nonEmpty && altAA.nonEmpty) {
      s"$proteinId:p.$pos$pastPosPart$altAA"
    } else if (variant.proteinVarType == Other) {
      "."
    } else {
      s"$proteinId:p.?" // ? if pos or altAA cant be calculated
    }
    variant.HGVSProtein = hgvs

  }
}
