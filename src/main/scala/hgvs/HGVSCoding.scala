package hgvs

import data.{DnaVariant, GffEntry, VariantType}

/**
 * The HGVS Nomenclature is an internationally-recognized standard for the description of DNA, RNA, and protein
 * sequence variants. It is used to convey variants in clinical reports and to share variants
 * in publications and databases.
 * SYNTAX -> https://hgvs-nomenclature.org/
 * Adds for variant -> DNA, RNA, Protein level HGVS description
 */
object HGVSCoding {
  def variantAddHGVS(variant: DnaVariant, entries: Seq[GffEntry]): Unit = {
      variant.varType match{
        case VariantType.SNP => variantSNPHGVS(variant, entries)
        case VariantType.DEL => variantDELHGVS(variant, entries)
        case VariantType.INS => variantINSHGVS(variant, entries)
        case VariantType.INDEL => variantDELINSHGVS(variant, entries)
        // AJ OSTATNE POTOM DUP, INV, RPT, ALLELES, EXT, FS
        case VariantType.Other => variantOtherHGVS(variant, entries)
        case _ =>
    }
  }

  /**
   * SNP variant HGVS annotation add
   * DNA-level HGVS
   * RNA-level HGVS
   * Protein-level HGVS
   */
  private def variantSNPHGVS(variant: DnaVariant, entries: Seq[GffEntry]): Unit = {
    val relevantEntry = entries.find(e => e.name == "transcript")
    val cdsEntry = entries.find(e => e.name == "CDS")
    val exon = entries.find(e => e.name == "exon")
    //Add DNA-level, RNA-level HGVS
    relevantEntry match {
      case Some(entry) =>
        variant.HGVSDNA = HGVSSnp.generateDnaTransHgvsSNP(variant, entry, exon) //Add DNA-level c.
        variant.HGVSRNA = HGVSSnp.generateRnaHgvsSNP(variant, entry, exon) //Add RNA-level HGVS r.
      case None =>
        variant.HGVSDNA = HGVSSnp.generateDnaHgvsSNP(variant) //Add DNA-level g.
    }
    //Add Protein-level HGVS
    cdsEntry match {
      case Some(cds) => variant.HGVSProtein = HGVSSnp.generateProteinHgvsPredictedSNP(variant, cds)
      case _ =>
    }
  }
  /**
   * DEL variant HGVS annotation add
   * DNA-level HGVS
   * RNA-level HGVS
   * Protein-level HGVS
   */
  private def variantDELHGVS(variant: DnaVariant, entries: Seq[GffEntry]): Unit = {
    //val relevantEntry = entries.find(e => e.attributes.contains("transcript_id"))
    val relevantEntry = entries.find(e => e.name == "transcript")
    val cdsEntry = entries.find(e => e.name == "CDS")
    val exon = entries.find(e => e.name == "exon")
    variant.HGVSDNA = HGVSDel.generateDelHgvsDNA(variant, relevantEntry, exon)     
    relevantEntry match {
      case Some(entry) =>
        variant.HGVSRNA = HGVSDel.generateDelHgvsRNA(variant, entry, exon)     //Add RNA-level HGVS r.
      case None =>
    }
    //Add Protein-level HGVS p.
    cdsEntry match {
      case Some(cds) => variant.HGVSProtein = HGVSDel.generateDelHgvsProtein(variant, cds)
      case _ =>
    }
  }

  /**
   * INS variant HGVS annotation add
   * DNA-level HGVS
   * RNA-level HGVS
   * Protein-level HGVS
   */
  private def variantINSHGVS(variant: DnaVariant, entries: Seq[GffEntry]): Unit = {
    val relevantEntry = entries.find(e => e.name == "transcript")
    val cdsEntry = entries.find(e => e.name == "CDS")
    val exon = entries.find(e => e.name == "exon")
    variant.HGVSDNA = HGVSIns.generateInsHgvsDNA(variant, relevantEntry, exon)  //Add DNA-level HGVS g. c.
    relevantEntry match {
      case Some(entry) =>
        variant.HGVSRNA = HGVSIns.generateInsHgvsRNA(variant, entry, exon)  //Add RNA-level HGVS r.
      case None =>
    }
    //Add Protein-level HGVS p.
    cdsEntry match {
      case Some(cds) => variant.HGVSProtein = HGVSIns.generateInsHgvsProtein(variant, cds)
      case _ =>
    }
  }

  /**
   * DELINS variant HGVS annotation add
   * DNA-level HGVS
   * RNA-level HGVS
   * Protein-level HGVS
   */
  private def variantDELINSHGVS(variant: DnaVariant, entries: Seq[GffEntry]): Unit = {
    val relevantEntry = entries.find(e => e.name == "transcript")
    val cdsEntry = entries.find(e => e.name == "CDS")
    val exon = entries.find(e => e.name == "exon")
    variant.HGVSDNA = HGVSDelins.generateDelinsHgvsDNA(variant, relevantEntry, exon)  //Add DNA-level HGVS g. c.
    relevantEntry match {
      case Some(entry) =>
        variant.HGVSRNA = HGVSDelins.generateDelinsHgvsRNA(variant, entry, exon)  //Add RNA-level HGVS r.
      case None =>
    }
    //Add Protein-level HGVS p.
    cdsEntry match {
      case Some(cds) => variant.HGVSProtein = HGVSDelins.generateDelinsHgvsProtein(variant, cds)
      case _ =>
    }
  }

  /**
   * Other variant HGVS annotation add
   * DNA-level HGVS only
   */
  private def variantOtherHGVS(variant: DnaVariant, entries: Seq[GffEntry]): Unit = {
    val relevantEntry = entries.find(e => e.name == "transcript")
    val cdsEntry = entries.find(e => e.name == "CDS")
    val exon = entries.find(e => e.name == "exon")
    val dnaHgvs = relevantEntry match {
      case Some(entry) =>
        val rnaPosition = exon match {
          case Some(exon) => Utils.calculateTranscriptPosition(variant.position, exon)
          case None => Utils.getTranscriptPosition(variant, variant.contig, entry.attributes("transcript_id"))
        }
        if (variant.refAllele.length == 1) {
          s"${entry.attributes("transcript_id")}:c.${rnaPosition}="
        } else {
          val rnaPositionEnd = exon match {
            case Some(exon) => Utils.calculateTranscriptPosition(variant.position + variant.refAllele.length - 1, exon)
            case None =>
              val adjustedPosition = if (entry.strandPlus) {variant.position + variant.refAllele.length - 1} else {variant.position - variant.refAllele.length + 1}
              Utils.getTranscriptPosition(variant.copy(position = adjustedPosition), variant.contig, entry.attributes("transcript_id"))
          }
          s"${entry.attributes("transcript_id")}:c.${rnaPosition}_${rnaPositionEnd}="
        }
      case None =>
        if (variant.refAllele.length == 1) {
          s"${variant.contig}:g.${variant.position}="
        } else {
          s"${variant.contig}:g.${variant.position}_${variant.position + variant.refAllele.length - 1}="
        }
    }
    variant.HGVSDNA = dnaHgvs
    variant.HGVSRNA = "."
    variant.HGVSProtein = "."
  }
}
