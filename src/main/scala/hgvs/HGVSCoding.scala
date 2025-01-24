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
      // ADD DNA/RNA LEVEL NOTATION
      variant.varType match{
        case VariantType.SNP => variantSNPHGVS(variant, entries)
        case VariantType.DEL => variantDELHGVS(variant, entries)
        case VariantType.INS => variantINSHGVS(variant, entries)
        case VariantType.INDEL => variantDELINSHGVS(variant, entries)
        case VariantType.DUP => variantDUPSHGVS(variant, entries)
        case VariantType.INV => variantINVSHGVS(variant, entries)
        case VariantType.RPT =>
        case VariantType.Other => variantOtherHGVS(variant, entries)
        case _ =>
      }
      // ADD PROTEIN LEVEL NOTATION
      val protEntry = entries.find(entry => entry.attributes.contains("protein_id"))
      if (protEntry.isDefined) {
        val proteinEntry = protEntry.get
        variant.proteinVarType match {
          case VariantType.SNP => variantSNPHGVSProtein(variant, proteinEntry)
          case VariantType.DEL => variantSNPHGVSProtein(variant, proteinEntry)
          case VariantType.INS => variantSNPHGVSProtein(variant, proteinEntry)
          case VariantType.INDEL => variantSNPHGVSProtein(variant, proteinEntry)
          case VariantType.DUP => variantDUPHGVSProtein(variant, proteinEntry)
          case VariantType.EXT =>
          case VariantType.RPT =>
          case VariantType.FS =>
          case _ =>
        }
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
  }

  /**
   * SNP variant HGVS annotation add
   * Protein-level HGVS
   */
  private def variantSNPHGVSProtein(variant: DnaVariant, cds: GffEntry): Unit = {
    variant.HGVSProtein = HGVSSnp.generateProteinHgvsPredictedSNP(variant, cds)
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
  }

  /**
   * DEL variant HGVS annotation add
   * Protein-level HGVS
   */
  private def variantDELHGVSProtein(variant: DnaVariant, cds: GffEntry): Unit = {
    variant.HGVSProtein = HGVSDel.generateDelHgvsProtein(variant, cds)
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
  }

  /**
   * INS variant HGVS annotation add
   * Protein-level HGVS
   */
  private def variantINSHGVSProtein(variant: DnaVariant, cds: GffEntry): Unit = {
    variant.HGVSProtein = HGVSIns.generateInsHgvsProtein(variant, cds)
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
  }
  /**
   * DELINS variant HGVS annotation add
   * Protein-level HGVS
   */
  private def variantDELINSHGVSProtein(variant: DnaVariant, cds: GffEntry): Unit = {
    variant.HGVSProtein = HGVSIns.generateInsHgvsProtein(variant, cds)
  }

  /**
   * INV variant HGVS annotation add
   * DNA-level HGVS
   * RNA-level HGVS
   */
  private def variantINVSHGVS(variant: DnaVariant, entries: Seq[GffEntry]): Unit = {
    val relevantEntry = entries.find(e => e.name == "transcript")
    val cdsEntry = entries.find(e => e.name == "CDS")
    val exon = entries.find(e => e.name == "exon")
    variant.HGVSDNA = HGVSInv.generateInvHgvsDNA(variant, relevantEntry, exon) //Add DNA-level HGVS g. c.
    relevantEntry match {
      case Some(entry) =>
        variant.HGVSRNA = HGVSInv.generateInvHgvsRNA(variant, entry, exon) //Add RNA-level HGVS r.
      case None =>
    }
  }

  /**
   * DUP variant HGVS annotation add
   * DNA-level HGVS
   * RNA-level HGVS
   */
  private def variantDUPSHGVS(variant: DnaVariant, entries: Seq[GffEntry]): Unit = {
    val relevantEntry = entries.find(e => e.name == "transcript")
    val cdsEntry = entries.find(e => e.name == "CDS")
    val exon = entries.find(e => e.name == "exon")
    variant.HGVSDNA = HGVSDup.generateDupHgvsDNA(variant, relevantEntry, exon) //Add DNA-level HGVS g. c.
    relevantEntry match {
      case Some(entry) =>
        variant.HGVSRNA = HGVSDup.generateDUPHgvsRNA(variant, entry, exon) //Add RNA-level HGVS r.
      case None =>
    }
  }

  /**
   * DUP variant HGVS annotation add
   * Protein-level HGVS
   */
  private def variantDUPHGVSProtein(variant: DnaVariant, cds: GffEntry): Unit = {
    variant.HGVSProtein = HGVSDup.generateDupHgvsProtein(variant, cds)
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
