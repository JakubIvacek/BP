package hgvs

import data.{DnaVariant, GffEntry, VariantType}

object HGVSCoding {
  def variantAddHGVS(variant: DnaVariant, entries: Seq[GffEntry]): Unit = {
      variant.varType match{
        case VariantType.SNP => variantSNPHGVS(variant, entries)
        case VariantType.DEL => variantDELHGVS(variant, entries)
        case VariantType.INS =>
        case VariantType.INDEL =>
        // AJ OSTATNE POTOM DUP, INV, RPT, ALLELES, EXT, FS
        case VariantType.Other =>
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
    val geneEntry = entries.find(e => e.name == "gene")
    val transcriptEntry = entries.find(e => e.name == "transcript")
    val cdsEntry = entries.find(e => e.name == "CDS")
    val exonEntry = entries.find(e => e.name == "exon")

    //Add DNA-level, RNA-level HGVS
    transcriptEntry match {
      case Some(transcript) =>
        variant.HGVSDNA = HGVSSnp.generateTranscriptHgvsSNP(variant, transcript) //Add DNA-level c.
        variant.HGVSRNA = HGVSSnp.generateRnaHgvsSNP(variant, transcript) //Add RNA-level HGVS
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
    val geneEntry = entries.find(e => e.name == "gene")
    val transcriptEntry = entries.find(e => e.name == "transcript")
    val cdsEntry = entries.find(e => e.name == "CDS")
    val exonEntry = entries.find(e => e.name == "exon")
    variant.HGVSDNA = HGVSDel.generateDelHgvsDNA(variant)
    transcriptEntry match {
      case Some(transcript) =>
        variant.HGVSRNA = HGVSDel.generateDelHgvsRNA(variant, transcript)
      case None =>
    }
    //Add Protein-level HGVS
    cdsEntry match {
      case Some(cds) => variant.HGVSProtein = HGVSSnp.generateProteinHgvsPredictedSNP(variant, cds)
      case _ =>
    }
  }
}
