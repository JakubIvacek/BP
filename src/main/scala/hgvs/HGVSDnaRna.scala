package hgvs

import data.VariantType.{DEL, DUP, INDEL, INS, INV, Other, RPT, SNP}
import data.{DnaVariant, GffEntry, VariantType}

object HGVSDnaRna {
  
  /**
   * Retrieves the genomic position or range for a DNA variant.
   *
   * @param variant The DNA variant.
   * @return A string representation of the genomic position or range.
   */
  def getPosition(variant: DnaVariant): String = {
    variant.varType match {
      case SNP =>
        variant.position.toString
      case DEL | INDEL | Other =>
        if (variant.refAllele.length == 1) {
          variant.position.toString
        } else {
          s"${variant.position}_${variant.positionEnd}"
        }
      case DUP =>
        if (variant.altAllele.length == 1) {
          s"${variant.position}"
        } else {
          s"${variant.position}_${variant.positionEnd}"
        }
      case INS => s"${variant.position}_${variant.position + 1}"
      case RPT | INV => s"${variant.position}_${variant.positionEnd}"
      case _ => "?"
    }
  }

  /**
   * Calculates the transcript-level position or range for a DNA variant.
   *
   * @param variant The DNA variant.
   * @param exon Optional GFF entry for the exon.
   * @param transcriptId The transcript ID.
   * @param strandPlus Whether the strand is positive or negative.
   * @return A string representing the transcript position or range.
   */
  def getTranscriptPosition(variant: DnaVariant, exon: Option[GffEntry], transcriptId: String, strandPlus: Boolean): String = {
    variant.varType match {
      case VariantType.SNP =>
        Utils.calculateTranscriptPosition(variant, variant.position.toInt, exon, transcriptId) // pos

      case VariantType.DEL | VariantType.INDEL | VariantType.Other =>
        if (variant.refAllele.length == 1) {
          Utils.calculateTranscriptPosition(variant, variant.position.toInt, exon, transcriptId)
        } else {
          calculatePosition(variant, exon, transcriptId, strandPlus)
        }

      case VariantType.INS =>
        val posStart = if (strandPlus) variant.position.toInt - 1 else variant.position.toInt
        val posEnd = if (strandPlus) variant.position.toInt else variant.position.toInt - 1
        val start = Utils.calculateTranscriptPosition(variant, posStart, exon, transcriptId)
        val end = Utils.calculateTranscriptPosition(variant, posEnd, exon, transcriptId)
        s"${start}_${end}"
      case VariantType.DUP =>
        if (variant.altAllele.length == 1) {
          val pos = Utils.calculateTranscriptPosition(variant, variant.position.toInt, exon, transcriptId)
          s"${pos}"
        } else {
          calculatePosition(variant, exon, transcriptId, strandPlus)
        }
      case VariantType.INV | VariantType.RPT =>
        calculatePosition(variant, exon, transcriptId, strandPlus)

      case _ => "?"
    }
  }


  /**
   * Helper function to calculate transcript-level start and end positions for a variant.
   *
   * @param variant      The DNA variant.
   * @param exon         Optional GFF entry for the exon.
   * @param transcriptId The transcript ID.
   * @param strandPlus   Whether the strand is positive or negative.
   * @return A string representing the transcript position range.
   */
  def calculatePosition(variant: DnaVariant, exon: Option[GffEntry], transcriptId: String, strandPlus: Boolean): String = {
    val length = if (variant.varType == VariantType.DUP || variant.varType == VariantType.RPT) variant.altAllele.length else variant.refAllele.length
    val (posStart, posEnd) = if (strandPlus) {
      (variant.position.toInt, variant.position.toInt + length - 1)
    } else {
      (variant.position.toInt, variant.position.toInt - length + 1)
    }
    val start = Utils.calculateTranscriptPosition(variant, posStart, exon, transcriptId)
    val end = Utils.calculateTranscriptPosition(variant, posEnd, exon, transcriptId)
    s"${start}_${end}"
  }

  /**
   * Retrieves the sequence change caused by the variant.
   *
   * @param variant    The DNA variant.
   * @param strandPlus Whether the strand is positive or negative.
   * @return A string representing the sequence change.
   */
  def getSeq(variant: DnaVariant, strandPlus: Boolean): String = {
    variant.varType match {
      case SNP =>
        if strandPlus then s"${variant.refAllele}>${variant.altAllele}"
        else s"${Utils.reverseComplement(variant.refAllele)}>${Utils.reverseComplement(variant.altAllele)}"
      case DEL => ""
      case DUP => ""
      case INS =>
        if (variant.altAllele.startsWith(variant.refAllele)) {
          val insertedSequence = variant.altAllele.substring(variant.refAllele.length)
          if (strandPlus) insertedSequence
          else Utils.reverseComplement(insertedSequence)
        } else {
          if (strandPlus) variant.altAllele
          else Utils.reverseComplement(variant.altAllele)
        }
      case INDEL =>
        val indelSeq = Utils.indelSequence(variant.refAllele, variant.altAllele)
        if (strandPlus) indelSeq
        else Utils.reverseComplement(indelSeq)
      case RPT =>
        val rptSeq = Utils.repeatedSequence(variant.refAllele, variant.altAllele)
        if (strandPlus) rptSeq
        else Utils.reverseComplement(rptSeq)
      case _ => ""
    }
  }
}
