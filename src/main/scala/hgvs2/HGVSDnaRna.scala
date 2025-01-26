package hgvs2

import data.VariantType.{DEL, DUP, INDEL, INS, INV, Other, RPT, SNP}
import data.{DnaVariant, GffEntry, VariantType}

object HGVSDnaRna {

  def getPosition(variant: DnaVariant): String = {
    variant.varType match {
      case SNP => variant.position.toString()     //pos
      case DEL | DUP | INDEL | Other =>           //start_end or pos
        if (variant.refAllele.length == 1) {
          s"${variant.position}"
        } else {
          s"${variant.position}_${variant.position + variant.refAllele.length - 1}"
        }
      case INS => s"${variant.position}_${variant.position + 1}" //start_end
      case RPT | INV => s"${variant.position}_${variant.position + variant.refAllele.length - 1}" //start_end
      case _ => "?"
    }
  }

  def getSeq(variant: DnaVariant): String = {
    variant.varType match {
      case SNP => s"${variant.refAllele}>${variant.altAllele}"
      case DEL => ""
      case DUP => ""
      case INS =>
        val insertedSequence = if (variant.altAllele.startsWith(variant.refAllele)) {
          variant.altAllele.substring(variant.refAllele.length)
        } else {
          variant.altAllele
        }
        insertedSequence
      case INDEL => Utils.indelSequence(variant.refAllele, variant.altAllele)
      case RPT => Utils.repeatedSequence(variant.refAllele, variant.altAllele)
      case _ => ""
    }
  }

  def getTranscriptPosition(variant: DnaVariant, exon: Option[GffEntry], transcriptId: String): String = {
    variant.varType match {
      case SNP => Utils.calculateTranscriptPosition(variant, variant.position.toInt, exon, transcriptId) //pos
      case DEL | DUP | INDEL | Other => //start_end or pos
        val start = Utils.calculateTranscriptPosition(variant, variant.position.toInt, exon, transcriptId)
        if (variant.refAllele.length == 1) {
          s"${start}"
        } else {
          val newPos = variant.position + variant.refAllele.length - 1
          val end = Utils.calculateTranscriptPosition(variant, newPos.toInt, exon, transcriptId)
          s"${start}_${end}"
        }
      case INS =>
        val start = Utils.calculateTranscriptPosition(variant, variant.position.toInt, exon, transcriptId) //start_end
        val end = Utils.calculateTranscriptPosition(variant, variant.position.toInt + 1, exon, transcriptId)
        s"${start}_${end}"
      case INV | RPT =>
        val start = Utils.calculateTranscriptPosition(variant, variant.position.toInt, exon, transcriptId) //start_end
        val newPos = variant.position + variant.refAllele.length - 1
        val end = Utils.calculateTranscriptPosition(variant, newPos.toInt, exon, transcriptId)
        s"${start}_${end}"
      case _ => ""
    }
  }
}
