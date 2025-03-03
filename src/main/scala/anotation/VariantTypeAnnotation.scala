package anotation

import data.{DnaVariant, GffEntry, VariantType}
import files.{FastaReader, FastaReader2, FileReaderVcf, GFFReader, WriteToMaf}
import hgvs.Utils
import hgvs.CodonAmino
import database.modules.ServiceModules
import utils.Gunzip

object VariantTypeAnnotation {
  /**
   * Determines the type of the genetic variant based on reference and alternate alleles
   * at the DNA/RNA level.
   *
   * @param refAllele The reference allele sequence.
   * @param altAllele The alternate allele sequence.
   * @return The type of the genetic variant at the DNA/RNA level.
   */
  def returnVariantTypeDnaRna(refAllele: String, altAllele: String): VariantType = {

    if (refAllele.isEmpty || altAllele.isEmpty) return VariantType.Other

    if (refAllele.length > 1 && refAllele.length == altAllele.length) {
      val reverseComplement = Utils.reverseComplement(refAllele)
      if (reverseComplement == altAllele) return VariantType.INV
    }

    if (refAllele.length == altAllele.length && refAllele != altAllele) return VariantType.SNP
    if (isRepeatedSequence(refAllele, altAllele)) return VariantType.RPT
    if (isDuplication(refAllele, altAllele)) return VariantType.DUP
    if (refAllele.length > altAllele.length) return VariantType.DEL
    if (refAllele.length < altAllele.length) return VariantType.INS
    if (refAllele != altAllele) return VariantType.INDEL
    VariantType.Other
  }

  /**
   * Determines the type of the genetic variant at the protein level.
   *
   * @param variant   The DNA variant information.
   * @param refAllele The reference allele sequence.
   * @param altAllele The alternate allele sequence.
   * @param cdsEntry  The coding sequence entry.
   * @return The type of the genetic variant at the protein level.
   */
  def returnVariantTypeProtein(variant: DnaVariant, refAllele: String, altAllele: String, cdsEntry: GffEntry, faPath: String): VariantType = {
    //val cdsSequence = FastaReader.getSequence(variant.NCBIBuild, cdsEntry.contig, cdsEntry.start, cdsEntry.end, cdsEntry.strandPlus, "")
    
    // Get the coding sequence (CDS) from the genome
    val cdsSequence = FastaReader2.getSequence(faPath, cdsEntry.contig, cdsEntry.start, cdsEntry.end, cdsEntry.strandPlus)
    //println(s"${cdsEntry.start} end ${cdsEntry.end} - length ${cdsSequence.length} - position ${variant.contig} ${variant.position} ${variant.positionEnd}")
    // Calculate variant offset within the CDS
    val variantOffset = if (cdsEntry.strandPlus) {
      (variant.position - cdsEntry.start).toInt
    } else {
      (cdsEntry.end - variant.position).toInt
    }


    // Translate the nucleotide sequences into proteins
    val refProtein = getProteinSequence(cdsSequence, refAllele, variantOffset)
    val altProtein = getProteinSequence(cdsSequence, altAllele, variantOffset)

    // Only check protein level if DNA-level classification is inconclusive
    if (isFrameshift(refAllele, altAllele)) return VariantType.FS
    if (isDuplication(refProtein, altProtein)) return VariantType.DUP
    if (isRepeatedSequence(refProtein, altProtein)) return VariantType.RPT
    if (isExtension(refProtein, altProtein)) return VariantType.EXT

    // Determine variant type
    if (refAllele.length > altAllele.length && altAllele.length == 1) return VariantType.DEL
    if (refAllele.length < altAllele.length && refAllele.length == 1) return VariantType.INS
    if (refAllele.length != altAllele.length) return VariantType.INDEL

    // Fix: SNP should be checked only if refAllele == altAllele at nucleotide level
    if (refProtein.length == altProtein.length && refProtein != altProtein) return VariantType.SNP
    VariantType.Other
  }

  /**
   * Checks if a variant represents a duplication of a sequence (DNA or protein level).
   *
   * @param ref The reference sequence (refAllele or refProtein).
   * @param alt The alternate sequence (altAllele or altProtein).
   * @return Boolean indicating whether the variant is a duplication of the reference sequence.
   */
  private def isDuplication(ref: String, alt: String): Boolean = {
    if (ref.length < 1 || ref.isEmpty || alt.isEmpty || alt.length <= ref.length) {
      return false
    }
    if (!alt.startsWith(ref)) {
      return false
    }
    val remaining = alt.substring(ref.length)
    ref.startsWith(remaining)
  }
  /**
   * Checks if a variant represents a repeated sequence (DNA or protein level).
   *
   * @param ref The reference sequence (refAllele or refProtein).
   * @param alt The alternate sequence (altAllele or altProtein).
   * @return Boolean indicating whether the variant is a repeated sequence.
   */
  def isRepeatedSequence(ref: String, alt: String): Boolean = {
    // If either sequence is empty, or if the alternate sequence is shorter than the reference, it's not a repeat
    if (ref.isEmpty || alt.isEmpty || alt.length < ref.length) {
      return false
    }

    // Ensure the alternate sequence starts with the reference sequence
    if (!alt.startsWith(ref)) {
      return false
    }

    // Extract the remaining part of the alternate sequence after the reference part
    val remaining = alt.substring(ref.length)
    
    val minRepeatLength = 2 

    // Check if the remaining sequence can be divided into multiple repeating units of a given length
    (minRepeatLength to remaining.length / 2).exists { unitLength =>
      // Extract the potential repeat unit
      val repeatUnit = remaining.substring(0, unitLength)

      // Ensure that the remaining sequence is composed of this repeat unit
      remaining.grouped(unitLength).forall(_ == repeatUnit)
    }
  }


  /**
   * Calculates the end position of a genetic variant based on its type.
   *
   * @param variant The DnaVariant instance containing details of the genetic variant.
   * @return The end position as a BigInt.
   */
  def calculateEndPosition(variant: DnaVariant): BigInt = {
    variant.varType match {
      case VariantType.SNP  =>
        variant.position
      case VariantType.INS =>
        variant.position + 1
      case VariantType.DEL | VariantType.INDEL =>
        variant.position + variant.refAllele.length - 1
      case VariantType.DUP | VariantType.RPT | VariantType.INV =>
        variant.position + variant.altAllele.length - 1
      case _ =>
        variant.position + variant.altAllele.length - 1
    }
  }

  private def isFrameshift(refAllele: String, altAllele: String): Boolean = {
    // Frameshift occurs when the length difference is NOT a multiple of 3
    (altAllele.length - refAllele.length) % 3 != 0
  }


  private def isExtension(refProtein: String, altProtein: String): Boolean = {
    // Check for C-terminal extension
    if (altProtein.startsWith(refProtein) && altProtein.length > refProtein.length) {
      return true
    }
    // Check for N-terminal extension
    if (altProtein.endsWith(refProtein) && altProtein.length > refProtein.length) {
      return true
    }
    false
  }

  def getProteinSequence(cdsSequence: String, allele: String, variantOffset: Int): String = {
    require(variantOffset >= 0 && variantOffset <= cdsSequence.length, s"Variant offset is out of range for the CDS sequence. $variantOffset length ${cdsSequence.length}")
    val endOffset = variantOffset + allele.length
    val safeEndOffset = math.min(endOffset, cdsSequence.length)

    // Modify the CDS sequence by inserting the allele at the specified offset
    val updatedCds = cdsSequence.substring(0, variantOffset) + allele + cdsSequence.substring(safeEndOffset)
    val trimmedCds = if (updatedCds.length % 3 == 0) {
      updatedCds
    } else {
      updatedCds.substring(0, updatedCds.length - (updatedCds.length % 3))
    }
    CodonAmino.translateDnaToProtein(trimmedCds)
  }
}
