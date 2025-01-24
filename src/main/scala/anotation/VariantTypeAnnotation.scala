package anotation

import data.{DnaVariant, GffEntry, VariantType}
import files.{FastaReader, FileReaderVcf, GFFReader, WriteToMaf}
import hgvs.{HGVSCoding, Utils, CodonAmino}

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
   * @param cdsEntry  The coding sequence (CDS) entry.
   * @return The type of the genetic variant at the protein level.
   */
  def returnVariantTypeProtein(variant: DnaVariant, refAllele: String, altAllele: String, cdsEntry: GffEntry): VariantType = {
    // Get the coding sequence (CDS) from the genome
    val cdsSequence = FastaReader.getSequence(variant.NCBIBuild, cdsEntry.contig, cdsEntry.start, cdsEntry.end, cdsEntry.strandPlus)

    // Calculate variant offset within the CDS
    val variantOffset = if (cdsEntry.strandPlus) {
      (variant.position - cdsEntry.start).toInt
    } else {
      (cdsEntry.end - variant.position).toInt
    }

    // Translate the nucleotide sequences into proteins
    val refProtein = getProteinSequence(cdsSequence, refAllele, variantOffset)
    val altProtein = getProteinSequence(cdsSequence, altAllele, variantOffset)

    // Determine variant type
    if (refProtein.length == altProtein.length && refProtein != altProtein) return VariantType.SNP
    if (refProtein.length > altProtein.length) return VariantType.DEL
    if (refProtein.length < altProtein.length) return VariantType.INS
    if (refProtein != altProtein) return VariantType.INDEL
    if (isDuplication(refProtein, altProtein)) return VariantType.DUP
    if (isRepeatedSequence(refProtein, altProtein)) return VariantType.RPT
    if (isFrameshift(refAllele, altAllele)) return VariantType.FS
    if (isExtension(refProtein, altProtein)) return VariantType.EXT

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
    if (ref.isEmpty || alt.isEmpty || alt.length < ref.length) {
      return false
    }

    // Ensure the alternate sequence starts with the reference sequence
    if (!alt.startsWith(ref)) {
      return false
    }

    // Check if the remaining sequence is composed of a repeat unit
    val remaining = alt.substring(ref.length)

    // Try all possible substrings of the reference as potential repeat units
    (1 to ref.length).exists { unitLength =>
      val repeatUnit = ref.substring(0, unitLength)
      remaining.grouped(unitLength).forall(_ == repeatUnit)
    }
  }

  private def isFrameshift(refAllele: String, altAllele: String): Boolean = {
    val lengthDifference = altAllele.length - refAllele.length
    // Check if the length difference is not divisible by 3
    lengthDifference % 3 != 0
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
    require(variantOffset >= 0 && variantOffset <= cdsSequence.length, "Variant offset is out of range for the CDS sequence.")
    val updatedCds = cdsSequence.substring(0, variantOffset) + allele + cdsSequence.substring(variantOffset + allele.length)
    val trimmedCds = if (updatedCds.length % 3 == 0) {
      updatedCds
    } else {
      updatedCds.substring(0, updatedCds.length - (updatedCds.length % 3))
    }
    CodonAmino.translateDnaToProtein(trimmedCds)
  }
}
