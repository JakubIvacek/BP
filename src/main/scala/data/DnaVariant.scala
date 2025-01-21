package data

import htsjdk.variant.variantcontext.VariantContext
import scala.jdk.CollectionConverters.*

/**
 * Class representing a DNA variant obtained from a VCF (Variant Call Format) file.
 *
 * @param contig The contig (chromosome) where the variant is located.
 * @param position The position of the variant in the contig.
 * @param refAllele The reference allele in the variant.
 * @param altAllele The alternate allele in the variant.
 * @param alleleFreq The frequency of the alternate allele in the sample.
 * @param alleleSomatic A flag indicating if the variant is somatic (found in a tumor sample).
 * @param varType The type of the variant (SNP or Indel).
 * @param VQSR_score The VQSR score for the variant.
 * @param NCBIBuild The reference genome build (e.g., "hg38").
 *                  
 *  ----- GENCODE ANNOTATION COLUMNS
 *  
 * @param geneID The gene ID associated with the variant
 * @param geneName The gene name associated with the variant
 * @param geneType The gene type associated with the variant
 * @param transID The transcript ID associated with the variant
 * @param transName The transcript name associated with the variant
 * @param transType The transcript type associated with the variant
 * @param exonID The exon ID associated with the variant
 * @param exonNum The exon number associated with the variant
 * @param level The annotation level of the variant
 *
 * ---- HGVS ANNOTATION
 * @param HGVSDNA hgvs coding dna level
 * @param HGVSProtein hgvs coding protein level
 * @param HGVSRNA hgvs coding rna level
 */
case class DnaVariant(
                  val contig: String,
                  val position: BigInt,
                  val refAllele: String,
                  val altAllele: String,
                  val alleleFreq: Double,
                  val alleleSomatic: Boolean,
                  var varType: VariantType,
                  val VQSR_score: Double,
                  var geneID: String,
                  var geneName: String,
                  var geneType: String,
                  var NCBIBuild: String,
                  var transID: String,
                  var transName: String,
                  var exonID: String,
                  var exonNum: String,
                  var transType: String,
                  var level: String,
                  var HGVSDNA: String,
                  var HGVSRNA: String,
                  var HGVSProtein: String
                ){
}

/**
 * Companion object for the DnaVariant class with methods to create and process variants.
 */
object DnaVariant{
  /**
   * Create a list of DnaVariant instances from VCF data
   *
   * @param variant The VariantContext object representing the variant information.
   * @return A list of DnaVariant instances
   */
  def createDnaVariants(variant: VariantContext): List[DnaVariant] = {
    val refAllele = variant.getReference.getBaseString
    val altAlleles = variant.getAlternateAlleles.asScala 
    val alleleFreq = getAlleleFreq(variant)
    
    val resultVariants = scala.collection.mutable.ListBuffer[DnaVariant]()

    // Iterate over all alternate alleles and create a DnaVariant for each one
    altAlleles.foreach { altAllele =>
      resultVariants += new DnaVariant(
        contig = variant.getContig,
        position = variant.getStart,
        refAllele = refAllele,
        altAllele = altAllele.getBaseString,
        alleleFreq = alleleFreq,
        alleleSomatic = DnaVariant.isSomatic(variant),
        varType = DnaVariant.returnVariantType(refAllele, altAllele.getBaseString),
        VQSR_score = BigDecimal(variant.getPhredScaledQual).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble,
        geneID = ".",
        geneName = ".",
        geneType =  ".",
        NCBIBuild = ".",
        transID = ".",
        transName =  ".",
        transType =  ".",
        level =  ".",
        exonNum =  ".",
        exonID =  ".",
        HGVSDNA = ".",
        HGVSRNA = ".",
        HGVSProtein = "."
      )
    }
    // Return the list as an immutable List
    resultVariants.toList
  }
  /**
   * Calculate the allele frequency for a given variant.
   *
   * @param variant The VariantContext object representing the variant data.
   * @return The allele frequency or -1 
   */
  private def getAlleleFreq(variant: VariantContext): Double = {
    var altCount = 0
    var totalDepth = 0

    // Iterate over each genotype to sum AD
    val genotypes = variant.getGenotypes.iterator()
    while (genotypes.hasNext) {
      val genotype = genotypes.next()
      if (genotype.hasAD) {
        val ad = genotype.getAD
        if (ad != null && ad.length > 1) {
          totalDepth += ad.sum
          altCount += ad.tail.sum
        }
      }
    }
    var freq: Double = -1.0
    if (totalDepth > 0) freq = altCount.toDouble / totalDepth
    val rounded = BigDecimal(freq).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble
    rounded
  }

  /**
   * Determine the type of variant (SNP or Indel)
   *
   * @param refAllele The reference allele.
   * @param altAllele The alternate allele.
   * @return (SNP or Indel).
   */
  private def returnVariantType(refAllele: String, altAllele: String): VariantType = {
    if (refAllele.isEmpty || altAllele.isEmpty) {
      return VariantType.Other 
    }
    if (refAllele.length == altAllele.length && refAllele != altAllele) {
      return VariantType.SNP
    }
    // Check DUP
    //CHECK RPT
    //CHECK INV
    //CHECK ALLELES
    //CHECK EXT
    //CHECK FS
    
    if (refAllele.length > altAllele.length) {
      return VariantType.DEL
    }
    if (refAllele.length < altAllele.length) {
      return VariantType.INS
    }
    if (refAllele != altAllele) {
      return VariantType.INDEL
    }
    VariantType.Other
  }

  /**
   * Check if a variant is somatic based on its annotation in the VCF file.
   *
   * @param variant The VariantContext object representing the variant.
   */
  private def isSomatic(variant: VariantContext): Boolean = {
    val somaticValue = variant.getAttributeAsString("SOMATIC", "");
    var somatic = false
    if (somaticValue != "") {
      somatic = true;
    }
    somatic
  }
}