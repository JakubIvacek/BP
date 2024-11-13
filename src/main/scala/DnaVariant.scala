import htsjdk.variant.variantcontext.VariantContext
import java.lang.Double.parseDouble
import scala.jdk.CollectionConverters._
// Class to Hold DNA Variants from VCF FILE
class DnaVariant(
                  val contig: String,
                  val position: BigInt,
                  val refAllele: String,
                  val altAllele: String,
                  val alleleFreq: Double,
                  val alleleSomatic: Boolean,
                  val varType: VariantType,
                  val copyNum: Int,
                  val VQSR_score: Double
                ){
  override def toString: String = s"DnaVariant(contig=$contig, position=$position, refAllele=$refAllele, altAllele=$altAllele, alleleFreq=$alleleFreq, alleleSomatic=$alleleSomatic, varType=$varType, copyNum=$copyNum, VQSR_score=$VQSR_score)"
}
// Companion object with needed methods
object DnaVariant{
  //create new variant
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
        copyNum = 0,
        VQSR_score = BigDecimal(variant.getPhredScaledQual).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble
      )
    }
    // Return the list as an immutable List
    resultVariants.toList
  }
  //Determine allele freq
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
  private def getContigNumeric(variant: VariantContext): Int = {
    val contig = if (variant.getContig.startsWith("chr")) {
      variant.getContig.substring(3)  // Remove the "chr" prefix
    } else {
      variant.getContig
    }
    
    val contigValue = contig match {
      case "X" => 23
      case "Y" => 24
      case "M" => 25 
      case num => num.toIntOption.getOrElse(-1)
    }
    contigValue
  }
  //determine variant type
  private def returnVariantType(refAllele: String, altAllele: String): VariantType = {
    val variantType = if (refAllele.length == altAllele.length) {
      VariantType.SNP
    } else {
      VariantType.Indel
    }
    variantType
  }
  //check if variant is somatic
  private def isSomatic(variant: VariantContext): Boolean = {
    val somaticValue = variant.getAttributeAsString("SOMATIC", "");
    var somatic = false
    if (somaticValue != "") {
      somatic = true;
    }
    somatic
  }
}