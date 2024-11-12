import htsjdk.variant.variantcontext.VariantContext
import java.lang.Double.parseDouble
// Class to Hold DNA Variants from VCF FILE
class DnaVariant(
                  val contig: Int,
                  val position: BigInt,
                  val refAllele: String,
                  val altAllele: String,
                  val alleleFreq: Double,
                  val alleleSomatic: Boolean,
                  val varType: VariantType,
                  val copyNum: Int,
                  val VQSR_score: Option[Float] //float
                ){
  override def toString: String = s"DnaVariant(contig=$contig, position=$position, refAllele=$refAllele, altAllele=$altAllele, alleleFreq=$alleleFreq, alleleSomatic=$alleleSomatic, varType=$varType, copyNum=$copyNum, VQSR_score=$VQSR_score)"
}
// Companion object with needed methods
object DnaVariant{
  //create new variant
  def createDnaVariant(variant: VariantContext): DnaVariant = {
    val refAllele = variant.getReference.getBaseString
    val altAllele = variant.getAlternateAlleles.get(0).getBaseString
    val alleleFreq = getAlleleFreq(variant)
    new DnaVariant(
      contig = DnaVariant.getContigNumeric(variant),
      position = variant.getStart,
      refAllele = refAllele,
      altAllele = altAllele,
      alleleFreq = alleleFreq,
      alleleSomatic = DnaVariant.isSomatic(variant),
      varType = DnaVariant.returnVariantType(refAllele, altAllele),
      copyNum = (alleleFreq * 2).toInt,
      VQSR_score = None
    )
  }
  //Determine allele freq
  private def getAlleleFreq(variant: VariantContext): Double = {
    val alleleFreqObj = variant.getAttribute("AF", "0.0")
    val alleleFreq = alleleFreqObj.toString
    var alleleFreqValue = 0.0
    try {
      alleleFreqValue = parseDouble(alleleFreq)
    } catch {
      case e: NumberFormatException =>
        if (alleleFreq.contains(",")) { // Handle multiple values separated by commas
          alleleFreqValue = cleanAlleleFreqMultiple(alleleFreq)
        }
    }
    alleleFreqValue
  }
  private def cleanAlleleFreqMultiple(alleleFreq: String): Double = {
    val values = alleleFreq.substring(1, alleleFreq.length - 1).split(",").map(_.trim)
    parseDouble(values.headOption.getOrElse("0.0"))
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