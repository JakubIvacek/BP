import htsjdk.variant.variantcontext.VariantContext

// Class to Hold DNA Variants from VCF FILE
class DnaVariant(
                  val contig: Int,
                  val position: BigInt,
                  val refAllele: String,
                  val altAllele: String,
                  val alleleFreq: Double,
                  val alleleSomatic: Boolean,
                  val varType: VariantType,
                  val copyNum: Option[BigInt], //int
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
    
    new DnaVariant(
      contig = DnaVariant.getContigNumeric(variant),
      position = variant.getStart,
      refAllele = refAllele,
      altAllele = altAllele,
      alleleFreq = getAlleleFreq(variant),
      alleleSomatic = DnaVariant.isSomatic(variant),
      varType = DnaVariant.returnVariantType(refAllele, altAllele),
      copyNum = None,
      VQSR_score = None
    )
  }
  //Determine allele freq
  private def getAlleleFreq(variant: VariantContext): Double = {
    val alleleFreqObj = variant.getAttribute("AF", "-1")
    val alleleFreq = alleleFreqObj.toString
    var alleleFreqValue = -1.0
    try {
      alleleFreqValue = java.lang.Double.parseDouble(alleleFreq)
    } catch {
      case e: NumberFormatException =>
        if (alleleFreq.startsWith("[") && alleleFreq.endsWith("]")) {
          alleleFreqValue = cleanAlleleFreqMultiple(alleleFreq)
        }
    }
    alleleFreqValue
  }
  private def cleanAlleleFreqMultiple(alleleFreq: String): Double = {
    val values = alleleFreq.substring(1, alleleFreq.length - 1).split(",").map(_.trim)
    try {
      // Try parsing the first value as a Double
      val firstValue = values.headOption match {
        case Some(v) =>
          try {
            Some(v.toDouble)
          } catch {
            case _: NumberFormatException => None
          }
        case None => None
      }
      firstValue match {
        case Some(alleleFreqValue) =>
          return alleleFreqValue
        case None =>
          println(s"Invalid number format for the first value in allele frequency: $alleleFreq")
          return -1
      }
    } catch {
      case _: Exception =>
        println(s"Error processing allele frequency: $alleleFreq")
        return -1
    }
  }
  private def getContigNumeric(variant: VariantContext): Int = {
    val contig = if (variant.getContig.startsWith("chr")) {
      variant.getContig.substring(3)  // Remove the "chr" prefix
    } else {
      variant.getContig
    }

    // Map the contig (chromosome) name to a numeric value
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