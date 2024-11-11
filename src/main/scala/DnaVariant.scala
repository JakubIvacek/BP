import scala.beans.BeanProperty

enum VariantType {
  case SNP, Indel, Other
}

// Class to Hold DNA Variant Information from files
class DnaVariant(
                  @BeanProperty val contig: Int,
                  @BeanProperty val position: BigInt,
                  @BeanProperty val refAllele: String,
                  @BeanProperty val altAllele: String, 
                  @BeanProperty val alleleFreq: Double,
                  @BeanProperty val alleleSomatic: Boolean,
                  @BeanProperty val varType: VariantType,
                  @BeanProperty val copyNum: Option[BigInt], //int
                  @BeanProperty val VQSR_score: Option[Float] //float
                ){
  override def toString: String = s"DnaVariant(contig=$contig, position=$position, refAllele=$refAllele, altAllele=$altAllele, alleleFreq=$alleleFreq, alleleSomatic=$alleleSomatic, varType=$varType, copyNum=$copyNum, VQSR_score=$VQSR_score)"
}