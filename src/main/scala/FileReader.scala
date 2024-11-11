import htsjdk.variant.vcf.{VCFFileReader, VCFHeader}
import htsjdk.variant.variantcontext.VariantContext
import scala.math.Numeric.Implicits.infixNumericOps

import java.io.File
import scala.collection.mutable.ListBuffer
import FileWriter.writeVcfFile

import java.util
object FileReader {
  def main(args: Array[String]): Unit = {
    // Specify the path to your input VCF file
    val vcfFilePath = "Lynch.1121.03.T.02.vcf"

    try {
      // Create a VCFFileReader instance to read the VCF file
      val vcfReader = new VCFFileReader(new File(vcfFilePath), false)
      val vcfHeader: VCFHeader = vcfReader.getFileHeader
      // Iterate over all VariantContext entries (each representing a variant in the VCF file)
      var variantList: ListBuffer[DnaVariant] = ListBuffer()
      vcfReader.iterator().forEachRemaining { variant =>
        val dnaVariant = createDnaVariant(variant)
        variantList += dnaVariant
        if (count < 100) {
          println(dnaVariant.toString)
        }
        count += 1
      }

    } catch {
      case e: Exception =>
        println(s"An error occurred while reading the VCF file: ${e.getMessage}")
    }
  }
}
var count = 0

def createDnaVariant(variant: htsjdk.variant.variantcontext.VariantContext): DnaVariant = {
  //if starts with chr remove
  val chrom = if (variant.getContig.startsWith("chr")) {
    variant.getContig.substring(3)
  } else {
    variant.getContig
  }
  //change x,y,m , parse to int
  val contigValue = chrom match {
    case "X" => 23
    case "Y" => 24
    case "M" => 25 // 25 "chrM"
    case num => num.toIntOption.getOrElse(-1)
  }
  //determine variant type
  val refAllele = variant.getReference.getBaseString
  val altAllele = variant.getAlternateAlleles.get(0).getBaseString
  val variantType = if (refAllele.length == altAllele.length) {
    VariantType.SNP
  } else {
    VariantType.Indel
  }
  //determine allele freq
  val alleleFreqObj = variant.getAttribute("AF", "-1") // Retrieve allele frequency as Object
  val alleleFreq = alleleFreqObj.toString // Convert to String
  var alleleFreqValue = -1.0 //default
  try {
    // Convert the alleleFreq to a Double using Double.parseDouble
    alleleFreqValue = java.lang.Double.parseDouble(alleleFreq)
  } catch {
    case e: NumberFormatException =>
      if (alleleFreq.startsWith("[") && alleleFreq.endsWith("]")) {
        alleleFreqValue = cleanAlleleFreqMultiple(alleleFreq)
      }
  }
  //create new variant
  new DnaVariant(
    contig = contigValue,
    position = variant.getStart,
    refAllele = refAllele,
    altAllele = altAllele,
    alleleFreq = alleleFreqValue,
    alleleSomatic = false, 
    varType = variantType,
    copyNum = None, 
    VQSR_score = None 
  )
}
// if alleleFreq is List , ArrayList get first value
def cleanAlleleFreqMultiple(alleleFreq: String): Double = {
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