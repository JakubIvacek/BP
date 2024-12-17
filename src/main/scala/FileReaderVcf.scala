import htsjdk.variant.variantcontext.VariantContext
import htsjdk.variant.vcf.{VCFFileReader, VCFHeader}

import java.io.File
import java.util
import scala.collection.mutable.ListBuffer

object FileReaderVcf {
  //Read file return loaded DnaVariants
  def read(inputFile: String): ListBuffer[DnaVariant] = {
    val vcfFilePath = inputFile
    var variantList: ListBuffer[DnaVariant] = ListBuffer()
    try {
      // Create a VCFFileReader instance to read the VCF file
      val vcfReader = new VCFFileReader(new File(vcfFilePath), false)
      vcfReader.iterator().forEachRemaining { variant =>
        val dnaVariants = DnaVariant.createDnaVariants(variant)
        // Add all the DnaVariants to the variantList
        variantList ++= dnaVariants
      }
    } catch {
      case e: Exception =>
        println(s"An error occurred while reading the VCF file: ${e.getMessage}")
    }
    variantList
  }
}