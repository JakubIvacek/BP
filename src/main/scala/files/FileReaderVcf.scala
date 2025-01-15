package files

import data.DnaVariant
import htsjdk.variant.variantcontext.VariantContext
import htsjdk.variant.vcf.{VCFFileReader, VCFHeader}

import java.io.File
import java.util
import scala.collection.mutable.ListBuffer

/**
 * The `FileReaderVcf` object is responsible for reading files in (VCF) format and loading them into DnaVariants List
 *
 */
object FileReaderVcf {
  /**
   * Write a list of DNA variants to a MAF file.
   *
   * @param inputFile  path to VCF file on local device
   * @return returns dna Variants loaded in ListBuffer[DnaVariant] 
   */
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