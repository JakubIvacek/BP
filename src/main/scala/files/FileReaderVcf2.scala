package files

import java.io.File
import scala.collection.mutable.ListBuffer
import htsjdk.variant.vcf.VCFFileReader
import data.DnaVariant

object FileReaderVcf2 {
  private var vcfReader: Option[VCFFileReader] = None
  private var iterator: Option[java.util.Iterator[htsjdk.variant.variantcontext.VariantContext]] = None

  /**
   * Initializes the VCF reader and iterator.
   *
   * @param inputFile Path to the VCF file.
   */
  def open(inputFile: String): Unit = {
    close() // Ensure any existing reader is closed before opening a new one
    vcfReader = Some(new VCFFileReader(new File(inputFile), false))
    iterator = Some(vcfReader.get.iterator())
  }

  /**
   * Reads the next batch of DNA variants from the VCF file.
   *
   * @param batchSize Number of variants to read per batch.
   * @return A ListBuffer of DnaVariants.
   */
  def readBatch(batchSize: Int): ListBuffer[DnaVariant] = {
    val variantList = ListBuffer[DnaVariant]()
    var count = 0

    iterator match {
      case Some(it) =>
        while (it.hasNext && count < batchSize) {
          val variant = it.next()
          val dnaVariants = DnaVariant.createDnaVariants(variant)
          variantList ++= dnaVariants
          count += 1
        }
      case None =>
        throw new IllegalStateException("VCF reader not initialized. Call open() first.")
    }

    variantList
  }

  /**
   * Closes the VCF reader.
   */
  def close(): Unit = {
    vcfReader.foreach(_.close())
    vcfReader = None
    iterator = None
  }
}


