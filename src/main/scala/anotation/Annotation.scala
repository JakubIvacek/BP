package anotation

import data.DnaVariant
import files.{FileReaderVcf, WriteToMaf, GFFReaderSW}


/**
 * The `Annotation` object is responsible for annotating DNA variants with genomic information from VCF files
 * data sources and outputting the results in the MAF format.
 */
object Annotation {


  /**
   * Main method to process the input VCF file, annotate the variants, and write the result to an output MAF file.
   *
   * @param inputFile       The path to the input VCF file containing DNA variants.
   * @param outputFile      The path to the output MAF file where the annotated variants will be written.
   * @param referenceGenome The reference genome used for annotation (e.g., "hg38").
   * @param batchSize       Size of vcf batch to annotate at once                       
   */
  def annotateInBatches(inputFile: String, outputFile: String, referenceGenome: String, batchSize: Int = 1000): Unit = {
    FileReaderVcf.open(inputFile) // Open the VCF reader once

    var batchCount = 1
    var hasMoreVariants = true

    while (hasMoreVariants) {

      val dnaVariants = FileReaderVcf.readBatch(batchSize)
      if (dnaVariants.isEmpty) {
        hasMoreVariants = false
      } else {
        println(s"Processing batch $batchCount... ${dnaVariants.toList.head.contig}")
        annotateVariants(dnaVariants.toList, referenceGenome)

        WriteToMaf.writeMafFile(dnaVariants, outputFile, append = batchCount > 1)
        batchCount += 1
      }
    }

    // Cleanup
    FileReaderVcf.close() // Close the VCF reader when done
    GFFReaderSW.close() // Close GFFReader once after processing all batches
  }
  /**
   * Annotate a list of DNA variants
   *
   * @param dnaVariants     A list of DNA variants to annotate.
   * @param referenceGenome The reference genome to use for annotation (e.g., "hg38").
   */
  def annotateVariants(dnaVariants: List[DnaVariant], referenceGenome: String): Unit = {
    dnaVariants.foreach(variant =>
      AnnotationGencode.annotateVariantGencode(variant, referenceGenome) //GENCODE
    )
    dnaVariants.foreach(
      variant => Annotation1000Genomes.annotateVariant1000Genomes(variant, referenceGenome)   //1000GENOMES))
    )
    
    dnaVariants.foreach(
      variant => AnnotationCosmic.annotateVariantCosmic(variant, referenceGenome) // COSMIC
    )
  }
}
