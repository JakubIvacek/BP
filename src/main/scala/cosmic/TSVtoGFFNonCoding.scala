package cosmic

import data.NonCodingCosmic
import java.io._
import java.util.zip.GZIPInputStream
import scala.collection.mutable

/**
 * Object for converting .tsv NonCoding cosmic file into .gff file
 */
object TSVtoGFFNonCoding {

  /**
   * Read the TSV file and convert it to a list of ResistanceMutation objects
   *
   * @param filePath Path to input file .tsv
   * @param batchSize NonCoding needs to be converted in batches because file is too big 
   * @return Returns seq of NonCodingVariants loaded from .tsv file           
   */
  def readTSVNonCodingVariantsInBatches(filePath: String, batchSize: Int): Iterator[Seq[NonCodingCosmic]] = {
    val gzipStream = new GZIPInputStream(new FileInputStream(filePath))
    val reader = new BufferedReader(new InputStreamReader(gzipStream))

    // Skip header line
    reader.readLine()

    new Iterator[Seq[NonCodingCosmic]] {
      var lineBuffer = mutable.Buffer[String]()
      var done = false

      def hasNext: Boolean = {
        if (lineBuffer.isEmpty && !done) {
          var i = 0
          while (i < batchSize && reader.ready() && !done) {
            val line = reader.readLine()
            if (line != null) {
              lineBuffer += line
              i += 1
            } else {
              done = true
            }
          }
        }
        lineBuffer.nonEmpty
      }

      def next(): Seq[NonCodingCosmic] = {
        val currentBatch = lineBuffer.take(batchSize).map { line =>
          val cols = line.split("\t")
          NonCodingCosmic(
            cosmicGeneId = cols(1),
            transcriptAccession = cols(2),
            cosmicPhenotypeId = cols(6),
            genomicMutationId = cols(7),
            legacyMutationId = cols(8),
            zygosity = cols(9),
            chromosome = cols(10),
            genomeStart = Utils.safeParseLong(cols(11)).getOrElse(0L),
            genomeStop = Utils.safeParseLong(cols(12)).getOrElse(0L),
            genomicWtAllele = cols(13),
            genomicMutAllele = cols(14),
            mutationSomaticStatus = cols(15)
          )
        }.toSeq
        lineBuffer = lineBuffer.drop(batchSize)
        currentBatch
      }
    }
  }

  /**
   * Convert NonCoding objects to GFF format and write to file
   *
   * @param filePath The path where the gff file will be created
   * @param inputTSV  Path to input file .tsv
   * @param batchSize NonCoding needs to be converted in batches because file is too big   
   */
  def writeGFF(filePath: String, inputTSV: String, batchSize: Int): Unit = {
    val writer = new PrintWriter(filePath)
    val file = new File(filePath)
    if (!file.getParentFile.exists()) {
      file.getParentFile.mkdirs() 
    }
    Utils.writeGFFHeader(writer)
    
    val batches = readTSVNonCodingVariantsInBatches(inputTSV, batchSize)
    
    batches.foreach { variants =>
      variants.foreach { variant =>
        val attributes = s"ID=${variant.genomicMutationId};" +
          s"TRANSCRIPT_ACCESSION=${variant.transcriptAccession};" +
          s"COSMIC_PHENOTYPE_ID=${variant.cosmicPhenotypeId};" +
          s"LEGACY_MUTATION_ID=${variant.legacyMutationId};" +
          s"ZYGOSITY=${variant.zygosity};" +
          s"GENOMIC_WT_ALLELE=${variant.genomicWtAllele};" +
          s"GENOMIC_MUT_ALLELE=${variant.genomicMutAllele};" +
          s"MUTATION_SOMATIC_STATUS=${variant.mutationSomaticStatus};"
        
        writer.println(s"${variant.chromosome}\tCosmic\tnon_coding_variant\t${variant.genomeStart}\t${variant.genomeStop}\t.\t+\t.\t$attributes")
      }
    }
    writer.close()
  }

  /**
   * Main method to convert TSV to GFF
   *
   * @param inputTSV  Path to input file .tsv
   * @param outputGFF Path where .gff file will be created           
   * @param batchSize NonCoding needs to be converted in batches because file is too big                 
   */
  def convertTSVToGFF(inputTSV: String, outputGFF: String, batchSize: Int = 10000): Unit = {
    println(s"Converting - $inputTSV to $outputGFF")
    writeGFF(outputGFF, inputTSV, batchSize)
  }
}


