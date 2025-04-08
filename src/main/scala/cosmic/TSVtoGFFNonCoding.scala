package cosmic

import cosmic.data.NonCodingVariant

import java.io._
import java.util.zip.GZIPInputStream
import scala.collection.mutable
import scala.util.Try

object TSVtoGFFNonCoding {

  def safeParseLong(str: String): Option[Long] = {
    Try(str.toLong).toOption
  }

  // Read TSV file in batches and process NonCodingVariants incrementally
  def readTSVNonCodingVariantsInBatches(filePath: String, batchSize: Int): Iterator[Seq[NonCodingVariant]] = {
    val gzipStream = new GZIPInputStream(new FileInputStream(filePath))
    val reader = new BufferedReader(new InputStreamReader(gzipStream))

    // Skip header line
    reader.readLine()

    new Iterator[Seq[NonCodingVariant]] {
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

      def next(): Seq[NonCodingVariant] = {
        val currentBatch = lineBuffer.take(batchSize).map { line =>
          val cols = line.split("\t")
          NonCodingVariant(
            cosmicGeneId = cols(1),
            transcriptAccession = cols(2),
            cosmicPhenotypeId = cols(6),
            genomicMutationId = cols(7),
            legacyMutationId = cols(8),
            zygosity = cols(9),
            chromosome = cols(10),
            genomeStart = safeParseLong(cols(11)).getOrElse(0L),
            genomeStop = safeParseLong(cols(12)).getOrElse(0L),
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

  // Write GFF header
  def writeGFFHeader(writer: PrintWriter): Unit = {
    writer.println("##gff-version 3")
  }

  // Convert NonCodingVariant objects to GFF format and write to file
  def writeGFF(filePath: String, inputTSV: String, batchSize: Int): Unit = {
    val writer = new PrintWriter(filePath)
    val file = new File(filePath)
    if (!file.getParentFile.exists()) {
      file.getParentFile.mkdirs() // Create the directory if it doesn't exist
    }
    // Write header
    writeGFFHeader(writer)

    // Process the TSV file in batches
    val batches = readTSVNonCodingVariantsInBatches(inputTSV, batchSize)

    // Write each batch to GFF file
    batches.foreach { variants =>
      variants.foreach { variant =>
        // Combine all relevant attributes in the GFF format
        val attributes = s"ID=${variant.genomicMutationId};" +
          s"TRANSCRIPT_ACCESSION=${variant.transcriptAccession};" +
          s"COSMIC_PHENOTYPE_ID=${variant.cosmicPhenotypeId};" +
          s"LEGACY_MUTATION_ID=${variant.legacyMutationId};" +
          s"ZYGOSITY=${variant.zygosity};" +
          s"GENOMIC_WT_ALLELE=${variant.genomicWtAllele};" +
          s"GENOMIC_MUT_ALLELE=${variant.genomicMutAllele};" +
          s"MUTATION_SOMATIC_STATUS=${variant.mutationSomaticStatus};"

        // Write the feature line in GFF format
        writer.println(s"${variant.chromosome}\tCosmic\tnon_coding_variant\t${variant.genomeStart}\t${variant.genomeStop}\t.\t+\t.\t$attributes")
      }
    }

    // Close the writer
    writer.close()
  }

  // Main method to convert TSV to GFF
  def convertTSVToGFF(inputTSV: String, outputGFF: String, batchSize: Int = 10000): Unit = {
    writeGFF(outputGFF, inputTSV, batchSize)
  }
}


