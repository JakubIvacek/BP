package cosmic


import cosmic.dataCosmic.GeneCensusVariant

import java.io.{BufferedReader, File, InputStreamReader, PrintWriter}
import java.util.zip.GZIPInputStream
import scala.io.Source
import scala.util.Try


object TSVtoGFFGeneCensus {

  def safeParseLong(str: String): Option[Long] = {
    Try(str.toLong).toOption
  }
  // Read the TSV file and convert it to a list of GeneFeature objects
  def readTSVGeneCensus(filePath: String): Seq[GeneCensusVariant] = {
    val gzipStream = new GZIPInputStream(new java.io.FileInputStream(filePath))
    val reader = new BufferedReader(new InputStreamReader(gzipStream))


    reader.lines().skip(1).toArray.map { line =>
      val cols = line.toString.split("\t")
      GeneCensusVariant(
        cosmicGeneId = cols(2), // COSMIC_GENE_ID
        chromosome = cols(3), // CHROMOSOME
        genomeStart = safeParseLong(cols(4)).getOrElse(0L),
        genomeStop = safeParseLong(cols(5)).getOrElse(0L),
        tumourTypesSomatic = cols(9), // TUMOUR_TYPES_SOMATIC
        tumourTypesGermline = cols(10), // TUMOUR_TYPES_GERMLINE
        cancerSyndrome = cols(11), // CANCER_SYNDROME
        tissueType = cols(12), // TISSUE_TYPE
        molecularGenetics = cols(13), // MOLECULAR_GENETICS
        roleInCancer = cols(14), // ROLE_IN_CANCER
        mutationTypes = cols(15), // MUTATION_TYPES
      )
    }.toList
  }
  // Write GFF header
  def writeGFFHeader(writer: PrintWriter): Unit = {
    writer.println("##gff-version 3")
  }

  // Convert GeneFeature objects to GFF format and write to file
  def writeGFF(filePath: String, features: Seq[GeneCensusVariant]): Unit = {
    val file = new File(filePath)
    if (!file.getParentFile.exists()) {
      file.getParentFile.mkdirs() // Create the directory if it doesn't exist
    }
    val writer = new PrintWriter(filePath)

    // Write header
    writeGFFHeader(writer)

    // Write each feature in GFF format
    features.foreach { feature =>
      // Combine all relevant attributes in the GFF format
      val attributes = s"ID=${feature.cosmicGeneId};" +
        s"TUMOUR_TYPES_SOMATIC=${feature.tumourTypesSomatic};" +
        s"TUMOUR_TYPES_GERMLINE=${feature.tumourTypesGermline};" +
        s"CANCER_SYNDROME=${feature.cancerSyndrome};" +
        s"TISSUE_TYPE=${feature.tissueType};" +
        s"MOLECULAR_GENETICS=${feature.molecularGenetics};" +
        s"ROLE_IN_CANCER=${feature.roleInCancer};" +
        s"MUTATION_TYPES=${feature.mutationTypes};"

      // Write the feature line in GFF format
      writer.println(s"${feature.chromosome}\tCosmic\tgene\t${feature.genomeStart}\t${feature.genomeStop}\t.\t+\t.\t$attributes")
    }

    // Close the writer
    writer.close()
  }

  // Main method to convert TSV to GFF
  def convertTSVToGFF(inputTSV: String, outputGFF: String): Unit = {
    println(s"Converting - $inputTSV to $outputGFF")
    val features = readTSVGeneCensus(inputTSV)
    writeGFF(outputGFF, features)
  }
}

// Example usage
//object Main extends App {
  //TSVtoGFFGeneCensusCOSMIC.convertTSVToGFF("data/cosmic/v101/hg38/Cosmic_CancerGeneCensus_v101_GRCh38.tsv.gz", "output.gff")
  //LiftOverTool.liftOverGFF("output.gff", "data", "output_t2t.gff")
//}

