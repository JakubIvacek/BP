package cosmic


import cosmic.dataCosmic.GeneCensusVariant

import java.io.{BufferedReader, File, InputStreamReader, PrintWriter}
import java.util.zip.GZIPInputStream

/**
 * Object for converting .tsv GeneCensus cosmic file into .gff file
 */
object TSVtoGFFGeneCensus {

  /**
   * Read the TSV file and convert it to a list of GeneFeature objects
   *
   * @param filePath Path to input file .tsv
   * @return Returns list of loaded entries from .tsv in GeneCensusVariant objects             
   */
  def readTSVGeneCensus(filePath: String): Seq[GeneCensusVariant] = {
    val gzipStream = new GZIPInputStream(new java.io.FileInputStream(filePath))
    val reader = new BufferedReader(new InputStreamReader(gzipStream))


    reader.lines().skip(1).toArray.map { line =>
      val cols = line.toString.split("\t")
      GeneCensusVariant(
        cosmicGeneId = cols(2), 
        chromosome = cols(3), 
        genomeStart = Utils.safeParseLong(cols(4)).getOrElse(0L),
        genomeStop = Utils.safeParseLong(cols(5)).getOrElse(0L),
        tumourTypesSomatic = cols(9), 
        tumourTypesGermline = cols(10), 
        cancerSyndrome = cols(11), 
        tissueType = cols(12), 
        molecularGenetics = cols(13), 
        roleInCancer = cols(14), 
        mutationTypes = cols(15),
      )
    }.toList
  }

  /**
   * Convert GeneCensusVariant objects to GFF format and write to file
   *
   * @param filePath The path where the gff file will be created
   * @param features Loaded GeneCensusVariant objects from .tsv file cosmic
   */
  def writeGFF(filePath: String, features: Seq[GeneCensusVariant]): Unit = {
    val file = new File(filePath)
    if (!file.getParentFile.exists()) {
      file.getParentFile.mkdirs() 
    }
    val writer = new PrintWriter(filePath)
    
    Utils.writeGFFHeader(writer)
    
    features.foreach { feature =>
      val attributes = s"ID=${feature.cosmicGeneId};" +
        s"TUMOUR_TYPES_SOMATIC=${feature.tumourTypesSomatic};" +
        s"TUMOUR_TYPES_GERMLINE=${feature.tumourTypesGermline};" +
        s"CANCER_SYNDROME=${feature.cancerSyndrome};" +
        s"TISSUE_TYPE=${feature.tissueType};" +
        s"MOLECULAR_GENETICS=${feature.molecularGenetics};" +
        s"ROLE_IN_CANCER=${feature.roleInCancer};" +
        s"MUTATION_TYPES=${feature.mutationTypes};"
      
      writer.println(s"${feature.chromosome}\tCosmic\tgene\t${feature.genomeStart}\t${feature.genomeStop}\t.\t+\t.\t$attributes")
    }
    writer.close()
  }

  /**
   * Main method to convert TSV to GFF
   *
   * @param inputTSV  Path to input file .tsv
   * @param outputGFF Path where .gff file will be created                
   */
  def convertTSVToGFF(inputTSV: String, outputGFF: String): Unit = {
    println(s"Converting - $inputTSV to $outputGFF")
    val features = readTSVGeneCensus(inputTSV)
    writeGFF(outputGFF, features)
  }
}

