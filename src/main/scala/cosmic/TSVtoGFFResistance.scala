package cosmic

import cosmic.dataCosmic.ResistanceMutation

import java.io.{BufferedReader, File, InputStreamReader, PrintWriter}
import java.util.zip.GZIPInputStream
import scala.io.Source
import scala.util.Try

/**
 * Object for converting .tsv Resistance cosmic file into .gff file
 */
object TSVtoGFFResistance {

  /**
   * Read the TSV file and convert it to a list of ResistanceMutation objects
   *
   * @param filePath  Path to input file .tsv
   * @return          Returns list of loaded entries from .tsv in ResistanceMutation objects             
   */
  def readTSVResistanceMutations(filePath: String): Seq[ResistanceMutation] = {
    val gzipStream = new GZIPInputStream(new java.io.FileInputStream(filePath))
    val reader = new BufferedReader(new InputStreamReader(gzipStream))

    reader.lines().skip(1).toArray.map { line =>
      val cols = line.toString.split("\t")
      ResistanceMutation(
        cosmicGeneId = cols(3), 
        transcriptAccession = cols(4), 
        censusGene = cols(5),
        drugName = cols(6),
        drugResponse = cols(7), 
        chromosome = cols(19), 
        genomeStart = Utils.safeParseLong(cols(20)).getOrElse(0L),
        genomeStop = Utils.safeParseLong(cols(21)).getOrElse(0L), 
        strand = cols(22), 
        mutationZygosity = cols(23) 
      )
    }.toList
  }

  /**
   * Convert ResistanceMutation objects to GFF format and write to file
   *
   * @param filePath The path where the gff file will be created
   * @param features Loaded GeneCensusVariant objects from .tsv file cosmic
   */
  def writeGFF(filePath: String, mutations: Seq[ResistanceMutation]): Unit = {
    val file = new File(filePath)
    if (!file.getParentFile.exists()) {
      file.getParentFile.mkdirs() // Create the directory if it doesn't exist
    }
    val writer = new PrintWriter(filePath)

    
    Utils.writeGFFHeader(writer)

    // Write each mutation in GFF format
    mutations.foreach { mutation =>
      // Combine all relevant attributes in the GFF format
      val attributes = s"ID=${mutation.cosmicGeneId};" +
        s"TRANSCRIPT_ACCESSION=${mutation.transcriptAccession};" +
        s"CENSUS_GENE=${mutation.censusGene};" +
        s"DRUG_NAME=${mutation.drugName};" +
        s"DRUG_RESPONSE=${mutation.drugResponse};" +
        s"MUTATION_ZYGOSITY=${mutation.mutationZygosity};"

      // Write the feature line in GFF format
      writer.println(s"${mutation.chromosome}\tCosmic\tresistance_mutation\t${mutation.genomeStart}\t${mutation.genomeStop}\t.\t${mutation.strand}\t.\t$attributes")
    }

    // Close the writer
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
    val mutations = readTSVResistanceMutations(inputTSV)
    writeGFF(outputGFF, mutations)
  }
}

