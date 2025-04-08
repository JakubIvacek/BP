package cosmic

import cosmic.data.ResistanceMutation

import java.io.{BufferedReader, File, InputStreamReader, PrintWriter}
import java.util.zip.GZIPInputStream
import scala.io.Source
import scala.util.Try

object TSVtoGFFResistance {

  def safeParseLong(str: String): Option[Long] = {
    Try(str.toLong).toOption
  }

  // Read the TSV file and convert it to a list of ResistanceMutation objects
  def readTSVResistanceMutations(filePath: String): Seq[ResistanceMutation] = {
    val gzipStream = new GZIPInputStream(new java.io.FileInputStream(filePath))
    val reader = new BufferedReader(new InputStreamReader(gzipStream))

    reader.lines().skip(1).toArray.map { line =>
      val cols = line.toString.split("\t")
      ResistanceMutation(
        cosmicGeneId = cols(3), // COSMIC_GENE_ID
        transcriptAccession = cols(4), // TRANSCRIPT_ACCESSION
        censusGene = cols(5), // CENSUS_GENE
        drugName = cols(6), // DRUG_NAME
        drugResponse = cols(7), // DRUG_RESPONSE
        chromosome = cols(19), // CHROMOSOME
        genomeStart = safeParseLong(cols(20)).getOrElse(0L), // GENOME_START
        genomeStop = safeParseLong(cols(21)).getOrElse(0L), // GENOME_STOP
        strand = cols(22), // STRAND
        mutationZygosity = cols(23) // MUTATION_ZYGOSITY
      )
    }.toList
  }

  // Write GFF header
  def writeGFFHeader(writer: PrintWriter): Unit = {
    writer.println("##gff-version 3")
  }

  // Convert ResistanceMutation objects to GFF format and write to file
  def writeGFF(filePath: String, mutations: Seq[ResistanceMutation]): Unit = {
    val file = new File(filePath)
    if (!file.getParentFile.exists()) {
      file.getParentFile.mkdirs() // Create the directory if it doesn't exist
    }
    val writer = new PrintWriter(filePath)

    // Write header
    writeGFFHeader(writer)

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

  // Main method to convert TSV to GFF
  def convertTSVToGFF(inputTSV: String, outputGFF: String): Unit = {
    val mutations = readTSVResistanceMutations(inputTSV)
    writeGFF(outputGFF, mutations)
  }
}

