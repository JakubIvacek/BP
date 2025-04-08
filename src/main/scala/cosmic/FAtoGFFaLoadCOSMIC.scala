package cosmic

import scala.io.Source
import scala.collection.mutable.ListBuffer
import java.util.zip.GZIPInputStream
import java.io.{File, FileInputStream, PrintWriter}
import dataCosmic.FaEntryCosmic
import utils.LiftOverTool

object FAtoGFFaLoadCOSMIC {
  
  var loadedList: Option[List[FaEntryCosmic]] = None
  
  def loadFastaFromGzip(filePath: String): Unit = {
    if loadedList.nonEmpty then return // already loaded dont want to load again
    val fileStream = new FileInputStream(filePath)
    val gzipStream = new GZIPInputStream(fileStream)
    val lines = Source.fromInputStream(gzipStream).getLines().toList
    val geneDataList = ListBuffer[FaEntryCosmic]()

    var currentHeader = ""
    var currentSequence = new StringBuilder
    
    lines.foreach { line =>
      if (line.startsWith(">")) { // Header line
        if (currentHeader.nonEmpty) {
          val headerParts = parseHeader(currentHeader)
          geneDataList += FaEntryCosmic(headerParts._1, headerParts._2, headerParts._3, headerParts._4, headerParts._5, headerParts._6, currentSequence.toString)
        }
        
        currentHeader = line
        currentSequence = new StringBuilder
      } else { 
        currentSequence.append(line)
      }
    }
    if (currentHeader.nonEmpty) {
      val headerParts = parseHeader(currentHeader)
      geneDataList += FaEntryCosmic(headerParts._1, headerParts._2, headerParts._3, headerParts._4, headerParts._5, headerParts._6, currentSequence.toString)
    }

    loadedList = Some(geneDataList.toList)
  }

  // Helper method to parse the header line
  def parseHeader(header: String): (String, String, String, Int, Int, String) = {
    // Expected header format: >GENE_SYMBOL TRANSCRIPT_ACCESSION CHROMOSOME:GENOME_START-GENOME_STOP(STRAND)
    val parts = header.tail.split(" ")
    val geneSymbol = parts(0)
    val transcriptAccession = parts(1)
    val chromosomeInfo = parts(2)
    val genomeStartStop = chromosomeInfo.split(":")(1).split("\\(")(0)
    val genomeStart = genomeStartStop.split("-")(0).toInt
    val genomeStop = genomeStartStop.split("-")(1).toInt
    val strand = chromosomeInfo.split("\\(")(1).stripSuffix(")")

    (geneSymbol, transcriptAccession, chromosomeInfo.split(":")(0), genomeStart, genomeStop, strand)
  }

  // Write GFF header
  def writeGFFHeader(writer: PrintWriter): Unit = {
    writer.println("##gff-version 3")
  }

  // Convert GeneFeature objects to GFF format and write to file
  def writeGFF(filePath: String, features: Seq[FaEntryCosmic]): Unit = {
    println(s"Converting cosmic .fa to .gff - $filePath")
    val file = new File(filePath)
    if (!file.getParentFile.exists()) {
      file.getParentFile.mkdirs() // Create the directory if it doesn't exist
    }
    val writer = new PrintWriter(filePath)

    // Write header
    writeGFFHeader(writer)

    // Write each feature in GFF format
    features.foreach { feature =>
      val attributes = s"Gene_ID=${feature.geneSymbol};" +
        s"Transcript_ID=${feature.transcriptAccession};"

      // Write the feature line in GFF format
      writer.println(s"${feature.chromosome}\tCosmic\ttranscript\t${feature.genomeStart}\t${feature.genomeStop}\t.\t${feature.strand}\t.\t$attributes")
    }

    // Close the writer
    writer.close()
  }
}