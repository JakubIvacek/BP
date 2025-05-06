package files

import data.FaEntryCosmic
import java.io.FileInputStream
import java.util.zip.GZIPInputStream
import scala.collection.mutable.ListBuffer
import scala.io.Source

/**
 * Object for loading .fasta cosmic file to loadedList[FaEntryCosmic]
 */
object FastaLoadCOSMIC {
  
  private var loadedList: Option[List[FaEntryCosmic]] = None
  var loadedGenome: String = ""

  /**
   * Returns the currently loaded list of FaEntryCosmic,
   * or an empty list if nothing has been loaded yet.
   */
  def getLoadedList: List[FaEntryCosmic] =
    loadedList.getOrElse(List.empty[FaEntryCosmic])
    
  /**
   * Load .fasta cosmic file to loadedList
   *
   * @param filePath The path to .fasta file which should be loaded
   */
  def loadFastaFromGzip(filePath: String, refGenome: String): Unit = {
    if loadedList.nonEmpty && refGenome == loadedGenome then return // already loaded dont want to load again
    val inputStream = {
      val fileStream = new FileInputStream(filePath)
      if (filePath.endsWith(".gz")) new GZIPInputStream(fileStream)
      else fileStream
    }
    val lines = Source.fromInputStream(inputStream).getLines().toList
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
    loadedGenome = refGenome
    loadedList = Some(geneDataList.toList)
  }

  /**
   * Helper method to parse the header line for .fasta entry
   *
   * @param header String with header data for entry
   * @return       Parsed header entry
   */
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
}