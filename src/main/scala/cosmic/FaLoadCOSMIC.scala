package cosmic

import scala.io.Source
import scala.collection.mutable.ListBuffer
import java.util.zip.GZIPInputStream
import java.io.FileInputStream
import data.FaEntryCosmic

object FaLoadCOSMIC {
  
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

  def main(args: Array[String]): Unit = {
    FaLoadCOSMIC.loadFastaFromGzip("data/cosmic/v101/hg38/Cosmic_Genes_v101_GRCh38.fasta.gz")
    println(FaLoadCOSMIC.loadedList.getOrElse(List()).length)
    TSVtoGFFGeneCensus.convertTSVToGFF("data/cosmic/v101/hg38/Cosmic_CancerGeneCensus_v101_GRCh38.tsv.gz", "con_census.gff")
    TSVtoGFFNonCoding.convertTSVToGFF("data/cosmic/v101/hg38/Cosmic_NonCodingVariants_v101_GRCh38.tsv.gz", "con_noncod.gff")
    TSVtoGFFResistance.convertTSVToGFF("data/cosmic/v101/hg38/Cosmic_ResistanceMutations_v101_GRCh38.tsv.gz", "con_resmut.gff")
  }
  
}