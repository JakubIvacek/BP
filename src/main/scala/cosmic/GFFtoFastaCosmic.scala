package cosmic

import data.GffEntry
import files.FastaReaderSW
import logfiles.RefChainDirManager

import java.io.*
import java.util.zip.GZIPInputStream
import scala.collection.mutable.ListBuffer
import scala.io.Source

/**
 * Object for creating back from .gff  cosmic .fasta file for annotation
 */
object GFFtoFastaCosmic {

  /**
   * Read cosmic fasta .gff file and return GffEntries list
   *
   * @param filePath    The path where the gff file is located
   * @return            Returns list of loaded GffEntries from file
   */
  def readGFF(filePath: String): List[GffEntry] = {
    val entries = ListBuffer[GffEntry]()

    // Check if the file is gzipped
    val isGzipped = filePath.endsWith(".gz")
    val reader = if (isGzipped) {
      val fileStream = new FileInputStream(filePath)
      val gzipStream = new GZIPInputStream(fileStream)
      new BufferedReader(new InputStreamReader(gzipStream))
    } else {
      val fileStream = Source.fromFile(filePath).bufferedReader()
      fileStream
    }

    // Iterate through each line
    for (line <- reader.lines().toArray().map(_.toString)) {
      if (!line.startsWith("#")) {
        val fields = line.split("\t")
        if (fields.length >= 9) {
          val contig = fields(0)
          val start = fields(3).toInt
          val end = fields(4).toInt
          val strandPlus = fields(6) == "+"
          val name = fields(2)

          val attributeString = fields(8)
          val attributes = parseAttributes(attributeString)

          val entry = GffEntry(contig, start, end, strandPlus, name, attributes)
          entries += entry
        }
      }
    }
    entries.toList
  }

  /**
   * Method to generate FASTA formatted sequence and write to fileRead fasta .gff file and return GffEntries list
   *
   * @param fastaFilePath The path where the .fasta file will be created
   * @param gffEntries    List of GffEntries loaded from .gff file
   */
  def writeToFasta(gffEntries: List[GffEntry], fastaFilePath: String): Unit = {
    val writer = new BufferedWriter(new FileWriter(fastaFilePath))
    val dirRef = RefChainDirManager.getReferenceFileDir.getOrElse("")
    for (entry <- gffEntries) {
      var sequence = FastaReaderSW.getSequence(s"$dirRef/chm13.fa", entry.contig, entry.start, entry.end, entry.strandPlus)
      sequence = sequence.toUpperCase

      val header = s">${entry.attributes.getOrElse("Gene_ID", "N/A")} ${entry.attributes.getOrElse("Transcript_ID", "N/A")} ${entry.contig}:${entry.start}-${entry.end}(${if (entry.strandPlus) "+" else "-"})"

      writer.write(header)
      writer.newLine()

      for (i <- sequence.grouped(60)) {
        writer.write(i.mkString)
        writer.newLine()
      }
    }

    writer.close()
  }

  /**
   * Method converts cosmic gff file to fasta file
   *
   * @param outputPath path where the .fasta file will be created
   * @param filePath   path where the .gff file is located
   */
  def convertToFasta(filePath: String, outputPath: String): Unit = {
    val gffEntries = readGFF(filePath)
    writeToFasta(gffEntries, outputPath)
    println(s"Gff - $filePath converted to .fasta - $outputPath")
    //Gunzip.zipFile(outputPath)
  }

  /**
   * Helper method to parse attributes
   *
   * @param attributeString String with .fasta attributes to be parsed
   */
  def parseAttributes(attributeString: String): Map[String, String] = {
    attributeString.split(";").map { attribute =>
      val keyValue = attribute.split("=")
      if (keyValue.length == 2) keyValue(0) -> keyValue(1) else keyValue(0) -> ""
    }.toMap
  }
}
