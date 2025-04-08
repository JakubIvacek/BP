package cosmic

import data.GffEntry
import files.FastaReaderSW

import java.io.*
import java.util.zip.GZIPInputStream
import scala.collection.mutable.ListBuffer
import scala.io.Source

object GFFtoFastaCosmic {

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
  // Method to generate FASTA formatted sequence and write to file
  def writeToFasta(gffEntries: List[GffEntry], fastaFilePath: String): Unit = {
    // Create a BufferedWriter to write to the FASTA file
    val writer = new BufferedWriter(new FileWriter(fastaFilePath))

    // Iterate over each GFF entry and fetch the sequence
    for (entry <- gffEntries) {
      // Retrieve the sequence for the current GFF entry using the FastaReaderSW
      var sequence = FastaReaderSW.getSequence("reference/t2t/chm13v2.0.fa", entry.contig, entry.start, entry.end, entry.strandPlus)
      sequence = sequence.toUpperCase
      // Format the header for the FASTA entry
      val header = s">${entry.attributes.getOrElse("Gene_ID", "N/A")} ${entry.attributes.getOrElse("transcript_id", "N/A")} ${entry.contig}:${entry.start}-${entry.end}(${if (entry.strandPlus) "+" else "-"})"

      // Write the header to the file
      writer.write(header)
      writer.newLine()

      // Write the sequence to the file, split into lines of 60 characters (optional)
      for (i <- sequence.grouped(60)) {
        writer.write(i.mkString)
        writer.newLine()
      }
    }

    // Close the writer after writing all sequences
    writer.close()
  }

  def parseAttributes(attributeString: String): Map[String, String] = {
    attributeString.split(";").map { attribute =>
      val keyValue = attribute.split("=")
      if (keyValue.length == 2) keyValue(0) -> keyValue(1) else keyValue(0) -> ""
    }.toMap
  }

  def main(args: Array[String]): Unit = {
    val filePath = "cosmic_genes.gff"  // Specify the path to your GFF file
    val gffEntries = readGFF(filePath)
    writeToFasta(gffEntries, "new.fasta")
  }
}
