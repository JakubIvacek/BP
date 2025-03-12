package pdb

import java.io.{BufferedWriter, FileInputStream, FileWriter}
import java.util.zip.GZIPInputStream

object ExtractUniProtIsoforms {

  def extractSequences(inputFilePath: String, outputFilePath: String): Unit = {
    val source = scala.io.Source.fromInputStream(new GZIPInputStream(new FileInputStream(inputFilePath)))
    val writer = new BufferedWriter(new FileWriter(outputFilePath))

    var currentAcc: Option[String] = None
    val seqBuffer = new StringBuilder
    var inSequence = false
    var isoforms = List.empty[String]

    for (line <- source.getLines()) {
      if (line.startsWith("ID")) {
        if (currentAcc.nonEmpty && seqBuffer.nonEmpty) {
          writer.write(s"${currentAcc.get}\t${seqBuffer.toString()}\n")
        }
        seqBuffer.clear()
        isoforms = Nil
        inSequence = false
      }

      if (line.startsWith("AC")) {
        val acc = line.stripPrefix("AC").split(";").head.trim
        currentAcc = Some(acc)
      }

      if (line.startsWith("FT   ALTERNATIVE PRODUCTS")) {
        inSequence = false
        isoforms = Nil
      }

      // Canonical sequence
      if (line.startsWith("SQ")) {
        inSequence = true
      }

      if (inSequence && line.startsWith("     ")) {
        seqBuffer.append(line.trim.replace(" ", ""))
      }
    }

    // Write final entry
    if (currentAcc.nonEmpty && seqBuffer.nonEmpty) {
      writer.write(s"${currentAcc.get}\t${seqBuffer.toString()}\n")
    }

    writer.close()
    source.close()
    println(s"Extracted canonical sequences to: $outputFilePath")
  }
}

