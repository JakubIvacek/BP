package pdb

object ExtractPDB {
  private val idRegex = """ID\s+([A-Z0-9]+)_""".r
  private val pdbRegex = """DR\s+PDB;\s+(\S+);\s+(\S+);\s+([\d.]+)?\s*A?;?\s*(.*)""".r
  private val chainRangeRegex = """([A-Za-z])=([\d]+)-([\d]+)""".r

  def extractPdbMappings(inputFilePath: String, outputFilePath: String): Unit = {
    val reader = scala.io.Source.fromInputStream(new java.util.zip.GZIPInputStream(new java.io.FileInputStream(inputFilePath)))
    val writer = new java.io.PrintWriter(outputFilePath)
    var currentUniprotId: Option[String] = None

    try {
      for (line <- reader.getLines()) {
        line match {
          case idRegex(uniprotId) =>
            currentUniprotId = Some(uniprotId)

          case pdbRegex(pdbId, method, resStr, ranges) if currentUniprotId.nonEmpty =>
            val resolution = Option(resStr).flatMap(s => scala.util.Try(s.toDouble).toOption)
            val entries = ranges.split(",").flatMap(_.trim match {
              case chainRangeRegex(chain, start, end) =>
                Some((currentUniprotId.get, pdbId, method, resolution.getOrElse(-1.0), chain.toUpperCase, start.toInt, end.toInt))
              case _ => None
            })
            entries.foreach { case (uid, pdbId, method, resolution, chain, start, end) =>
              writer.println(s"$uid\t$pdbId\t$method\t$resolution\t$chain\t$start\t$end")
            }

          case _ => // skip
        }
      }
      println(s"PDB mappings successfully written to $outputFilePath")
    } catch {
      case e: Exception =>
        println(s"Error extracting PDB mappings: ${e.getMessage}")
    } finally {
      writer.close()
      reader.close()
    }
  }
}
