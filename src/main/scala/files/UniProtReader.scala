package files

import commandLine.PathSaver

import scala.io.Source
import scala.util.matching.Regex
import data.UniProtEntry
import module.UniProtDownload

/**
 * An object for reading UniProt-PDB mapping files and converting them into UniProtEntry instances.
 */
object UniProtReader {

  // Regular expression for parsing mapping lines
  private val pdbRegex: Regex = """DR\s+PDB;\s+(\S+);\s+(\S+);\s+([\d.]+)?\s*A?;?\s*(.*)""".r
  private val chainRangeRegex: Regex = """([A-Za-z])=([\d]+)-([\d]+)""".r
  private val fileName = "uniprot_pdb_mappings.txt"
  var list: Option[List[UniProtEntry]] = None
  /**
   * Loads UniProt-PDB mappings from a given file path.
   *
   * @param filePath Path to the UniProt-PDB mapping file.
   * @return A list of UniProtEntry instances representing the mappings.
   */
  def loadMappings(filePath: String): List[UniProtEntry] = {
    val source = Source.fromFile(filePath)
    try {
      source.getLines().flatMap { line =>
        val parts = line.split("\t")
        if (parts.length >= 7) {
          val uniprotId = parts(0)
          val pdbId = parts(1)
          val method = parts(2)
          val resolution = if (parts(3) == "-1.0") None else Some(parts(3).toDouble)
          val chain = parts(4)
          val start = parts(5).toInt
          val end = parts(6).toInt
          Some(UniProtEntry(pdbId, method, resolution, chain, start, end, uniprotId))
        } else None
      }.toList
    } finally {
      source.close()
    }
  }


  /**
   * Loads the UniProt-PDB mappings, downloading them if necessary.
   * Caches the result to avoid repeated loading.
   *
   * @return A list of UniProtEntry instances representing the mappings.
   */
  def getList(): List[UniProtEntry] = {
    list.getOrElse {
      val dir = UniProtDownload.getPath() match {
        case p if p.nonEmpty => p                      // Use existing path if available
        case _ =>
          val savePath = PathSaver.getPath.getOrElse("") // Get path from PathSaver or use empty string
          UniProtDownload.download(savePath)
          UniProtDownload.getPath()
      }
      val path = s"$dir/$fileName"
      val loadedList = loadMappings(path)
      list = Some(loadedList) // Cache the loaded list
      loadedList
    }
  }

  def main(args: Array[String]): Unit = {
    val entries: List[UniProtEntry] = getList()
    entries.foreach { entry =>
      println(s"Range: ${entry.start}-${entry.end}, ID: ${entry.pdbId}")
    }
  }
}

