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
        line match {
          case pdbRegex(pdbId, method, resStr, ranges) if ranges.nonEmpty =>
            val resolution = Option(resStr).flatMap(s => scala.util.Try(s.toDouble).toOption)
            // Split multiple ranges and normalize chain cases
            ranges.split(",").flatMap(_.trim match {
              case chainRangeRegex(chain, start, end) =>
                Some(UniProtEntry(pdbId, method, resolution, chain.toUpperCase, start.toInt, end.toInt))
              case _ => None
            })
          case _ => None
        }
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

