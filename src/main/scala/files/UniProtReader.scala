package files

import scala.io.Source
import scala.util.matching.Regex
import data.UniProtEntry
import logfiles.PathSaver
import module.UniProtDownload

/**
 * Reads UniProt-PDB mapping files and converts them into UniProtEntry instances.
 */
object UniProtReader {

  private val fileName = "uniprot_pdb_mappings.txt"
  var list: Option[List[UniProtEntry]] = None

  /**
   * Loads UniProt-PDB mappings from a given file path.
   *
   * @param filePath Path to the UniProt-PDB mapping file.
   * @return A list of UniProtEntry instances representing the mappings.
   */
  def loadMappings(filePath: String): List[UniProtEntry] = {
   // println(s"Attempting to load UniProt-PDB mappings from: $filePath")

    if (!new java.io.File(filePath).exists()) {
      println(s"ERROR: File not found - $filePath")
      return List.empty
    }

    val source = Source.fromFile(filePath)
    try {
      val entries = source.getLines().flatMap { line =>
        // Debug: Print each line being processed
        // println(s"🔍 Processing Line: $line")

        val parts = line.split("\t") // UniProt ID + PDB mapping
        if (parts.length < 2) None
        else {
          val uniprotId = parts(0).trim // Extract UniProt ID (e.g., Q6GZX4)
          val pdbParts = parts(1).split(";").map(_.trim) // Extract PDB-related details

          if (pdbParts.length >= 5) {
            val pdbId = pdbParts(1)
            val method = pdbParts(2)
            val resolution = if (pdbParts(3).contains("A")) {
              Some(pdbParts(3).split(" ")(0).toDouble) // Extract numeric resolution
            } else None

            // Extract chains and positions
            val chainRangeRegex = """([A-Za-z])=([\d]+)-([\d]+)""".r
            val chains = chainRangeRegex.findAllMatchIn(pdbParts(4)).map { m =>
              val chain = m.group(1)
              val start = m.group(2).toInt
              val end = m.group(3).toInt
              UniProtEntry(pdbId, method, resolution, chain, start, end, uniprotId) // Correctly store UniProt ID
            }.toList

            chains
          } else None
        }
      }.toList

      //println(s"Successfully loaded ${entries.length} UniProt-PDB mappings")
      entries
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
      //println(s"Loaded ${loadedList.length} PDB entries")
      list = Some(loadedList) // Cache the loaded list
      loadedList
    }
  }

  def main(args: Array[String]): Unit = {
    val entries: List[UniProtEntry] = loadMappings(s"uniprot/$fileName")
    entries.foreach { entry =>
      //println(s" UniProt: ${entry.uniProtId}, PDB: ${entry.pdbId}, Range: ${entry.start}-${entry.end}")
    }
  }
}