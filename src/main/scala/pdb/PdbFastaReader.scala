package pdb

import downloader.UniProtDownload
import logfiles.PathSaver
import java.util.zip.GZIPInputStream
import java.io.FileInputStream
import scala.io.Source
import scala.collection.mutable

object PdbFastaReader {
  private var fastaMap: Map[String, String] = Map()
  private var isLoaded: Boolean = false
  var filename = "uniprot_sprot.fasta.gz"

  /**
   * Loads UniProt FASTA file and keeps only human (_HUMAN) sequences.
   * Extracts only the first part of the UniProt ID.
   *
   * @param fastaFile Path to UniProt FASTA file
   */
  def loadFasta(fastaFile: String): Unit = {

    val gzipStream = new GZIPInputStream(new FileInputStream(fastaFile))
    val fastaSource = Source.fromInputStream(gzipStream)
    val tempMap = mutable.Map[String, String]()
    var currentID: Option[String] = None
    val sequence = new StringBuilder

    for (line <- fastaSource.getLines()) {
      if (line.startsWith(">")) {
        // Save previous entry before moving to the next one
        currentID.foreach(id => tempMap(id) = sequence.toString())
        sequence.clear()

        // Split header by "|"
        val parts = line.split('|')
        if (parts.length > 2) {
          val fullID = parts(2).split(" ")(0) // Take first part before space (e.g., "1433F_HUMAN")

          if (fullID.endsWith("_HUMAN")) { // Keep only human proteins
            val uniprotID = fullID.split("_")(0) // Take "1433F" from "1433F_HUMAN"
            currentID = Some(uniprotID)
          } else {
            currentID = None // Ignore non-human entries
          }
        } else {
          currentID = None
        }
      } else if (currentID.isDefined) {
        sequence.append(line.trim)
      }
    }
    // Store last sequence
    currentID.foreach(id => tempMap(id) = sequence.toString())

    fastaSource.close()
    fastaMap = tempMap.toMap
    isLoaded = true
    //println(s" Loaded ${fastaMap.size} Human UniProt sequences into memory.")
  }

  /**
   * Retrieves the sequence for a given UniProt ID.
   *
   * @param uniprotID UniProt identifier (e.g., "TKT1")
   * @return Optional protein sequence string
   */
  def getSequence(uniprotID: String): Option[String] = {
    if (!isLoaded) {
      println("Warning: FASTA not loaded! Call loadFasta() first.")
      return None
    }

    fastaMap.get(uniprotID) // Match using extracted ID
  }

  def load(): Unit = {
    if (isLoaded) {
      //println("ℹ️ UniProt FASTA already loaded. Skipping reload.")
      return
    }
    val dir = UniProtDownload.getPath() match {
      case p if p.nonEmpty => p // Use existing path if available
      case _ =>
        val savePath = PathSaver.getPath.getOrElse("") // Get path from PathSaver or use empty string
        UniProtDownload.download(savePath)
        UniProtDownload.getPath()
    }
    val path = s"$dir/$filename"
    loadFasta(path)
  }
}


