package anotation
import data.UniProtEntry

import scala.util.matching.Regex

object pdbID {
  // Define regex patterns for single position and range
  val singlePosPattern: Regex = """([A-Za-z]+)(\d+)""".r
  val rangePattern: Regex = """([A-Za-z]+)(\d+)_([A-Za-z]+)(\d+)""".r
  val extPattern: Regex = """([A-Za-z]+)(\d+)ext(?:Ter)?""".r
  /**
   * Extracts positions from HGVS notation.
   *
   * @param hgvs The HGVS notation string position part.
   * @return A tuple containing the start and end positions.
   */
  def extractPositions(hgvsPos: String): (Int, Option[Int]) = {
    hgvsPos match {
      // Single position format: Gly54
      case singlePosPattern(_, pos) => (pos.toInt, None)

      // Range format: Gly35_Glu45
      case rangePattern(_, startPos, _, endPos) =>
        (startPos.toInt, Some(endPos.toInt))
        
      // Extension (ext) case: Met1ext or Ter56extTer
      case extPattern(_, pos) =>
        (pos.toInt, None)

      // Handle other cases (e.g., invalid formats)
      case _ => (0, None) // Default case for invalid formats
    }
  }

  /**
   * Extracts positions from HGVS notation.
   *
   * @param hgvs The HGVS notation string position part.
   * @return String containing matched pdbID
   */
  def getPdbID(hgvsPos: String): String = {
    val positions = extractPositions(hgvsPos)
    val entries = files.UniProtReader.getList()
    positions match {
      case (startPos, None) =>
        findMatchingEntry(startPos, startPos, entries)
      case (startPos, Some(endPos)) =>
        findMatchingEntry(startPos, endPos, entries)
      case _ =>
        // Handle invalid or unsupported format
        //println("Invalid position format")
        "."
    }
  }

  def findMatchingEntry(start: Int, end: Int, entries: List[UniProtEntry]): String= {
    val matchingEntries = entries.filter(entry => start >= entry.start && end <= entry.end)
    matchingEntries match {
      case Nil => "." // No matching entries found
      case list =>
        val bestEntry = list.minBy(entry => entry.resolution.getOrElse(Double.MaxValue))
        bestEntry.pdbId
    }
  }
}
