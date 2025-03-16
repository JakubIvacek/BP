package anotation
import data.UniProtEntry
import pdb.UniProtSequenceLoader

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
  def getPdbID(hgvs: String, hgvsPos: String, isoformsFastaPath: String): String = {
    val positions = extractPositions(hgvsPos)
    val entries = files.UniProtReader.getList()
    val matchedEntryOpt: Option[UniProtEntry] = positions match {
      case (startPos, None) =>
        findMatchingEntry(startPos, startPos, entries)
      case (startPos, Some(endPos)) =>
        findMatchingEntry(startPos, endPos, entries)
      case _ =>
        None
    }
    "."
    // val seqMap: Map[String, String] = UniProtSequenceLoader.loadSequencesFromFasta("uniprot_isoforms.fasta") // tu asi tu cestu by som daval ??
    //matchedEntryOpt match {
    //  case Some(entry) =>
    //    val isValid = validateHGVSrefAA(hgvs, entry.pdbId, seqMap)
    //    if (isValid) entry.pdbId else "."
    //  case None => "."
    //}
  }



















  
  def findMatchingEntry(start: Int, end: Int, entries: List[UniProtEntry]): Option[UniProtEntry]= {
    val matchingEntries = entries.filter(entry => start >= entry.start && end <= entry.end)
    matchingEntries match {
      case Nil => None
      case list =>
        val bestEntry = list.minBy(entry => entry.resolution.getOrElse(Double.MaxValue))
        Some(bestEntry)
    }
  }

  def validateHGVSrefAA(hgvs: String, uniprotId: String, sequenceMap: Map[String, String]): Boolean = {
    hgvs match {
      // Handle single position format (e.g., Gly54)
      case singlePosPattern(refAA3, posStr) =>
        val pos = posStr.toInt
        val refAA1 = threeLetterToOneLetter(refAA3)  // Convert three-letter to one-letter code
        val sequence = sequenceMap.getOrElse(uniprotId, "")
        if (sequence.isEmpty || pos > sequence.length) return false
        sequence.charAt(pos - 1).toString == refAA1

      // Handle range format (e.g., Gly35_Glu45)
      case rangePattern(refAA3, startPosStr, refAA4, endPosStr) =>
        val startPos = startPosStr.toInt
        val endPos = endPosStr.toInt
        val refAA1 = threeLetterToOneLetter(refAA3)
        val refAA2 = threeLetterToOneLetter(refAA4)
        val sequence = sequenceMap.getOrElse(uniprotId, "")
        if (sequence.isEmpty || endPos > sequence.length || startPos > endPos) return false
        sequence.charAt(startPos - 1).toString == refAA1 && sequence.charAt(endPos - 1).toString == refAA2

      // Handle extension format (e.g., Met1ext or Ter56extTer)
      case extPattern(refAA3, posStr) =>
        val pos = posStr.toInt
        val refAA1 = threeLetterToOneLetter(refAA3)
        val sequence = sequenceMap.getOrElse(uniprotId, "")
        if (sequence.isEmpty || pos > sequence.length) return false
        sequence.charAt(pos - 1).toString == refAA1 || sequence.charAt(pos - 1).toString == "X"  // "X" for termination or extension

      case _ => false
    }
  }


  def threeLetterToOneLetter(code: String): String = code match {
    case "Ala" => "A"
    case "Arg" => "R"
    case "Asn" => "N"
    case "Asp" => "D"
    case "Cys" => "C"
    case "Glu" => "E"
    case "Gln" => "Q"
    case "Gly" => "G"
    case "His" => "H"
    case "Ile" => "I"
    case "Leu" => "L"
    case "Lys" => "K"
    case "Met" => "M"
    case "Phe" => "F"
    case "Pro" => "P"
    case "Ser" => "S"
    case "Thr" => "T"
    case "Trp" => "W"
    case "Tyr" => "Y"
    case "Val" => "V"
    case _ => "X"
  }

}
