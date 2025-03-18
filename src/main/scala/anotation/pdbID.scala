package anotation
import data.UniProtEntry
import pdb.{UniProtSequenceLoader, PdbFastaReader}

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
  def getPdbID(hgvs: String, hgvsPos: String): String = {
    PdbFastaReader.load()
    val positions = extractPositions(hgvsPos)
    val entries = files.UniProtReader.getList()

    // Find PDB entry matching the position
    val matchedEntryOpt = positions match {
      case (startPos, None) =>
        findMatchingEntry(startPos, startPos, entries)
      case (startPos, Some(endPos)) =>
        findMatchingEntry(startPos, endPos, entries)
      case _ => None
    }
    //println(s"$hgvs - matchedPos ${matchedEntryOpt} ")
    // Retrieve UniProt sequence from FASTA
    val sequenceOpt = PdbFastaReader.getSequence(matchedEntryOpt.map(_.uniProtId).getOrElse(""))
    if (sequenceOpt.isEmpty) return "." // No sequence available for this UniProt ID
    val sequence = sequenceOpt.get
    //println(s"$hgvs - matchedPos ${matchedEntryOpt} ${sequence.take(10)}")
    // Validate the reference amino acid
    if (!validateHGVSrefAA(hgvs, sequence, hgvsPos)) return "."
    // Return the matched PDB ID if all checks pass
    matchedEntryOpt.map(_.pdbId).getOrElse(".")
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

  def validateHGVSrefAA(hgvs: String, sequence: String, hgvsPos: String): Boolean = {
    val seqLen = sequence.length // Store sequence length for better performance

    hgvsPos match {
      //  Single Position Check (e.g., p.Arg100His)
      case pdbID.singlePosPattern(refAA3, posStr) =>
        val pos = posStr.toInt
        val refAA1 = pdbID.threeLetterToOneLetter(refAA3)

        if (pos > seqLen) {
          println(s"$hgvsPos ERROR: Position $pos exceeds sequence length ($seqLen).")
          return false
        }

        println(s"$hgvsPos - Checking: ${sequence.charAt(pos - 1).toString} == $refAA1")
        sequence.charAt(pos - 1).toString == refAA1

      // Range Check (e.g., p.Gly35_Glu45)
      case pdbID.rangePattern(refAA3, startPosStr, refAA4, endPosStr) =>
        val startPos = startPosStr.toInt
        val endPos = endPosStr.toInt
        val refAA1 = pdbID.threeLetterToOneLetter(refAA3)
        val refAA2 = pdbID.threeLetterToOneLetter(refAA4)

        if (startPos > seqLen || endPos > seqLen) {
          println(s"$hgvsPos ERROR: Start ($startPos) or End ($endPos) exceeds sequence length ($seqLen).")
          return false
        }

        println(s"Checking: ${sequence.charAt(startPos - 1).toString} == $refAA1")
        println(s"Checking: ${sequence.charAt(endPos - 1).toString} == $refAA2")

        sequence.charAt(startPos - 1).toString == refAA1 &&
          sequence.charAt(endPos - 1).toString == refAA2

      // Extension Check (e.g., p.Met1ext or p.Ter56extTer)
      case pdbID.extPattern(refAA3, posStr) =>
        val pos = posStr.toInt
        val refAA1 = pdbID.threeLetterToOneLetter(refAA3)

        if (pos > seqLen) {
          println(s"$hgvsPos INFO: Position $pos extends beyond sequence length ($seqLen), considered valid extension.")
          return true
        }

        // Check if reference AA matches at canonical position
        sequence.charAt(pos - 1).toString == refAA1 || sequence.charAt(pos - 1).toString == "X"

      case _ =>
        println(s"${hgvsPos} ERROR: Invalid HGVS format.")
        false
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
