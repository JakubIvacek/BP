package data

case class FaEntryCosmic(
                          geneSymbol: String,
                          transcriptAccession: String,
                          chromosome: String,
                          genomeStart: Int,
                          genomeStop: Int,
                          strand: String,
                          sequence: String
                        ){
  /**
   * Grab the subsequence covering [subStart, subEnd] in genome coords.
   * Returns None if the requested interval lies outside this entry.
   */
  def getSequencePart(subStart: Int, subEnd: Int): String = {
    // convert genome coords to 0‐based sequence indices
    val offsetStart = subStart - genomeStart
    val offsetEnd = subEnd - genomeStart + 1

    // check against string bounds
    if (offsetStart < 0 || offsetEnd > sequence.length)
      ""
    else {
      val raw = sequence.substring(offsetStart, offsetEnd)
      if (strand == "-") reverseComplement(raw) else raw
    }
  }

  private def reverseComplement(s: String): String = {
    s.map {
        case 'A' => 'T'; case 'T' => 'A'
        case 'C' => 'G'; case 'G' => 'C'
        case  x  => x
      }
      .reverse
  }
}

