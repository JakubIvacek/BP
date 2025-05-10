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
    val (sRaw, eRaw) =
      if (subStart <= subEnd) (subStart, subEnd)
      else (subEnd, subStart)

    val offsetStart = sRaw - genomeStart
    val offsetEnd   = eRaw - genomeStart + 1

    if (offsetStart < 0 || offsetEnd > sequence.length || offsetStart >= offsetEnd) {
      ""
    } else {
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

