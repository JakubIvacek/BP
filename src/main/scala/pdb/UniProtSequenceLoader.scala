package pdb

object UniProtSequenceLoader {
  def loadSequencesFromFasta(fastaPath: String): Map[String, String] = {
    val lines = scala.io.Source.fromFile(fastaPath).getLines()
    val result = scala.collection.mutable.Map[String, StringBuilder]()
    var currentId = ""

    for (line <- lines) {
      if (line.startsWith(">")) {
        currentId = line.stripPrefix(">").trim
        result(currentId) = new StringBuilder()
      } else {
        result(currentId).append(line.trim)
      }
    }
    result.map { case (k, v) => k -> v.toString() }.toMap
  }
}

