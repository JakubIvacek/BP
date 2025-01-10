import scala.io.Source
import scala.util.Try
import scala.collection.mutable.ListBuffer

object GFFReader {
  var closestUpstream: Option[GffEntry] = None
  var closestDownstream: Option[GffEntry] = None

  def parseAttributes(attributeString: String): Map[String, String] = {
    attributeString.split(";").map { attr =>
      val keyValue = attr.split("=")
      if (keyValue.length == 2) keyValue(0) -> keyValue(1) else keyValue(0) -> ""
    }.toMap
  }
  // Method to parse and match a single line of GFF3 into a GffEntry
  def parseMatchLine(line: String, pos: Int, contig: String): Option[GffEntry] = {
    val fields = line.split("\t")

    if (fields.length < 9) {
      None
    } else {
      val contigFile = fields(0)  // The contig field from the GFF file
      val startFile = Try(fields(3).toInt).getOrElse(0)  // The start field from the GFF file
      val endFile = Try(fields(4).toInt).getOrElse(0)  // The end field from the GFF file

      // Perform the checks to see if the contig matches and the position is within the range
      if (contig == contigFile && pos >= startFile && pos <= endFile) {
        // Return the GffEntry with contig, start, and end values if both checks pass
        val attributes = parseAttributes(fields(8))
        Some(GffEntry(contigFile, startFile, endFile, attributes))
      } else if (contig == contigFile) {
        // Handling the cases for upstream and downstream
        if (pos > endFile) {
          // If the position is upstream of the current entry
          closestUpstream match {
            case None => closestUpstream = Some(GffEntry(contigFile, startFile, endFile, parseAttributes(fields(8))))
            case Some(existing) if existing.end < endFile => closestUpstream = Some(GffEntry(contigFile, startFile, endFile, parseAttributes(fields(8))))
            case _ =>
          }
          None
        } else if (pos < startFile) {
          // If the position is downstream of the current entry
          closestDownstream match {
            case None => closestDownstream = Some(GffEntry(contigFile, startFile, endFile, parseAttributes(fields(8))))
            case Some(existing) if existing.start > startFile => closestDownstream = Some(GffEntry(contigFile, startFile, endFile, parseAttributes(fields(8))))
            case _ =>
          }
          None
        } else {
          None
        }
      } else {
        None
      }
    }
  }
  // Method to parse the entire GFF3 file and return a List of GffEntry objects
  // Returns only relevant lines matching contig, pos
  def parseMatchGff3File(filename: String, pos: Int, conting: String): List[GffEntry] = {
    val source = Source.fromFile(filename)

    // Use ListBuffer for efficient accumulation of entries
    val entries = ListBuffer[GffEntry]()

    // Process the file line by line
    source.getLines()
      .filterNot(_.startsWith("#")) // Skip comment lines
      .foreach { line =>
        parseMatchLine(line, pos, conting) match {
          case Some(entry) =>  entries += entry// Add valid entries to the list
          case None => // Handle invalid lines if necessary
        }
      }

    source.close()

    // Return the list of entries (converted to an immutable List)
    if (entries.nonEmpty){
      entries.toList
    }else{
      closestUpstream.orElse(closestDownstream).toList
    }
  }
}




