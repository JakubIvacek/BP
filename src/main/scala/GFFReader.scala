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
      val contigFile = fields(0)
      val startFile = Try(fields(3).toInt).getOrElse(0)
      val endFile = Try(fields(4).toInt).getOrElse(0)

      if (contig == contigFile && pos >= startFile && pos <= endFile) {
        val attributes = parseAttributes(fields(8))
        Some(GffEntry(contigFile, startFile, endFile, attributes))
      } else if (contig == contigFile) {
        // upstream and downstream
        if (pos > endFile) {
          closestUpstream match {
            case None => closestUpstream = Some(GffEntry(contigFile, startFile, endFile, parseAttributes(fields(8))))
            case Some(existing) if existing.end < endFile => closestUpstream = Some(GffEntry(contigFile, startFile, endFile, parseAttributes(fields(8))))
            case _ =>
          }
          None
        } else if (pos < startFile) {
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
  def parseMatchGff3File(filename: String, pos: Int, conting: String): List[GffEntry] = {
    val source = Source.fromFile(filename)

    val entries = ListBuffer[GffEntry]()

    // Process the file line by line
    source.getLines()
      .filterNot(_.startsWith("#"))
      .foreach { line =>
        parseMatchLine(line, pos, conting) match {
          case Some(entry) =>  entries += entry
          case None =>
        }
      }

    source.close()

    if (entries.nonEmpty){
      entries.take(30).toList
    }else{
      closestUpstream.orElse(closestDownstream).toList
    }
  }
}




