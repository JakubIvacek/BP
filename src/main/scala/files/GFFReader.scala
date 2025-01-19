package files

import data.{GffEntry, IntervalTree}
import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.util.Try

/**
 * The `GFFReader` object is responsible for reading GFF3  files and loading
 * them into a collection of `IntervalTree` objects
 */
object GFFReader {
  private var intervalTrees: Map[String, IntervalTree] = Map.empty
  var isLoaded: Boolean = false

  /**
   * Preload a GFF3 file into memory, parsing the file into `IntervalTree` structures for each contig.
   *
   * @param filename The path to the GFF3 file to load.
   */
  def preloadGff3File(filename: String): Unit = {
    val source = Source.fromFile(filename)
    val contigTrees = scala.collection.mutable.Map[String, IntervalTree]()

    source.getLines()
      .filterNot(_.startsWith("#"))
      .flatMap { line =>
        val fields = line.split("\t")
        if (fields.length < 9) None
        else {
          val contig = fields(0)
          val start = Try(fields(3).toInt).getOrElse(0)
          val end = Try(fields(4).toInt).getOrElse(0)
          val name = fields(2)
          val strandPlus = if fields(6) == "+" then true else false
          val attributes = fields(8).split(";").map { attr =>
            val keyValue = attr.split("=")
            if (keyValue.length == 2) keyValue(0) -> keyValue(1) else keyValue(0) -> ""
          }.toMap

          Some(contig, GffEntry(contig, start, end, strandPlus, name, attributes))
        }
      }
      .foreach { case (contig, entry) =>
        val tree = contigTrees.getOrElseUpdate(contig, new IntervalTree())
        tree.insert(entry)
      }

    intervalTrees = contigTrees.toMap
    isLoaded = true
    source.close()
  }

  /**
   * Retrieve the `IntervalTree` for a specific contig.
   *
   * @param contig The name of the contig
   * @return The `IntervalTree` containing the features for the specified contig.
   */
  def getIntervalTree(contig: String): IntervalTree = {
    if (!isLoaded) throw new IllegalStateException("GFF3 file not preloaded. Call preloadGff3File first.")
    intervalTrees.getOrElse(contig, new IntervalTree())
  }
}





