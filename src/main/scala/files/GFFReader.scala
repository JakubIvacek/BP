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

    try {
      for (line <- source.getLines()) {
        if (!line.startsWith("#")) {
          val fields = line.split("\t", -1) // -1 keeps empty values
          if (fields.length >= 9) {
            val contig = fields(0)
            val start = Try(fields(3).toInt).getOrElse(0)
            val end = Try(fields(4).toInt).getOrElse(0)
            val name = fields(2)
            val strandPlus = fields(6) == "+"
            val attributes = fields(8).split(";").map { attr =>
              val keyValue = attr.split("=")
              keyValue.head -> keyValue.lift(1).getOrElse("")
            }.toMap

            // Insert directly into tree without buffering in a list
            val tree = contigTrees.getOrElseUpdate(contig, new IntervalTree())
            tree.insert(GffEntry(contig, start, end, strandPlus, name, attributes))
          }
        }
      }
      intervalTrees = contigTrees.toMap
      isLoaded = true
    } finally {
      source.close() // Ensure resource is closed
    }
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





