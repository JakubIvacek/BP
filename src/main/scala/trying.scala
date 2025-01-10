import scala.io.Source
import scala.collection.mutable

// Case class to represent a GFF3 record
case class GFF3Record(
                       seqId: String,
                       source: String,
                       featureType: String,
                       start: Int,
                       end: Int,
                       score: Option[Double],
                       strand: Option[Char],
                       attributes: Map[String, String]
                     )

// Simple interval tree implementation
case class Interval[T](start: Int, end: Int, data: T)

class IntervalTree[T] {
  private val intervals = mutable.ListBuffer[Interval[T]]()

  def insert(interval: Interval[T]): Unit = {
    intervals += interval
  }

  def query(range: Interval[_]): List[T] = {
    intervals.filter(i => i.start <= range.end && i.end >= range.start).map(_.data).toList
  }
}

// Optimized function to load GFF3 records directly into interval trees
def loadIntoIntervalTrees(file: String): Map[String, IntervalTree[GFF3Record]] = {
  val trees = mutable.Map[String, IntervalTree[GFF3Record]]()
  val source = Source.fromFile(file)

  try {
    for (line <- source.getLines().filter(!_.startsWith("#"))) {
      val cols = line.split("\t")
      val record = GFF3Record(
        seqId = cols(0),
        source = cols(1),
        featureType = cols(2),
        start = cols(3).toInt,
        end = cols(4).toInt,
        score = if (cols(5) == ".") None else Some(cols(5).toDouble),
        strand = if (cols(6) == ".") None else Some(cols(6).head),
        attributes = cols(8).split(";").flatMap { attr =>
          attr.split("=", 2) match {
            case Array(key, value) => Some(key -> value)
            case _ => None
          }
        }.toMap
      )

      // Insert directly into the interval tree
      val tree = trees.getOrElseUpdate(record.seqId, new IntervalTree[GFF3Record]())
      tree.insert(Interval(record.start, record.end, record))
    }
  } finally {
    source.close()
  }

  trees.toMap
}

// Query function to find overlapping intervals for a given contig and range
def queryIntervalTree(
                       intervalTrees: Map[String, IntervalTree[GFF3Record]],
                       contig: String,
                       start: Int,
                       end: Int
                     ): List[GFF3Record] = {
  intervalTrees.get(contig) match {
    case Some(tree) => tree.query(Interval(start, end, ()))
    case None       => List.empty
  }
}

// Main object to demonstrate functionality
object GFF3AnnotationExample extends App {
  val gff3File = "gencode.v47.primary_assembly.annotation.gff3"

  // Load data into interval trees
  println("Loading data into interval trees...")
  val intervalTrees = loadIntoIntervalTrees(gff3File)
  println("Data loaded successfully.")

  // Example query
  val contig = "chr1"
  val start = 1000
  val end = 2000
  val overlappingFeatures = queryIntervalTree(intervalTrees, contig, start, end)

  // Print results
  println(s"Overlapping features for $contig:$start-$end:")
  overlappingFeatures.foreach(println)
}