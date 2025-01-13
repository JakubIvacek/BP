import scala.io.Source
import scala.util.Try
import scala.collection.mutable.ListBuffer
import scala.collection.mutable

class IntervalTree {
  private val tree = mutable.Map[String, mutable.TreeSet[GffEntry]]()

  implicit val ordering: Ordering[GffEntry] = Ordering.by(_.start)

  def insert(entry: GffEntry): Unit = {
    tree.getOrElseUpdate(entry.contig, mutable.TreeSet[GffEntry]()).add(entry)
  }

  def search(contig: String, position: Int): List[GffEntry] = {
    tree.get(contig) match {
      case Some(entries) =>
        entries.filter(entry => entry.start <= position && entry.end >= position).toList
      case None => List()
    }
  }

  def findClosestUpstream(contig: String, position: Int): Option[GffEntry] = {
    tree.get(contig) match {
      case Some(entries) =>
        entries.filter(_.end < position).maxByOption(_.end)
      case None => None
    }
  }

  def findClosestDownstream(contig: String, position: Int): Option[GffEntry] = {
    tree.get(contig) match {
      case Some(entries) =>
        entries.filter(_.start > position).minByOption(_.start)
      case None => None
    }
  }
}

object GFFReaders {
  var contingSaved = ""
  var intervalTree = new IntervalTree()

  def parseAttributes(attributeString: String): Map[String, String] = {
    attributeString.split(";").map { attr =>
      val keyValue = attr.split("=")
      if (keyValue.length == 2) keyValue(0) -> keyValue(1) else keyValue(0) -> ""
    }.toMap
  }

  def parseLine(line: String, conting: String): Option[GffEntry] = {
    val fields = line.split("\t")

    if (fields.length < 9) {
      None
    } else {
      val contigFile = fields(0)
      val startFile = Try(fields(3).toInt).getOrElse(0)
      val endFile = Try(fields(4).toInt).getOrElse(0)
      if (contigFile == conting){
        Some(GffEntry(contigFile, startFile, endFile, parseAttributes(fields(8))))
      }else None
    }
  }

  def parseGff3File(filename: String, contig: String): Unit = {
    val source = Source.fromFile(filename)

    var newIntervalTree = new IntervalTree()

    source.getLines()
      .filterNot(_.startsWith("#"))
      .flatMap(line => parseLine(line, contig))
      .foreach(newIntervalTree.insert)

    intervalTree = newIntervalTree
    contingSaved = contig

    source.close()
  }
  
  def matchGffEntries(entries: List[GffEntry], dnaVariant: DnaVariant): List[GffEntry] = {
    val contig = dnaVariant.contig
    val pos = dnaVariant.position

    var closestUpstream: Option[GffEntry] = None
    var closestDownstream: Option[GffEntry] = None

    
    val result = entries.flatMap { entry =>
      val contigFile = entry.contig
      val startFile = entry.start
      val endFile = entry.end

      
      if (contig == contigFile && pos >= startFile && pos <= endFile) {
        Some(entry)  
      } else if (contig == contigFile) {
        if (pos > endFile) {
          closestUpstream match {
            case None => closestUpstream = Some(entry)
            case Some(existing) if existing.end < entry.end => closestUpstream = Some(entry)
            case _ => 
          }
          None  
        } else if (pos < startFile) {
          closestDownstream match {
            case None => closestDownstream = Some(entry)
            case Some(existing) if existing.start > entry.start => closestDownstream = Some(entry)
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
    
    val resultWithUpstreamDownstream = result ++ closestUpstream.toList ++ closestDownstream.toList
    resultWithUpstreamDownstream
  }

  def annotateVariantsGencode(dnaVariants: List[DnaVariant], referenceGenome: String): Unit = {
    for (variant <- dnaVariants) {
      //println("start")
      //val path = database.modules.ServiceModules.getNewestModulePathGenCode("hg38")
      // Determine overlapping entries
      val normalizedContig = variant.contig.trim.toLowerCase
      val normalizedSaved = GFFReaders.contingSaved.trim.toLowerCase
      val Entries = if (normalizedContig == normalizedSaved) {
        GFFReaders.intervalTree
      } else {
        println(variant.contig)
        GFFReaders.parseGff3File("gencode.v47.annotation.gff3", variant.contig)
      }
      var overlappingEntries = GFFReaders.intervalTree.search(variant.contig, variant.position.toInt)
      if (overlappingEntries.isEmpty) overlappingEntries ++= GFFReaders.intervalTree.findClosestUpstream(variant.contig, variant.position.toInt)
        ++ GFFReaders.intervalTree.findClosestDownstream(variant.contig, variant.position.toInt)
      //println(overlappingEntries.length)
      // Combine attributes from overlapping entries
      variant.geneID = overlappingEntries.flatMap(_.attributes.get("gene_id"))
        .distinct.mkString(",") match {
        case "" => "."
        case result => result
      }
      variant.geneName = overlappingEntries.flatMap(_.attributes.get("gene_name")).
        distinct.mkString(",") match {
        case "" => "."
        case result => result
      }
      variant.geneType = overlappingEntries.flatMap(_.attributes.get("gene_type")).
        distinct.mkString(",") match {
        case "" => "."
        case result => result
      }
      variant.transID = overlappingEntries.flatMap(_.attributes.get("transcript_id")).
        distinct.mkString(",") match {
        case "" => "."
        case result => result
      }
      variant.transID = overlappingEntries.flatMap(_.attributes.get("transcript_name")).
        distinct.mkString(",") match {
        case "" => "."
        case result => result
      }
      variant.transType = overlappingEntries.flatMap(_.attributes.get("transcript_type")).
        distinct.mkString(",") match {
        case "" => "."
        case result => result
      }
      variant.exonID = overlappingEntries.flatMap(_.attributes.get("exon_id")).
        distinct.mkString(",") match {
        case "" => "."
        case result => result
      }
      variant.exonNum = overlappingEntries.flatMap(_.attributes.get("exon_number")).
        distinct.mkString(",") match {
        case "" => "."
        case result => result
      }
      variant.level = overlappingEntries.flatMap(_.attributes.get("level")).
        distinct.mkString(",") match {
        case "" => "."
        case result => result
      }
      variant.NCBIBuild = referenceGenome
    }
  }

  def annotate(inputFile: String, outputFile: String, referenceGenome: String): Unit = {
    val dnaVariants: ListBuffer[DnaVariant] = FileReaderVcf.read(inputFile) //read vcf
    annotateVariantsGencode(dnaVariants.toList, referenceGenome) //anotate
    WriteToMaf.writeMafFile(dnaVariants, outputFile) //output to maf file
  }

  def main(args: Array[String]): Unit = {
    annotate("Small.vcf", "Annotated.maf", "hg38")
  }
}
