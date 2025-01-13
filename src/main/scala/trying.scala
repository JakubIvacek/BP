import scala.io.Source
import scala.util.Try
import scala.collection.mutable.ListBuffer

object GFFReaders {
  var contingSaved = ""
  var entries = List[GffEntry]()

  def parseAttributes(attributeString: String): Map[String, String] = {
    attributeString.split(";").map { attr =>
      val keyValue = attr.split("=")
      if (keyValue.length == 2) keyValue(0) -> keyValue(1) else keyValue(0) -> ""
    }.toMap
  }
  // Method to parse and match a single line of GFF3 into a GffEntry
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
  // Method to parse the entire GFF3 file and return a List of GffEntry objects
  def parseGff3File(filename: String, contig: String): List[GffEntry] = {
    val source = Source.fromFile(filename)

    // Process the file line by line and collect entries into a new list
    val newEntries = source.getLines()
      .filterNot(_.startsWith("#")) // Skip comment lines
      .flatMap { line =>
        parseLine(line, contig) // Parse each line and collect the entries
      }.toList

    // Update the global entries with the new entries
    entries = newEntries // Accumulate new entries
    contingSaved = contig // Save the current contig

    source.close() // Always close the file
    newEntries // Return the newly parsed entries
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
        GFFReaders.entries
      } else {
        println(variant.contig)
        GFFReaders.parseGff3File("gencode.v47.annotation.gff3", variant.contig)
      }
      val overlappingEntries = GFFReaders.matchGffEntries(Entries, variant).take(30)
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
