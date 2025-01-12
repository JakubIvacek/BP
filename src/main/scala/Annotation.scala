import scala.collection.mutable.ListBuffer


object Annotation {
  def annotateVariantsGencode(dnaVariants: List[DnaVariant], referenceGenome: String): Unit = {
    for (variant <- dnaVariants) {
      //println("start")
      //val path = database.modules.ServiceModules.getNewestModulePathGenCode("hg38")
      var overlappingEntries: List[GffEntry] = GFFReader.parseMatchGff3File("gencode.v47.annotation.gff3", variant.position.toInt, variant.contig)
      println(overlappingEntries.length)
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
    Annotation.annotateVariantsGencode(dnaVariants.toList, referenceGenome) //anotate
    WriteToMaf.writeMafFile(dnaVariants, outputFile) //output to maf file
  }
}
