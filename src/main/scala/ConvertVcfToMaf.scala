
import database.DatabaseConnection
//import utils.LiftOverVcf

import scala.collection.mutable.ListBuffer

object  ConvertVcfToMaf {
  def convert(inputFile: String, outputFile: String): Unit = {
    val dnaVariants: ListBuffer[DnaVariant] = FileReaderVcf.read(inputFile) //read vcf
    annotateVariants(dnaVariants.toList) //anotate
    WriteToMaf.writeMafFile(dnaVariants, outputFile) //output to maf file
  }

  private def annotateVariants(dnaVariants: List[DnaVariant]): Unit = {
    for (variant <- dnaVariants) {
      println("start")
      var overlappingEntries:  List[GffEntry] = GFFReader.parseMatchGff3File("gencode.v47.primary_assembly.annotation.gff3",variant.position.toInt, variant.contig)
      println(overlappingEntries.length)
      // Combine attributes from overlapping entries
      variant.geneID = overlappingEntries.flatMap(_.attributes.get("gene_id"))
        .distinct
        .mkString(",") match {
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
    }
  }

  def main(args: Array[String]): Unit = {
    convert("Small.vcf", "Annotated.maf")
  }
}
