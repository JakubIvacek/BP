package database.annotationruns

import java.sql.Timestamp

case class AnnotationRuns(
                   id: Option[Int],
                   inputFile: Option[String],
                   outputFile: Option[String],
                   gencodeUsed: Option[String],
                   uniprotUsed: Option[String],
                   cosmicUsed: Option[String],
                   genomes1000Used: Option[String],
                   referenceGenome: String,
                   created:   Option[Timestamp] = None,
                   updated:   Option[Timestamp] = None,
                 ) {
  /**
   * Print information about this annotation run record
   */
  def print(): Unit = {
    println(s"\nRun [ID: ${id.getOrElse("N/A")}]")
    println(s"Input File:         ${inputFile.getOrElse("N/A")}")
    println(s"Output File:        ${outputFile.getOrElse("N/A")}")
    println(s"Gencode Used:       ${gencodeUsed.getOrElse("N/A")}")
    println(s"UniProt Used:       ${uniprotUsed.getOrElse("N/A")}")
    println(s"COSMIC Used:        ${cosmicUsed.getOrElse("N/A")}")
    println(s"1000G Genomes Used: ${genomes1000Used.getOrElse("N/A")}")
    println(s"Reference genome Used : ${referenceGenome}")
    println(s"Created:            ${created.getOrElse("N/A")}, Updated: ${updated.getOrElse("N/A")}")
  }
}
