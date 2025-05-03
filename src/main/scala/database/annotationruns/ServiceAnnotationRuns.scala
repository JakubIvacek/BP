package database.annotationruns

import database.DatabaseConnection

/**
 * Service for managing annotation run records in the database.
 */
object ServiceAnnotationRuns {

  /**
   * Add a new annotation run to the database.
   *
   * @param inputFile        Path to the input VCF file
   * @param outputFile       Path to the output MAF file
   * @param gencodeUsed      Gencode resource used
   * @param uniprotUsed      UniProt resource used
   * @param cosmicUsed       COSMIC resource used
   * @param genomes1000Used  1000 Genomes resource used
   * @param referenceGenome  Reference genome identifier
   */
  def addAnnotationRun(
                        inputFile: String,
                        outputFile: String,
                        gencodeUsed: String,
                        uniprotUsed: String,
                        cosmicUsed: String,
                        genomes1000Used: String,
                        referenceGenome: String
                      ): Unit = {
    val connection = DatabaseConnection.getConnection
    try {
      val run = AnnotationRuns(
        id              = None,
        inputFile       = Some(inputFile),
        outputFile      = Some(outputFile),
        gencodeUsed     = Some(gencodeUsed),
        uniprotUsed     = Some(uniprotUsed),
        cosmicUsed      = Some(cosmicUsed),
        genomes1000Used = Some(genomes1000Used),
        referenceGenome = referenceGenome,
        created         = None,
        updated         = None
      )

      RepositoryAnnotationRuns.insertRun(connection, run)
      println(s"Annotation run record for input '$inputFile' added successfully.")
    } catch {
      case e: Exception =>
        println(s"Error adding annotation run: ${e.getMessage}")
        e.printStackTrace()
    } finally {
      DatabaseConnection.closeConnection()
    }
  }

  /**
   * Retrieve all annotation runs from the database.
   */
  def getAllAnnotationRuns: List[AnnotationRuns] = {
    val connection = DatabaseConnection.getConnection
    try {
      RepositoryAnnotationRuns.getAllRuns(connection)
    } catch {
      case e: Exception =>
        println(s"Error retrieving annotation runs: ${e.getMessage}")
        List.empty
    } finally {
      DatabaseConnection.closeConnection()
    }
  }
}


