package database.annotationruns

import java.sql.{Connection, PreparedStatement, ResultSet}

/**
 * Repository for operations on the "annotationruns" table.
 */
object RepositoryAnnotationRuns {

  /**
   * Insert a new annotation run record into the database.
   */
  def insertRun(connection: Connection, run: AnnotationRuns): Unit = {
    val insertQuery =
      """
        |INSERT INTO annotationruns (
        |  input_file,
        |  output_file,
        |  gencode_used,
        |  uniprot_used,
        |  cosmic_used,
        |  genomes1000_used,
        |  reference_genome,
        |  created,
        |  updated
        |) VALUES (
        |  ?, ?, ?, ?, ?, ?, ?, CURRENT_TIMESTAMP, CURRENT_TIMESTAMP
        |);
        |""".stripMargin

    val ps: PreparedStatement = connection.prepareStatement(insertQuery)
    ps.setString(1, run.inputFile.orNull)
    ps.setString(2, run.outputFile.orNull)
    ps.setString(3, run.gencodeUsed.orNull)
    ps.setString(4, run.uniprotUsed.orNull)
    ps.setString(5, run.cosmicUsed.orNull)
    ps.setString(6, run.genomes1000Used.orNull)
    ps.setString(7, run.referenceGenome)
    ps.executeUpdate()
  }

  /**
   * Retrieve all annotation run records from the database.
   */
  def getAllRuns(connection: Connection): List[AnnotationRuns] = {
    val selectQuery = "SELECT * FROM annotationruns;"
    val stmt = connection.createStatement()
    val rs: ResultSet = stmt.executeQuery(selectQuery)

    val buf = scala.collection.mutable.ListBuffer.empty[AnnotationRuns]
    while (rs.next()) {
      buf += AnnotationRuns(
        id               = Some(rs.getInt("id")),
        inputFile        = Option(rs.getString("input_file")),
        outputFile       = Option(rs.getString("output_file")),
        gencodeUsed      = Option(rs.getString("gencode_used")),
        uniprotUsed      = Option(rs.getString("uniprot_used")),
        cosmicUsed       = Option(rs.getString("cosmic_used")),
        genomes1000Used  = Option(rs.getString("genomes1000_used")),
        referenceGenome  = rs.getString("reference_genome"),
        created          = Option(rs.getTimestamp("created")),
        updated          = Option(rs.getTimestamp("updated"))
      )
    }
    buf.toList
  }
}
