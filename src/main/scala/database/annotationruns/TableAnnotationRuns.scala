package database.annotationruns

import java.sql.Connection

/**
 * Object for managing the "annotationruns" table in SQLite.
 */
object TableAnnotationRuns {

  /**
   * Creates the annotationruns table in the database if it does not exist.
   */
  def createTableAnnotationRuns(connection: Connection): Unit = {
    val createTableQuery =
      """
        |CREATE TABLE IF NOT EXISTS annotationruns (
        |    id                INTEGER PRIMARY KEY AUTOINCREMENT,
        |    input_file        TEXT,
        |    output_file       TEXT,
        |    gencode_used      TEXT,
        |    uniprot_used      TEXT,
        |    cosmic_used       TEXT,
        |    genomes1000_used  TEXT,
        |    reference_genome  TEXT NOT NULL,
        |    created           TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
        |    updated           TIMESTAMP DEFAULT CURRENT_TIMESTAMP
        |);
        |""".stripMargin

    val stmt = connection.createStatement()
    stmt.execute(createTableQuery)
    stmt.close()
  }

  /**
   * Drops the annotationruns table from the database if it exists.
   */
  def dropTableAnnotationRuns(connection: Connection): Unit = {
    val dropTableQuery =
      """
        |DROP TABLE IF EXISTS annotationruns;
        |""".stripMargin

    val stmt = connection.createStatement()
    stmt.execute(dropTableQuery)
    stmt.close()
    println("Table 'annotationruns' dropped successfully (if it existed)!")
  }
}


