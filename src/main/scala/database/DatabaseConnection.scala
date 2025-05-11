package database

import java.sql.{Connection, DriverManager}

object DatabaseConnection {
  private val url = "jdbc:sqlite:database.db"

  /** Vždy nová Connection, na začiatku migrácie/tabuliek, po skončení close(). */
  def getConnection: Connection = {
    val conn = DriverManager.getConnection(url)
    conn.setAutoCommit(false)

    // Môžete spustiť migrácie LEN raz pri starte aplikácie.
    // Tu ich spúšťame pri každom getConnection len pre ukážku – ideálne ich
    // volať niekde pri init-e, nie v každom vlákne.
    modules.TableModules.createTableModules(conn)
    annotationruns.TableAnnotationRuns.createTableAnnotationRuns(conn)

    conn
  }

  /** Zatvorenie Connection – volajte v finally bloku. */
  def closeConnection(conn: Connection): Unit = {
    if (conn != null && !conn.isClosed) {
      conn.close()
    }
  }
}


