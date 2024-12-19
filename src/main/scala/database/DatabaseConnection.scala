package database

import java.sql.{Connection, DriverManager}

object DatabaseConnection {
  private val url = "jdbc:sqlite:database.db"
  var connection: Connection = _

  /**
   * Start connection to database
   */
  def getConnection: Connection = {
    if (connection == null || connection.isClosed) {
      connection = DriverManager.getConnection(url)
      //println("Connection to SQLite database established: " + url)
    }
    connection
  }

  /**
   * Close connection to database
   */
  def closeConnection(): Unit = {
    if (connection != null && !connection.isClosed) {
      connection.close()
      //println("Connection to SQLite database closed: " + url)
    }
  }
}

