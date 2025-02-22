package database.modules

import database.DatabaseConnection

import java.sql.Connection

object TableModules {

  /**
   * Add table Modules from database
   */
  def createTableModules(connection: Connection): Unit = {
    val createTableQuery =
      """
        |CREATE TABLE IF NOT EXISTS modules (
        |    id INTEGER PRIMARY KEY AUTOINCREMENT,
        |    name TEXT NOT NULL,
        |    created TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
        |    updated TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
        |    version TEXT NOT NULL,
        |    versionReference TEXT NOT NULL,
        |    overlift TIMESTAMP,
        |    location_path TEXT,
        |    download_path TEXT
        |);
        |""".stripMargin

    val statement = connection.createStatement()
    statement.execute(createTableQuery)
    //println("Table modules created successfully!")
  }

  /**
   * Drop table Modules from database
   */
  def dropTableModules(connection: Connection): Unit = {
    val dropTableQuery =
      """
        |DROP TABLE IF EXISTS modules;
        |""".stripMargin

    val statement = connection.createStatement()
    statement.execute(dropTableQuery)
    println("Table modules dropped successfully (if it existed)!")
  }
}
