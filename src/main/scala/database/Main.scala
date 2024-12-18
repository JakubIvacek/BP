package database

import java.sql.{Connection, Timestamp}

object Main {

  def main(args: Array[String]): Unit = {
    // Establish connection to the SQLite database
    DatabaseConnection.getConnection
    TablesCreate.createTableModules(DatabaseConnection.connection)
    // Example 1: Insert a new module
    val newModule = Module(
      id = None,
      name = "Module1",
      created = None,
      updated = None,
      version = "v1.0.0",
      overlift = None,
      locationPath = Some("/path/to/location"),
      downloadPath = Some("/path/to/download")
    )
    QueryModules.insertModule(DatabaseConnection.connection, newModule)

    // Example 2: Update the inserted module
    val updatedModule = newModule.copy(
      id = Some(1), // Assuming the inserted module has ID 1
      name = "Updated Module1",
      version = "v1.1.0"
    )
    QueryModules.updateModule(DatabaseConnection.connection, updatedModule)

    // Example 3: Retrieve and print all modules
    val allModules = QueryModules.getAllModules(DatabaseConnection.connection)
    println("All Modules:")
    allModules.foreach(_.print())

    // Example 4: Find modules by name
    val foundModules = QueryModules.findByName(DatabaseConnection.connection, "Updated Module1")
    println("Modules with the name 'Updated Module1':")
    foundModules.foreach(_.print())
    TablesCreate.dropTableModules(DatabaseConnection.connection)
    // Close the database connection
    DatabaseConnection.closeConnection()
  }
}

