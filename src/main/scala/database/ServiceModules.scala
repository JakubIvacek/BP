package database

import java.sql.Timestamp
import java.time.Instant

object ServiceModules {
  /**
   * Add module information to database.
   *
   * @param name          module name (e.g., gencode)
   * @param version       module version (e.g., 16).
   * @param locationPath  location path on your device (e.g., "/users/keno/data/")
   * @param downloadPath  ftp download path of server + directory (e.g., server:directory)
   * @param overLift      Boolean value ( On - if overLift module add, Off - otherwise )
   * 
   */
  def addModuleToDatabase(name: String, version: String, locationPath: String, downloadPath: String, overLift: Boolean): Unit = {
    val connection = DatabaseConnection.getConnection

    try {
      //TablesCreate.createTableModules(DatabaseConnection.connection)
      // Determine the value for overlift
      val overlift = if (overLift) Some(Timestamp.from(Instant.now())) else None

      // Create the new Module
      val module = Module(
        id = None, // New modules don't have an ID yet
        name = name,
        created = Some(Timestamp.from(Instant.now())), // Set the created timestamp to now
        updated = Some(Timestamp.from(Instant.now())), // Set the updated timestamp to now
        version = version,
        overlift = overlift,
        locationPath = Some(locationPath),
        downloadPath = Some(downloadPath)
      )

      // Insert the module into the database
      QueryModules.insertModule(connection, module)
      println(s"Module '$name' with version '$version' added successfully to database.")


    } catch {
      case e: Exception =>
        println(s"An error occurred while adding the module: ${e.getMessage}")
        e.printStackTrace()
    } finally {
      val allModules = QueryModules.getAllModules(connection)
      println(s"Current modules in the database: $allModules")
      DatabaseConnection.closeConnection()
    }
  }

  /**
   * Delete module information from database.
   *
   * @param name         module name (e.g., gencode)
   * @param version      module version (e.g., 16).
   *
   */
  def deleteModuleFromDatabase(name: String, version: String): Unit = {
    val connection = DatabaseConnection.getConnection
    try {
      val maybeModule = QueryModules.findByNameVersion(connection, name, version).headOption

      maybeModule match {
        case Some(module) =>
          module.id match {
            case Some(id) =>
              QueryModules.deleteModuleById(connection, id)
              println(s"Module with id $id, name '$name', and version '$version' deleted successfully.")
            case None =>
              println(s"Module found for name '$name' and version '$version', but ID is None.")
          }
        case None =>
          println(s"No module found with name '$name' and version '$version'. Nothing to delete.")
      }
    } catch {
      case e: Exception =>
        println(s"An error occurred while deleting the module: ${e.getMessage}")
    } finally {
      val allModules = QueryModules.getAllModules(connection)
      println(s"Current modules in the database: $allModules")
      DatabaseConnection.closeConnection()
    }
  }
}
