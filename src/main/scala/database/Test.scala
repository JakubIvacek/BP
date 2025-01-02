package database

import database.modules.{Module, RepositoryModules, TableModules}

import java.sql.{Connection, Timestamp}

object Test {

  def main(args: Array[String]): Unit = {
    DatabaseConnection.getConnection
    TableModules.createTableModules(DatabaseConnection.connection)
    val newModule = Module(
      id = None,
      name = "Module1",
      created = None,
      updated = None,
      version = "v1.0.0",
      versionReference = "hg38",
      overlift = None,
      locationPath = Some("/path/to/location"),
      downloadPath = Some("/path/to/download")
    )
    RepositoryModules.insertModule(DatabaseConnection.connection, newModule)

    val updatedModule = newModule.copy(
      id = Some(1), 
      name = "Updated Module1",
      version = "v1.1.0"
    )
    RepositoryModules.updateModule(DatabaseConnection.connection, updatedModule)
    
    val allModules = RepositoryModules.getAllModules(DatabaseConnection.connection)
    println("All Modules:")
    allModules.foreach(_.print())
    
    val foundModules = RepositoryModules.findByName(DatabaseConnection.connection, "Updated Module1")
    println("Modules with the name 'Updated Module1':")
    foundModules.foreach(_.print())
    TableModules.dropTableModules(DatabaseConnection.connection)
    
    DatabaseConnection.closeConnection()
  }
}

