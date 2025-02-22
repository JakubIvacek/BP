package database.modules

import database.DatabaseConnection

import java.sql.Timestamp
import java.time.Instant

object ServiceModules {
  /**
   * Add module information to database.
   *
   * @param name              module name (e.g., gencode)
   * @param version           module version (e.g., 16).
   * @param locationPath      location path on your device (e.g., "/users/keno/data/")
   * @param downloadPath      ftp download path of server + directory (e.g., server:directory)
   * @param overLift          Boolean value ( On - if overLift module add, Off - otherwise )
   * @param versionReference  module genome reference version                 
   * 
   */
  def addModuleToDatabase(name: String, version: String, locationPath: String, downloadPath: String, overLift: Boolean, versionReference: String): Unit = {
    val connection = DatabaseConnection.getConnection

    try {
      //TablesCreate.createTableModules(DatabaseConnection.connection)
      val overlift = if (overLift) Some(Timestamp.from(Instant.now())) else None

      // Create the new Module
      val module = Module(
        id = None, 
        name = name,
        created = Some(Timestamp.from(Instant.now())),
        updated = Some(Timestamp.from(Instant.now())),
        version = version,
        versionReference = versionReference,
        overlift = overlift,
        locationPath = Some(locationPath),
        downloadPath = Some(downloadPath)
      )

      // Insert the module into the database
      RepositoryModules.insertModule(connection, module)
      println(s"Module '$name' with version '$version' added successfully to database.")


    } catch {
      case e: Exception =>
        println(s"An error occurred while adding the module: ${e.getMessage}")
        e.printStackTrace()
    } finally {
      //val allModules = RepositoryModules.getAllModules(connection)
      //allModules.foreach(_.print())
      DatabaseConnection.closeConnection()
    }
  }

  /**
   * Get specific module
   *
   * @param name             module name (e.g., gencode)
   * @param version          module version (e.g., 16).
   * @param versionReference module reference version (e.g., hg38).
   * @return                 Option[Module] if found, otherwise None.
   */
  def getModuleFromDatabase(name: String, version: String, versionReference: String): Option[Module] = {
    val connection = DatabaseConnection.getConnection
    try {
      // Attempt to find the module in the repository
      val maybeModule = RepositoryModules.findByNameVersion(connection, name, version, versionReference).headOption
      maybeModule match {
        case Some(module) =>
          Some(module) 
        case None =>
          println(s"No module found with name '$name' and version '$version'. Nothing to retrieve.")
          None 
      }
    } catch {
      case e: Exception =>
        println(s"An error occurred while retrieving the module: ${e.getMessage}")
        None
    } finally {
      DatabaseConnection.closeConnection()
    }
  }

  /**
   * Get specific module by Id
   *
   * @param id            module id
   */
  def getModuleFromDatabaseById(id: Int): Option[Module] = {
    val connection = DatabaseConnection.getConnection
    try {
      // Attempt to find the module in the repository
      val maybeModule = RepositoryModules.findById(connection, id).headOption
      maybeModule match {
        case Some(module) =>
          Some(module)
        case None =>
          println(s"No module found with id '$id'. Nothing to retrieve.")
          None
      }
    } catch {
      case e: Exception =>
        println(s"An error occurred while retrieving the module: ${e.getMessage}")
        None
    } finally {
      DatabaseConnection.closeConnection()
    }
  }
  /**
   * Delete module information from database.
   *
   * @param name         module name (e.g., gencode)
   * @param version      module version (e.g., 16).
   * @param versionReference      module reference version (e.g., hg38).
   */
  def deleteModuleFromDatabase(name: String, version: String, versionReference: String): Unit = {
    val connection = DatabaseConnection.getConnection
    try {
      val maybeModule = RepositoryModules.findByNameVersion(connection, name, version, versionReference).headOption

      maybeModule match {
        case Some(module) =>
          module.id match {
            case Some(id) =>
              RepositoryModules.deleteModuleById(connection, id)
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
      //val allModules = RepositoryModules.getAllModules(connection)
      //allModules.foreach(_.print())
      DatabaseConnection.closeConnection()
    }
  }

  /**
   * Delete module information from database by Id.
   *
   * @param id            module id
   */
  def deleteModuleFromDatabaseById(id: Int): Unit = {
    val connection = DatabaseConnection.getConnection
    try {
      val maybeModule = RepositoryModules.findById(connection, id).headOption

      maybeModule match {
        case Some(module) =>
          RepositoryModules.deleteModuleById(connection, id)
        case None =>
          println(s"No module found with id $id. Nothing to delete.")
      }
    } catch {
      case e: Exception =>
        println(s"An error occurred while deleting the module: ${e.getMessage}")
    } finally {
      DatabaseConnection.closeConnection()
    }
  }
  /**
   * Get all modules with same name (e.g, gencode)
   */
  def getModulesByName(name: String): List[Module] = {
    val connection = DatabaseConnection.getConnection
    try {
      val modules = RepositoryModules.findByName(connection, name)
      modules
    } catch {
      case e: Exception =>
        println(s"An error occurred while getting the modules: ${e.getMessage}")
        List()
    } finally {

        DatabaseConnection.closeConnection()
    }
  }

  /**
   * Get all modules
   */
  def getModules: List[Module] = {
    val connection = DatabaseConnection.getConnection
    try {
      val modules = RepositoryModules.getAllModules(connection)
      modules
    } catch {
      case e: Exception =>
        println(s"An error occurred while getting the modules: ${e.getMessage}")
        List()
    } finally {

      DatabaseConnection.closeConnection()
    }
  }

  /**
   * Get the path of the newest gencode module based on the versionReference.
   *
   * @param versionReference Module genome reference version (e.g., hg38).
   * @return Option[String] Path to the newest gencode module or None if not found.
   */
  def getNewestModulePathGenCode(versionReference: String): Option[String] = {
    val connection = DatabaseConnection.getConnection
    try {
      // Retrieve all gencode modules with the specified versionReference
      val modules = RepositoryModules.findByNameAndReference(connection, "gencode", versionReference)

      // Find the module with the highest version number
      val newestModule = modules.maxByOption(_.version.toInt)

      newestModule match {
        case Some(module) =>
          module.locationPath match {
            case Some(path) => {
              val fileName = s"gencode.v${module.version}.annotation.gff3.gz"
              Some(path + "/" + fileName)
            }
            case None =>
              println(s"Newest module found but locationPath is not set.")
              None
          }
        case None =>
          println(s"No modules found for gencode with versionReference '$versionReference'.")
          None
      }
    } catch {
      case e: Exception =>
        println(s"An error occurred while retrieving the newest gencode module: ${e.getMessage}")
        None
    } finally {
      DatabaseConnection.closeConnection()
    }
  }

  /**
   * Get the path of the gencode module reference file by version
   *
   * @param versionReference Module genome reference version (e.g., hg38, t2t).
   * @return Option[String] Path to the newest gencode module or None if not found.
   */
  def getReferenceFilePathGenCode(versionReference: String): Option[String] = {
    val connection = DatabaseConnection.getConnection
    try {
      // Retrieve all gencode modules with the specified versionReference
      val modules = RepositoryModules.findByNameAndReference(connection, "gencode", versionReference)
      
      val module = modules.headOption
      module match {
        case Some(module) =>
          module.locationPath match {
            case Some(path) => {
              val fileName = if versionReference == "hg38" then "GRCh38.primary_assembly.genome.fa.gz" else if (versionReference == "t2t") then "chm13v2.0.fa" else ""
              println(s"Reference file path : $path/$fileName")
              Some(path + "/" + fileName)
            }
            case None =>
              println(s"Module found but locationPath is not set.")
              None
          }
        case None =>
          println(s"No modules found for gencode with versionReference '$versionReference'.")
          None
      }
    } catch {
      case e: Exception =>
        println(s"An error occurred while retrieving gencode module: ${e.getMessage}")
        None
    } finally {
      DatabaseConnection.closeConnection()
    }
  }
  /**
   * Get the version of newest module
   *
   * @param moduleName Module name
   * @return Option[String] Newst module version
   */
  def getNewestModuleVersion(moduleName: String): String = {
    val connection = DatabaseConnection.getConnection
    try {
      // Retrieve all gencode modules with the specified versionReference
      val modules = RepositoryModules.findByName(connection, moduleName)

      // Find the module with the highest version number
      modules.maxByOption(_.version.toInt).map(_.version).getOrElse("0")
    } catch {
      case e: Exception =>
        println(s"An error occurred while retrieving the newest gencode module: ${e.getMessage}")
        "0"
    } finally {
      DatabaseConnection.closeConnection()
    }
  }

  def getUnitProtPath(): Option[String] = {
    val connection = DatabaseConnection.getConnection
    try {
      // Retrieve all unitprot modules
      val modules = RepositoryModules.findByName(connection, "uniprot")
      // Return the path if any modules are found
      modules.headOption.flatMap(_.locationPath)
    } catch {
      case e: Exception =>
        println(s"An error occurred while retrieving the newest unitprot module: ${e.getMessage}")
        None
    } finally {
      DatabaseConnection.closeConnection()
    }
  }

  /**
   * Create tables to create table for Modules
   */
  def createTables(): Unit = {
    DatabaseConnection.getConnection
    TableModules.createTableModules(DatabaseConnection.connection)
    DatabaseConnection.closeConnection()
  }

  /**
   * Drop tables to create drop for Modules
   */
  def dropTables(): Unit = {
    DatabaseConnection.getConnection
    TableModules.dropTableModules(DatabaseConnection.connection)
    DatabaseConnection.closeConnection()
  }
}
