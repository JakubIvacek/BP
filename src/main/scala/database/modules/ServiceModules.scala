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
      DatabaseConnection.closeConnection(connection)
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
          //println(s"No module found with name '$name' and version '$version'. Nothing to retrieve.")
          None 
      }
    } catch {
      case e: Exception =>
        println(s"An error occurred while retrieving the module: ${e.getMessage}")
        None
    } finally {
      DatabaseConnection.closeConnection(connection)
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
      DatabaseConnection.closeConnection(connection)
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
      DatabaseConnection.closeConnection(connection)
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
      DatabaseConnection.closeConnection(connection)
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

        DatabaseConnection.closeConnection(connection)
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

      DatabaseConnection.closeConnection(connection)
    }
  }
  
  /**
   * Get the path of the newest gencode module based on the versionReference.
   *
   * @param versionReference Module genome reference version (e.g., hg38).
   * @return Option[(String, String)] Tuple containing paths to the newest gencode annotation and genome file, or None if not found.
   */
  def getNewestModulePathGenCode(versionReference: String): Option[(String, String)] = {
    val connection = DatabaseConnection.getConnection
    val faName = "GRCh38.primary_assembly.genome.fa.gz"
    val faNameT2T = "chm13.fa"
    val faFinalName = if versionReference == "hg38" then faName else faNameT2T
    try {
      // Retrieve all gencode modules with the specified versionReference
      val modules = RepositoryModules.findByNameAndReference(connection, "gencode", versionReference)

      // Find the module with the highest version number
      modules.maxByOption(_.version.toInt).flatMap { module =>
        module.locationPath.map { path =>
          val annotationFile = s"gencode.v${module.version}.annotation.gff3.gz"
          (s"$path/$annotationFile", s"$path/$faFinalName")
        }
      }
    } catch {
      case e: Exception =>
        println(s"An error occurred while retrieving the newest gencode module: ${e.getMessage}")
        None
    } finally {
      DatabaseConnection.closeConnection(connection)
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
      modules.maxByOption(_.version.toLong).map(_.version).getOrElse("0")
    } catch {
      case e: Exception =>
        println(s"An error occurred while retrieving the newest module: ${e.getMessage}")
        "0"
    } finally {
      DatabaseConnection.closeConnection(connection)
    }
  }

  def getUnitProtPath: Option[String] = {
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
      DatabaseConnection.closeConnection(connection)
    }
  }

  def getUnitProtTimeStamp: Option[Timestamp] = {
    val connection = DatabaseConnection.getConnection
    try {
      // Retrieve all unitprot modules
      val modules = RepositoryModules.findByName(connection, "uniprot")
      // Return the path if any modules are found
      modules.headOption.flatMap(_.created)
    } catch {
      case e: Exception =>
        println(s"An error occurred while retrieving the newest unitprot module: ${e.getMessage}")
        None
    } finally {
      DatabaseConnection.closeConnection(connection)
    }
  }

  def getNewestModulePath(moduleName: String, refGenome: String): Option[String] = {
    val connection = DatabaseConnection.getConnection
    try {
      // Retrieve all
      val modules = RepositoryModules.findByNameAndReference(connection, moduleName, refGenome)
      // Return the path if any modules are found
      modules.maxByOption(_.version.toLong).flatMap(_.locationPath)
    } catch {
      case e: Exception =>
        println(s"An error occurred while retrieving the newest $moduleName $refGenome module path: ${e.getMessage}")
        None
    } finally {
      DatabaseConnection.closeConnection(connection)
    }
  }

  def getNewestModule(moduleName: String, refGenome: String): Option[Module] = {
    val connection = DatabaseConnection.getConnection
    try {
      // Retrieve all unitprot modules
      val modules = RepositoryModules.findByNameAndReference(connection, moduleName, refGenome)
      // Return the path if any modules are found
      modules.maxByOption(_.version.toLong)
    } catch {
      case e: Exception =>
        println(s"An error occurred while retrieving the newest $moduleName $refGenome module path: ${e.getMessage}")
        None
    } finally {
      DatabaseConnection.closeConnection(connection)
    }
  }

  /**
   * Create tables to create table for Modules
   */
  def createTables(): Unit = {
    val connection = DatabaseConnection.getConnection
    TableModules.createTableModules(connection)
    DatabaseConnection.closeConnection(connection)
  }

  /**
   * Drop tables to create drop for Modules
   */
  def dropTables(): Unit = {
    val connection = DatabaseConnection.getConnection
    TableModules.dropTableModules(connection)
    DatabaseConnection.closeConnection(connection)
  }
}
