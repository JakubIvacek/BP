package database

import java.sql.{Connection, PreparedStatement, ResultSet, Timestamp}
import java.time.LocalDateTime

object RepositoryModules {

  /**
   * Insert Module to Database
   * @param connection Database connection
   * @param module     Module to add
   */
  def insertModule(connection: Connection, module: Module): Unit = {
    val insertQuery =
      """
        |INSERT INTO modules (name, created, updated, version, versionReference, overlift, location_path, download_path)
        |VALUES (?, CURRENT_TIMESTAMP, CURRENT_TIMESTAMP, ?, ?, ?, ?, ?);
        |""".stripMargin
    val preparedStatement: PreparedStatement = connection.prepareStatement(insertQuery)
    setModuleParameters(preparedStatement, module)
    preparedStatement.executeUpdate()
    println(s"Module '${module.name}' inserted successfully!")
  }

  /**
   * Insert Module to Database
   * @param connection Database connection
   * @param module     Module with updated information
   */
  def updateModule(connection: Connection, module: Module): Unit = {
    val updateQuery =
      """
        |UPDATE modules
        |SET name = ?, updated = CURRENT_TIMESTAMP, version = ?, versionReference = ?, overlift = ?, location_path = ?, download_path = ?
        |WHERE id = ?;
        |""".stripMargin

    val preparedStatement: PreparedStatement = connection.prepareStatement(updateQuery)
    setModuleParameters(preparedStatement, module, includeId = true)
    preparedStatement.executeUpdate()
    println(s"Module with ID ${module.id.get} updated successfully!")
  }

  /**
   * Retrieve all modules SELECT
   * @param connection Database connection
   */
  def getAllModules(connection: Connection): List[Module] = {
    val selectQuery = "SELECT * FROM modules;"
    val statement = connection.createStatement()
    val resultSet: ResultSet = statement.executeQuery(selectQuery)

    val modules = mapResultSetToModules(resultSet)
    println("Modules retrieved successfully!")
    modules
  }

  /**
   * Retrieve by name modules SELECT
   * @param connection Database connection
   * @param moduleName Name of module to find
   *                   
   */
  def findByName(connection: Connection, moduleName: String): List[Module] = {
    val selectQuery =
      """
        |SELECT * FROM modules WHERE name = ?;
        |""".stripMargin

    val preparedStatement: PreparedStatement = connection.prepareStatement(selectQuery)
    preparedStatement.setString(1, moduleName)

    val resultSet: ResultSet = preparedStatement.executeQuery()
    val modules = mapResultSetToModules(resultSet)

    //println(s"Modules with name '$moduleName' retrieved successfully!")
    modules
  }

  /**
   * Retrieve by id modules SELECT
   *
   * @param connection Database connection
   * @param id         Module id
   *
   */
  def findById(connection: Connection, id: Int): List[Module] = {
    val selectQuery =
      """
        |SELECT * FROM modules WHERE id = ?;
        |""".stripMargin

    val preparedStatement: PreparedStatement = connection.prepareStatement(selectQuery)
    preparedStatement.setString(1, id.toString)
    val resultSet: ResultSet = preparedStatement.executeQuery()
    val modules = mapResultSetToModules(resultSet)
    modules
  }

  /**
   * Retrieve by name and version modules SELECT
   * @param connection Database connection
   * @param moduleName Name of module to find    (e.g., gencode)
   * @param version    Version of module to find (e.g., 37)
   * @param versionReference    Version of module dna reference to find (e.g., hg38)               
   */
  def findByNameVersion(connection: Connection, moduleName: String, version: String, versionReference: String): List[Module] = {
    val selectQuery =
      """
        |SELECT * FROM modules WHERE name = ? AND version = ? AND versionReference = ?;
        |""".stripMargin

    val preparedStatement: PreparedStatement = connection.prepareStatement(selectQuery)
    preparedStatement.setString(1, moduleName)
    preparedStatement.setString(2, version)
    preparedStatement.setString(3, versionReference)
    val resultSet: ResultSet = preparedStatement.executeQuery()
    val modules = mapResultSetToModules(resultSet)

    println(s"Modules with name and version'$moduleName' retrieved successfully!")
    modules
  }

  /**
   * DELETE operation: Delete a module by ID
   *
   * @param connection Database connection
   * @param id         Id of module in database
   */
  def deleteModuleById(connection: Connection, id: Int): Unit = {
    val deleteQuery = "DELETE FROM modules WHERE id = ?;"
    val preparedStatement: PreparedStatement = connection.prepareStatement(deleteQuery)
    preparedStatement.setInt(1, id)

    preparedStatement.executeUpdate()
    println(s"Module with ID $id deleted successfully!")
  }

  /**
   * Helper method to set module parameters in a PreparedStatement
   *
   * @param preparedStatement Statement in which to insert data
   * @param module            Module with all the data      
   * @param includeId          So there is not much duplicate code and two functions can use similar code
   */
  private def setModuleParameters(preparedStatement: PreparedStatement, module: Module, includeId: Boolean = false): Unit = {
    preparedStatement.setString(1, module.name)
    preparedStatement.setString(2, module.version)
    preparedStatement.setString(3, module.versionReference)
    preparedStatement.setTimestamp(4, module.overlift.orNull)
    preparedStatement.setString(5, module.locationPath.orNull)
    preparedStatement.setString(6, module.downloadPath.orNull)

    if (includeId) {
      preparedStatement.setInt(7, module.id.getOrElse(0))
    }
  }

  // Helper method to map ResultSet to List[Module]
  private def mapResultSetToModules(resultSet: ResultSet): List[Module] = {
    val modules = scala.collection.mutable.ListBuffer[Module]()

    while (resultSet.next()) {
      modules += Module(
        id = Some(resultSet.getInt("id")),
        name = resultSet.getString("name"),
        created = Option(resultSet.getTimestamp("created")),
        updated = Option(resultSet.getTimestamp("updated")),
        version = resultSet.getString("version"),
        versionReference = resultSet.getString("versionReference"),
        overlift = Option(resultSet.getTimestamp("overlift")),
        locationPath = Option(resultSet.getString("location_path")),
        downloadPath = Option(resultSet.getString("download_path"))
      )
    }
    modules.toList
  }
}
