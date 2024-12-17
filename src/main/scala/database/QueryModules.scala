package database

import java.sql.{Connection, PreparedStatement, ResultSet, Timestamp}
import java.time.LocalDateTime

object QueryModules {

  // INSERT operation
  def insertModule(connection: Connection, module: Module): Unit = {
    val insertQuery =
      """
        |INSERT INTO modules (name, created, updated, version, overlift, location_path, download_path)
        |VALUES (?, CURRENT_TIMESTAMP, CURRENT_TIMESTAMP, ?, ?, ?, ?);
        |""".stripMargin

    val preparedStatement: PreparedStatement = connection.prepareStatement(insertQuery)
    setModuleParameters(preparedStatement, module)
    preparedStatement.executeUpdate()
    println(s"Module '${module.name}' inserted successfully!")
  }

  // UPDATE operation: Update a module by ID
  def updateModule(connection: Connection, module: Module): Unit = {
    val updateQuery =
      """
        |UPDATE modules
        |SET name = ?, updated = CURRENT_TIMESTAMP, version = ?, overlift = ?, location_path = ?, download_path = ?
        |WHERE id = ?;
        |""".stripMargin

    val preparedStatement: PreparedStatement = connection.prepareStatement(updateQuery)
    setModuleParameters(preparedStatement, module, includeId = true)
    preparedStatement.executeUpdate()
    println(s"Module with ID ${module.id.get} updated successfully!")
  }

  // SELECT operation: Retrieve all modules
  def getAllModules(connection: Connection): List[Module] = {
    val selectQuery = "SELECT * FROM modules;"
    val statement = connection.createStatement()
    val resultSet: ResultSet = statement.executeQuery(selectQuery)

    val modules = mapResultSetToModules(resultSet)
    println("Modules retrieved successfully!")
    modules
  }

  // SELECT operation: Find modules by name
  def findByName(connection: Connection, moduleName: String): List[Module] = {
    val selectQuery =
      """
        |SELECT * FROM modules WHERE name = ?;
        |""".stripMargin

    val preparedStatement: PreparedStatement = connection.prepareStatement(selectQuery)
    preparedStatement.setString(1, moduleName)

    val resultSet: ResultSet = preparedStatement.executeQuery()
    val modules = mapResultSetToModules(resultSet)

    println(s"Modules with name '$moduleName' retrieved successfully!")
    modules
  }

  // DELETE operation: Delete a module by ID
  def deleteModuleById(connection: Connection, id: Int): Unit = {
    val deleteQuery = "DELETE FROM modules WHERE id = ?;"
    val preparedStatement: PreparedStatement = connection.prepareStatement(deleteQuery)
    preparedStatement.setInt(1, id)

    preparedStatement.executeUpdate()
    println(s"Module with ID $id deleted successfully!")
  }

  
  // Helper method to set module parameters in a PreparedStatement
  private def setModuleParameters(preparedStatement: PreparedStatement, module: Module, includeId: Boolean = false): Unit = {
    preparedStatement.setString(1, module.name)
    preparedStatement.setString(2, module.version)
    preparedStatement.setTimestamp(3, module.overlift.orNull)
    preparedStatement.setString(4, module.locationPath.orNull)
    preparedStatement.setString(5, module.downloadPath.orNull)

    if (includeId) {
      preparedStatement.setInt(6, module.id.getOrElse(0))
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
        overlift = Option(resultSet.getTimestamp("overlift")),
        locationPath = Option(resultSet.getString("location_path")),
        downloadPath = Option(resultSet.getString("download_path"))
      )
    }
    modules.toList
  }
}
