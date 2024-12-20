package database.modules

import utils.RepositoryManager

import java.sql.Timestamp

case class Module(
                   id: Option[Int],
                   name: String,
                   created: Option[Timestamp] = None,
                   updated: Option[Timestamp] = None,
                   version: String,
                   versionReference: String,
                   overlift: Option[Timestamp],
                   locationPath: Option[String],
                   downloadPath: Option[String]
                 )
{
  /**
   * Print information about module
   */
  def print(): Unit = {
    println(s"\nModule [ID: ${id.getOrElse("N/A")}, Name: $name, Version: $version, versionReference: $versionReference]")
    println(s"Created: ${created.getOrElse("N/A")}, Updated: ${updated.getOrElse("N/A")}")
    println(s"Overlift: ${overlift.getOrElse("N/A")}")
    println(s"Location Path: ${locationPath.getOrElse("N/A")}")
    println(s"Download Path: ${downloadPath.getOrElse("N/A")}")
    println(s"Local file size: ${RepositoryManager.getDirectorySize(locationPath.getOrElse("N/A"))} B \n")
  }
}
