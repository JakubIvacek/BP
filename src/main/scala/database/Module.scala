package database

import java.sql.Timestamp

case class Module(
                   id: Option[Int],
                   name: String,
                   created: Option[Timestamp] = None,
                   updated: Option[Timestamp] = None,
                   version: String,
                   overlift: Option[Timestamp],
                   locationPath: Option[String],
                   downloadPath: Option[String]
                 )
{
  // Print method to display module details in a formatted way
  def print(): Unit = {
    println(s"\nModule [ID: ${id.getOrElse("N/A")}, Name: $name, Version: $version]")
    println(s"Created: ${created.getOrElse("N/A")}, Updated: ${updated.getOrElse("N/A")}")
    println(s"Overlift: ${overlift.getOrElse("N/A")}")
    println(s"Location Path: ${locationPath.getOrElse("N/A")}")
    println(s"Download Path: ${downloadPath.getOrElse("N/A")}\n")
  }
}
