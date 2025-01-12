package tests
import database.modules.ServiceModules
import module.GenCodeModule

import scala.io.StdIn
import scala.util.{Failure, Success, Try}

object TestMain {
  def main(args: Array[String]): Unit = {
    ServiceModules.createTables()
    var continue = true
    while (continue) {
      println("Enter: \nE (exit) \nD (download module) \nP (print modules) \nR (remove module)")
      var input = StdIn.readLine().trim.toLowerCase
      input match {
        case "e" =>
          continue = false // Exit the loop
        case "p" =>
          GenCodeModule.printAllClassModules()
        case "d" =>
          println("Enter module number to download: \n1 - GenCode \n")
          var input = StdIn.readLine().trim.toLowerCase
          input match {
            case "1" => downloadGenCode()
          }
        case "r" =>
          println("Enter module number to remove: \n1 - GenCode \n")
          var input = StdIn.readLine().trim.toLowerCase
          input match {
            case "1" => {
              println("Enter ID of the module to remove (e.g., 13):")
              val idInput = StdIn.readLine().trim
              GenCodeModule.removeModuleById(idInput.toInt) //remove with id
            }
          }
        case _ =>
          println(s"Unrecognized command: '$input'")
      }
    }
  }

  def downloadGenCode(): Unit = {
    println("Enter: \n1 to download the latest version \n2 to download with a specific version")
    val input = StdIn.readLine().trim
    println("Enter the path where to download or (yes) for the working directory:")
    var path = StdIn.readLine().trim
    if path == "yes" then path = ""
    input match {
      case "1" =>
        GenCodeModule.downloadModuleLatest(path) // Download the latest version
      case "2" =>
        println("Enter version (e.g., 40):")
        val version = StdIn.readLine().trim
        GenCodeModule.downloadModule(path, version) // Download specific version
      case _ =>
        println(s"Invalid choice: '$input'. Please select 1 or 2.")
    }
  }
}
