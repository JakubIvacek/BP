package commandLine

import java.nio.file.{Files, Paths, StandardOpenOption}

/** Saves/Retrieves path from path.log file */
object PathSaver {

  val logFilePath = "path.log"
  /** Gets the path either from user input or the log file */
  def getPath: Option[String] = {
    if (Files.exists(Paths.get(logFilePath))) {
      val defaultPath = Files.readAllLines(Paths.get(logFilePath))
      if defaultPath.isEmpty then None
      else {
        println(s"Using default path from log file: $defaultPath")
        Some(defaultPath.get(0))
      }
    } else {
      None
    }
  }

  /** Saves the given path to the log file */
  def savePathToLogFile(path: String): Unit = {
    try {
      Files.write(
        Paths.get(logFilePath),
        path.getBytes,
        StandardOpenOption.CREATE,
        StandardOpenOption.TRUNCATE_EXISTING
      )
      println(s"Saved path to log file: $logFilePath")
    } catch {
      case ex: Exception =>
        println(s"Failed to save path to log file: ${ex.getMessage}")
    }
  }
}
