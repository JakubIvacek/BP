package logfiles

import java.nio.file.{Files, Paths, StandardOpenOption}

object RefChainDirManager {
  val logFilePath = "refChain.log"

  /** Gets the chain file directory from the log file */
  def getChainFileDir: Option[String] = {
    getPaths match {
      case Some((_, chainFilesDirPath)) => Some(chainFilesDirPath)
      case None => None
    }
  }

  /** Gets the reference file directory from the log file */
  def getReferenceFileDir: Option[String] = {
    getPaths match {
      case Some((referencesDirPath, _)) => Some(referencesDirPath)
      case None => None
    }
  }

  /** Gets both paths from the log file */
  def getPaths: Option[(String, String)] = {
    if (Files.exists(Paths.get(logFilePath))) {
      val defaultPaths = Files.readAllLines(Paths.get(logFilePath))
      if (defaultPaths.isEmpty || defaultPaths.size < 2) then None
      else {
        val referencesDirPath = defaultPaths.get(0)
        val chainFilesDirPath = defaultPaths.get(1)
        Some((referencesDirPath, chainFilesDirPath))
      }
    } else {
      None
    }
  }

  /** Saves the given paths to the log file */
  def savePathsToLogFile(referencesDirPath: String, chainFilesDirPath: String): Unit = {
    try {
      val paths = s"$referencesDirPath\n$chainFilesDirPath"
      Files.write(
        Paths.get(logFilePath),
        paths.getBytes,
        StandardOpenOption.CREATE,
        StandardOpenOption.TRUNCATE_EXISTING
      )
      println(s"Saved dir paths to log file: $logFilePath")
    } catch {
      case ex: Exception =>
        println(s"Failed to save dir paths to log file: ${ex.getMessage}")
    }
  }

}
