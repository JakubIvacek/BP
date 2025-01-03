package utils
import java.io.File
import scala.sys.process._
import scala.util.{Try, Success, Failure}

object Gunzip {
  /**
   * Unzips a `.gz` file to its original form.
   *
   * @param filePath The path to the `.gz` file to be unzipped.
   * @return An `Option` containing the path to the unzipped file if successful
   */
  def unzipFile(filePath: String): Option[String] = {
    val inputFile = new File(filePath)

    if (!inputFile.exists()) {
      println(s"Input file does not exist: $filePath")
      return None
    }

    val outputFilePath = filePath.stripSuffix(".gz")
    val command = Seq("gunzip", filePath)

    println(s"Executing command: ${command.mkString(" ")}")

    val result = Try(command.!)

    result match {
      case Success(exitCode) if exitCode == 0 =>
        println(s"File unzipped successfully to: $outputFilePath")
        Some(outputFilePath)
      case Success(exitCode) =>
        println(s"Unzipping failed with exit code $exitCode. Command: ${command.mkString(" ")}")
        None
      case Failure(exception) =>
        println(s"Error unzipping file: ${exception.getMessage}. Command: ${command.mkString(" ")}")
        None
    }
  }

  /**
   * Compresses a file into a `.gz` archive.
   *
   * @param filePath The path to the file to be zipped.
   * @return An `Option` containing the path to the zipped file if successful
   */
  def zipFile(filePath: String): Option[String] = {
    val inputFile = new File(filePath)

    if (!inputFile.exists()) {
      println(s"Input file does not exist: $filePath")
      return None
    }

    val outputFilePath = s"$filePath.gz"
    val command = Seq("gzip", filePath)

    println(s"Executing command: ${command.mkString(" ")}")

    val result = Try(command.!)

    result match {
      case Success(exitCode) if exitCode == 0 =>
        println(s"File zipped successfully to: $outputFilePath")
        Some(outputFilePath)
      case Success(exitCode) =>
        println(s"Zipping failed with exit code $exitCode. Command: ${command.mkString(" ")}")
        None
      case Failure(exception) =>
        println(s"Error zipping file: ${exception.getMessage}. Command: ${command.mkString(" ")}")
        None
    }
  }
}

