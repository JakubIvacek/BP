package utils

import scala.util.{Try, Success, Failure}
import java.io.{File, FileInputStream, FileOutputStream, IOException}
import scala.sys.process._

object FileStuff {
  /**
   * Moves a file from a source path to a destination path.
   *
   * @param sourcePath      The path to the source file.
   * @param destinationPath The path to the destination file.
   * @return An `Option` containing the destination path if the file is moved successfully
   */
  def moveFile(sourcePath: String, destinationPath: String): Option[String] = {
    val sourceFile = new File(sourcePath)

    if (!sourceFile.exists()) {
      println(s"Source file does not exist: $sourcePath")
      return None
    }

    val destinationFile = new File(destinationPath)

    Try(sourceFile.renameTo(destinationFile)) match {
      case Success(true) =>
        println(s"File moved successfully to: $destinationPath")
        Some(destinationPath)
      case Success(false) =>
        println(s"Failed to move file to: $destinationPath")
        None
      case Failure(exception) =>
        println(s"Error moving file: ${exception.getMessage}")
        None
    }
  }

  /**
   * Copies a file from a source path to a destination path using the `cp` command.
   *
   * @param sourcePath      The path to the source file.
   * @param destinationPath The path to the destination file.
   * @return An `Option` containing the destination path if the file is copied successfully
   */
  def copyFile(sourcePath: String, destinationPath: String): Option[String] = {
    val sourceFile = new File(sourcePath)

    if (!sourceFile.exists()) {
      println(s"Source file does not exist: $sourcePath")
      return None
    }

    val command = Seq("cp", sourcePath, destinationPath)

    println(s"Executing command: ${command.mkString(" ")}")

    val result = Try(command.! /* Executes the command */)

    result match {
      case Success(0) => // Exit code 0 indicates success
        println(s"File copied successfully to: $destinationPath")
        Some(destinationPath)
      case Success(exitCode) =>
        println(s"File copy failed with exit code $exitCode. Command: ${command.mkString(" ")}")
        None
      case Failure(exception) =>
        println(s"Error executing file copy: ${exception.getMessage}. Command: ${command.mkString(" ")}")
        None
    }
  }
}
