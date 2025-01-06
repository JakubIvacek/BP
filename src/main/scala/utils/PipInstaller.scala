package utils

import scala.sys.process._
import scala.util.{Try, Success, Failure}

object PipInstaller {
  /**
   * Installs a Python package using `pip install`.
   *
   * @param packageName The name of the Python package to install (e.g., "CrossMap").
   * @return A `Boolean` indicating whether the installation was successful.
   */
  def installPythonPackage(packageName: String): Boolean = {
    val command = Seq("pip", "install", packageName)

    println(s"Executing command: ${command.mkString(" ")}")

    val result = Try(command.!) // Executes the command and captures the exit code.

    result match {
      case Success(0) => // Exit code 0 indicates success
        println(s"Package '$packageName' installed successfully.")
        true
      case Success(exitCode) =>
        println(s"Package installation failed with exit code $exitCode. Command: ${command.mkString(" ")}")
        false
      case Failure(exception) =>
        println(s"Error executing pip install: ${exception.getMessage}. Command: ${command.mkString(" ")}")
        false
    }
  }
}
