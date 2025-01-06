package utils
import scala.sys.process._
import scala.util.{Try, Success, Failure}

object CommandLocator {
  /**
   * Finds the absolute path of a command using the `which` command.
   *
   * @param command The name of the command to locate (e.g., "ls", "CrossMap").
   * @return An `Option` containing the absolute path to the command if found.
   */
  def findCommandPath(command: String): Option[String] = {
    val whichCommand = Seq("which", command)

    println(s"Executing command: ${whichCommand.mkString(" ")}")

    val result = Try(whichCommand.!!) // Executes the `which` command and captures its output.

    result match {
      case Success(output) =>
        val path = output.trim
        if (path.nonEmpty) {
          println(s"Command found at: $path")
          Some(path)
        } else {
          println(s"Command not found: $command")
          None
        }
      case Failure(exception) =>
        println(s"Error executing `which`: ${exception.getMessage}")
        None
    }
  }
}
