package utils

import scala.util.{Try, Success, Failure}
import java.nio.file.{Files, Paths}
import java.io.File
import scala.sys.process._

object LiftOverGFF {
  private val chainFilePathHg38 = "liftover.chains/hg38-chm13v2.over.chain"
  private val crossMapPath = "/home/ivacek/.local/bin/CrossMap"

  /**
   * LiftOver GFF file using CrossMap.
   *
   * @param inputPath  Path to the input GFF file.
   * @param outputPath Path where to save the LiftOver GFF file.
   * @param filename   Name of output file
   */
  def liftOverGFF(inputPath: String, outputPath: String, filename: String): Unit = {
    if (!Files.exists(Paths.get(inputPath))) {
      println(s"Input file does not exist: $inputPath")
    }
    val localDir = new java.io.File(outputPath)
    if (!localDir.exists()) {
      localDir.mkdirs()
    }
    val command = Seq(
      crossMapPath,
      "gff",
      chainFilePathHg38,
      inputPath,
      s"$outputPath/$filename"
    )

    println(s"Executing command: ${command.mkString(" ")}")

    val result = Try(command.!)

    result match {
      case Success(exitCode) if exitCode == 0 =>
        println("LiftOver GFF completed successfully.")
      case Success(exitCode) =>
        println(s"LiftOver GFF failed with exit code $exitCode. Command: ${command.mkString(" ")}")
      case Failure(exception) =>
        println(s"Error executing LiftOver GFF: ${exception.getMessage}. Command: ${command.mkString(" ")}")
    }
  }
}
