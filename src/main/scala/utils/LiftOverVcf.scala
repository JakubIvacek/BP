package utils

import database.DatabaseConnection

import java.nio.file.{Files, Paths}
import scala.sys.process.*
import scala.util.{Failure, Success, Try}

object LiftOverVcf {

  private val chainFilePathHg38 = "liftover.chains/hg38-chm13v2.over.chain"
  private val chainFilePathHg19 = "liftover.chains/hg19-chm13v2.over.chain"
  private val referencePath = "reference/t2t/chm13v2.0.fa"
  private val gatkPath = "/opt/gatk-4.4.0.0/gatk"

  /**
   * LiftOver Vcf using Gatk tool
   *
   * @param inputFile  path to input File    
   * @param hg38       reference version hg38 or hg19 (e.g., TRUE IF hg38).
   * @param outputPath path where to save overlifted file
   * @param fileName   name of output file name
   */
  def liftOverVcf(inputFile: String, hg38: Boolean, outputPath: String, fileName: String): Option[String] = {
    if (!Files.exists(Paths.get(inputFile))) {
      println(s"Input file does not exist: $inputFile")
      return None
    }

    val localDir = new java.io.File(outputPath)
    if (!localDir.exists()) {
      localDir.mkdirs()
    }

    val newPath = s"$outputPath/$fileName"
    val rejectPath = s"$outputPath/rejected-$fileName"
    val chainFilePath = if (hg38) chainFilePathHg38 else chainFilePathHg19

    val gatkCommand = Seq(
      gatkPath,
      "LiftoverVcf",
      "-I", inputFile,
      "-O", newPath,
      "-CHAIN", chainFilePath,
      "-REJECT", rejectPath,
      "-R", referencePath
    )

    println(s"Executing command: ${gatkCommand.mkString(" ")}")

    val result = Try(gatkCommand.!)

    result match {
      case Success(exitCode) if exitCode == 0 =>
        println("LiftoverVcf completed successfully.")
        Some(newPath)
      case Success(exitCode) =>
        println(s"LiftoverVcf failed with exit code $exitCode. Command: ${gatkCommand.mkString(" ")}")
        None
      case Failure(exception) =>
        println(s"Error executing LiftoverVcf: ${exception.getMessage}. Command: ${gatkCommand.mkString(" ")}")
        None
    }
  }
}
