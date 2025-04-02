package utils

import logfiles.RefChainDirManager

import scala.util.{Failure, Success, Try}
import java.nio.file.{Files, Paths}
import java.io.File
import scala.annotation.tailrec
import scala.sys.process.*


/**
 * LiftOver GFF and VCF files to T2T from hg38
 * Automatically download CrossMap tool if not located on device
 */
object LiftOverTool {
  private var chainFilePathHg38toT2T: Option[String] = None
  private var referencePathT2T: Option[String] = None
  private var pathToCrossMap = ""
  private val hg38toT2TChainName = "hg38-chm13.over.chain"
  private val T2TRefName = "chm13.fa"

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
    checkIfPathsSet()
    checkWhereCrossMapLocated()
    val command = Seq(
      pathToCrossMap,
      "gff",
      chainFilePathHg38toT2T.getOrElse(""),
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

  /**
   * LiftOver VCF using CrossMap tool
   *
   * @param inputFile  path to input VCF file
   * @param outputPath path where to save overlifted file
   * @param outputFileName name of overlifted file
   */
  def liftOverVcf(inputFile: String, outputPath: String, outputFileName: String): Option[String] = {
    if (!Files.exists(Paths.get(inputFile))) {
      println(s"Input file does not exist: $inputFile")
      return None
    }

    val localDir = new java.io.File(outputPath)
    if (!localDir.exists()) {
      localDir.mkdirs()
    }
    checkIfPathsSet()
    checkWhereCrossMapLocated()
    // Command for CrossMap VCF liftover
    val crossMapCommand = Seq(
      pathToCrossMap,         
      "vcf",                
      chainFilePathHg38toT2T.getOrElse(""),
      inputFile,
      referencePathT2T.getOrElse(""),
      s"$outputPath/$outputFileName"
    )

    println(s"Executing command: ${crossMapCommand.mkString(" ")}")

    val result = Try(crossMapCommand.! )

    result match {
      case Success(exitCode) if exitCode == 0 =>
        println("LiftOver VCF completed successfully.")
        Some(outputPath)
      case Success(exitCode) =>
        println(s"Liftover VCF failed with exit code $exitCode. Command: ${crossMapCommand.mkString(" ")}")
        None
      case Failure(exception) =>
        println(s"Error executing Liftover VCF: ${exception.getMessage}. Command: ${crossMapCommand.mkString(" ")}")
        None
    }
  }
  private def checkIfPathsSet(): Unit = {
    if chainFilePathHg38toT2T.isEmpty
    then chainFilePathHg38toT2T = Some(RefChainDirManager.getChainFileDir.getOrElse("") + s"/$hg38toT2TChainName")
    if referencePathT2T.isEmpty
    then referencePathT2T = Some(RefChainDirManager.getReferenceFileDir.getOrElse("") + s"/$T2TRefName")
  }
  /**
   * Check whether the CrossMap tool is available on the system.
   * If not found, attempt to install or update it using Pip.
   * Aborts if the installation fails after the maximum retries.
   */
  private def checkWhereCrossMapLocated(): Unit = {
    var found = false
    var retries = 3
    while (!found && retries > 0) {
      CommandLocator.findCommandPath("CrossMap") match {
        case Some(path) =>
          pathToCrossMap = path
          found = true
        case None =>
          println(s"CrossMap not found. Trying install.")
          if (!PipInstaller.installPythonPackage("CrossMap")) {
            retries -= 1
            if (retries == 0) {
              println("Failed to install CrossMap after multiple attempts. Aborting.")
              return
            }
          }
      }
    }
  }

}
