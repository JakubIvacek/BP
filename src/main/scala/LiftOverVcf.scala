import scala.sys.process._
import scala.util.{Try, Success, Failure}

import scala.sys.process._
import scala.util.{Try, Success, Failure}
import java.nio.file.{Paths, Files}

object LiftOverVcf {

  private val chainFilePathHg38 = "liftover.chains/hg38-chm13v2.over.chain"
  private val chainFilePathHg19 = "liftover.chains/hg19-chm13v2.over.chain"
  private val referencePath = "reference/t2t/chm13v2.0.fa"

  def liftOverVcf(inputFile: String, hg38: Boolean): Option[String] = {
    if (!Files.exists(Paths.get(inputFile))) {
      println(s"Input file does not exist: $inputFile")
      return None
    }
    val newPath = s"liftovered/$inputFile"
    val rejectPath = s"rejected/$inputFile"
    val chainFilePath = if (hg38) chainFilePathHg38 else chainFilePathHg19

    val gatkCommand = Seq(
      "gatk",
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
