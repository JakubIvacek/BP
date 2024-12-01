import scala.sys.process._
import scala.util.{Try, Success, Failure}

object LiftOverVcf {
  
  private val chainFilePathHg38 = "liftover.chains/hg38-chm13v2.over.chain"
  private val chainFilePathHg19 = "liftover.chains/hg19-chm13v2.over.chain"
  private val referencePath = "reference/t2t/chm13v2.0.fa"
  
  def liftOverVcf(inputFile: String, hg38: Boolean): Option[String] = {
    val newPath = s"liftovered/$inputFile"
    val chainFilePath = if hg38 then chainFilePathHg38 else chainFilePathHg19
    val gatkCommand = Seq(
      "gatk",
      "LiftoverVcf",
      "-I", inputFile,
      "-O", newPath,
      "-CHAIN", chainFilePath,
      "-REJECT", "rejected.vcf",
      "-R", referencePath
    )
    val result = Try(gatkCommand.!)

    result match {
      case Success(exitCode) =>
        if (exitCode == 0) Some(newPath)
        else {
          println(s"LiftoverVcf failed with exit code $exitCode.")
          None
        }
      case Failure(exception) =>
        println(s"Error executing LiftoverVcf: ${exception.getMessage}")
        None
    }
  }
}
