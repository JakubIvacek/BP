
import database.DatabaseConnection
//import utils.LiftOverVcf

import scala.collection.mutable.ListBuffer

object  ConvertVcfToMaf {
  def convert(inputFile: String, outputFile: String): Unit = {
    val dnaVariants: ListBuffer[DnaVariant] = FileReaderVcf.read(inputFile)
    WriteToMaf.writeMafFile(dnaVariants, outputFile)
  }

  def convertWithLiftOver(inputFile: String, outputFile: String, variantType: String): Unit = {
    //liftover file first
    val newPath = variantType match {
      //case "hg38" => LiftOverVcf.liftOverVcf(inputFile, true, "")
      //case "hg19" => LiftOverVcf.liftOverVcf(inputFile, false, "")
      case _ =>
        println(s"Unsupported variant type: $variantType")
        return
    }
    newPath match {
      case Some(newPath) =>
        //convert to maf
        val dnaVariants: ListBuffer[DnaVariant] = FileReaderVcf.read(newPath)
        WriteToMaf.writeMafFile(dnaVariants, outputFile)
      case None =>
        println(s"Failed to liftover VCF file: $inputFile")
    }
  }
  
  //run command sbt "runMain ConvertVcfToMaf Lynch.vcf output.maf ?hg38/19?"
  def main(args: Array[String]): Unit = {
    if (args.length < 2) {
      println("Not enough args Usage: ConvertVcfToMaf <inputFile> <outputFile> <optional(hg38/hg19)>")
    }else if (args.length == 3){
      convertWithLiftOver(args(0), args(1), args(3))
    } else {
      convert(args(0), args(1))
    }
  }
}
