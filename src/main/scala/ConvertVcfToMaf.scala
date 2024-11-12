import scala.collection.mutable.ListBuffer

object ConvertVcfToMaf {
  def convert(inputFile: String, outputFile: String): Unit = {
    val dnaVariants: ListBuffer[DnaVariant] = FileReaderVcf.read(inputFile)
    WriteToMaf.writeMafFile(dnaVariants, outputFile)
  }

  def main(args: Array[String]): Unit = {
    if (args.length != 2) {
      println("Not enough args Usage: ConvertVcfToMaf <inputFile> <outputFile>")
    } else {
      convert(args(0), args(1))
    }
  }
}
