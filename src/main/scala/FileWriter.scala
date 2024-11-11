import java.io.{File, PrintWriter}
import scala.collection.mutable.ListBuffer
import htsjdk.variant.vcf.VCFHeader

object FileWriter {
  def writeVcfFile(outputFilePath: String, header: VCFHeader, variants: ListBuffer[DnaVariant]): Unit = {
    val writer = new PrintWriter(new File(outputFilePath))
    try {
      // Write the header lines to the output file
      header.getMetaDataInInputOrder.forEach { line =>
        //writer.println(line.toString)
      }

      // Write each variant to the output file
      variants.foreach { variant =>
        
      }
    } finally {
      writer.close() // Ensure the writer is closed
    }
  }
}