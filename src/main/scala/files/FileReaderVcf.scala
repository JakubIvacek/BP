package files

import java.io.File
import scala.collection.mutable.ListBuffer
import htsjdk.variant.vcf.VCFFileReader
import data.DnaVariant

object FileReaderVcf {
  private var vcfReader: Option[VCFFileReader] = None
  private var iterator: Option[java.util.Iterator[htsjdk.variant.variantcontext.VariantContext]] = None

  def open(inputFile: String): Unit = {
    close()
    vcfReader = Some(new VCFFileReader(new File(inputFile), false))
    iterator  = Some(vcfReader.get.iterator())
  }

  def readBatch(batchSize: Int): ListBuffer[DnaVariant] = {
    val variantList = ListBuffer[DnaVariant]()
    var count = 0

    iterator match {
      case Some(it) =>
        while (it.hasNext && count < batchSize) {
          val vc = it.next()
          try {
            val dnaVars = DnaVariant.createDnaVariants(vc)
            variantList ++= dnaVars
            count       += dnaVars.size
          } catch {
            case e: htsjdk.tribble.TribbleException.InternalCodecException =>
              // log and skip
              System.err.println(
                //s"Skipping ${vc.getContig}:${vc.getStart} – ${e.getMessage}"
              )
            // you could catch Throwable if you want to be extra-robust
          }
        }
      case None =>
        throw new IllegalStateException("VCF reader not initialized. Call open() first.")
    }

    variantList
  }

  def close(): Unit = {
    vcfReader.foreach(_.close())
    vcfReader = None
    iterator  = None
  }
}



