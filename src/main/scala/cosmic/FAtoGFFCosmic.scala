package cosmic

import data.FaEntryCosmic
import java.io.{File, PrintWriter}

/**
 * Object for creating .gff file for .fasta cosmic entries
 */
object FAtoGFFCosmic {
  /**
   * Convert FaEntryCosmic objects to GFF format and write to file
   *
   * @param filePath    The path where the gff file will be created
   * @param features    Loaded FaEntryCosmic objects from .fasta file cosmic
   */
  def writeGFF(filePath: String, features: Seq[FaEntryCosmic]): Unit = {
    println(s"Converting cosmic .fa to .gff - $filePath")
    val file = new File(filePath)
    if (!file.getParentFile.exists()) {
      file.getParentFile.mkdirs() 
    }
    val writer = new PrintWriter(filePath)
    
    Utils.writeGFFHeader(writer)
    
    features.foreach { feature =>
      val attributes = s"Gene_ID=${feature.geneSymbol};" +
        s"Transcript_ID=${feature.transcriptAccession};"
      writer.println(s"${feature.chromosome}\tCosmic\ttranscript\t${feature.genomeStart}\t${feature.genomeStop}\t.\t${feature.strand}\t.\t$attributes")
    }
    writer.close()
  }
}
