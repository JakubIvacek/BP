package utils

import java.io.{File, FileInputStream, FileOutputStream, PrintWriter}
import java.nio.file.{Files, Paths}
import java.util.zip.GZIPInputStream
import scala.io.Source
import downloader.HttpDownload
/**
 * Utility to download and decompress:
 * 1. hg38→CHM13v2.0 chain file
 * 2. CHM13v2.0 whole-assembly FASTA
 */
object ChainReferenceDownload {
  // URLs for the resources
  private val chainUrl = "http://hgdownload.soe.ucsc.edu/hubs/GCA/009/914/755/GCA_009914755.4/liftOver/hg38-chm13v2.over.chain.gz"
  private val fastaUrl = "http://hgdownload.soe.ucsc.edu/hubs/GCA/009/914/755/GCA_009914755.4/GCA_009914755.4.fa.gz"

  /**
   * Download and decompress the hg38→CHM13v2.0 chain file.
   * @param destDir Directory to place the files (defaults to current directory)
   */
  def downloadChain(destDir: String = "."): Unit = {
    val gzName    = s"$destDir/hg38-chm13.over.chain.gz"
    HttpDownload.downloadFile(chainUrl, gzName)
    Gunzip.unzipFile(gzName)
  }

  /**
   * Download and decompress the CHM13v2.0 whole-assembly FASTA.
   * @param destDir Directory to place the files (defaults to current directory)
   */
  def downloadFasta(destDir: String = "."): Unit = {
    val gzName    = s"$destDir/chm13NonPrefx.fa.gz"
    HttpDownload.downloadFile(fastaUrl, gzName)
    Gunzip.unzipFile(gzName)
    prefixChromosomeHeaders(s"$destDir/chm13NonPrefx.fa",s"$destDir/chm13.fa" )
  }

  /**
   * Read a FASTA, and rewrite headers so that
   * the nth sequence gets ">chrN <original_accession>"
   * but when N == 23 or 24, uses "X" or "Y" instead.
   *
   * @param inputPath  Path to the decompressed FASTA file.
   * @param outputPath Path to write the rewritten FASTA.
   */
  def prefixChromosomeHeaders(inputPath: String, outputPath: String): Unit = {
    // chrom order: 1–22, then X, then Y
    val contigOrder = (1 to 22).map(_.toString) ++ Seq("X", "Y")

    val in = Source.fromFile(inputPath)
    val out = new PrintWriter(new File(outputPath))
    try {
      var idx = 0
      for (line <- in.getLines()) {
        if (line.startsWith(">")) {
          val acc = line.substring(1).trim
          val label =
            if (idx < contigOrder.length) contigOrder(idx)
            else (idx + 1).toString // fallback to numeric
          out.println(s">chr$label $acc")
          idx += 1
        } else {
          out.println(line)
        }
      }
    } finally {
      in.close()
      out.close()
    }
  }
}

