package utils

import java.io.{FileInputStream, FileOutputStream}
import downloader.HttpDownload
/**
 * Utility to download and decompress:
 * 1. hg38→CHM13v2.0 chain file
 * 2. CHM13v2.0 whole-assembly FASTA
 */
object ChainReferenceDownload {
  // URLs for the resources
  private val chainUrl = "http://hgdownload.soe.ucsc.edu/hubs/GCA/009/914/755/GCA_009914755.4/liftOver/chm13v2-hg38.over.chain.gz"
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
    val gzName    = s"$destDir/chm13.fa.gz"
    HttpDownload.downloadFile(fastaUrl, gzName)
    Gunzip.unzipFile(gzName)
  }
}

