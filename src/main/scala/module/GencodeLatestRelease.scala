package module

import org.apache.commons.net.ftp.FTPClient
import ftp.FtpClient

object GencodeLatestRelease {
  def main(args: Array[String]): Unit = {
    println(FtpClient.downloadSpecificGencodeAnnotation("C:\\Users\\ivace", 40))
  }
}

