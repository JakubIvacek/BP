package downloader

import database.modules.ServiceModules
import ftp.FtpClient
import pdb.ExtractPDB

import java.nio.file.{Files, Paths}

object UniProtDownload {
  val server = "ftp.uniprot.org"
  val directory = "/pub/databases/uniprot/knowledgebase/complete/"
  val fileName = "uniprot_sprot.dat.gz"
  val fastaName = "uniprot_sprot.fasta.gz"

  def download(localPath: String) : Unit = {
    if getPath().isEmpty then {
      val finalLocalPath = if localPath == "" then s"uniprot" else s"$localPath/uniprot"
      FtpClient.downloadSpecificFile(finalLocalPath, fileName, server, directory)
      FtpClient.downloadSpecificFile(finalLocalPath, fastaName, server, directory)
      // Process the downloaded file
      val inputFilePath = s"$finalLocalPath/$fileName"
      val outputFilePath = s"$finalLocalPath/uniprot_pdb_mappings.txt"
      ServiceModules.addModuleToDatabase("uniprot", "1", finalLocalPath, s"$server$directory", false, "")
      if (Files.exists(Paths.get(inputFilePath))) {
        ExtractPDB.extractPdbMappings(inputFilePath, outputFilePath)
      } else {
        println(s"Downloaded file not found at $inputFilePath.")
      }
    } else {
      println("Already installed.")
    }
  }


  def getPath(): String = {
    val path = ServiceModules.getUnitProtPath
    
    path match {
      case Some(path) => path
      case None => ""
    }
  }

  def main(args: Array[String]): Unit = {
    download("")
  }
}
