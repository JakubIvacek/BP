package module

import database.modules.ServiceModules
import ftp.FtpClient
import pdb.ExtractPDB
import utils.RepositoryManager

import java.nio.file.{Files, Paths}

object UniprotModule extends ModuleManager {
  
  val server = "ftp.uniprot.org"
  val directory = "/pub/databases/uniprot/knowledgebase/complete/"
  val fileName = "uniprot_sprot.dat.gz"
  val fastaName = "uniprot_sprot.fasta.gz"

  /**
   * Downloads the latest release of Uniprot annotation files
   *
   * @param localPath The local directory where the files should be saved.
   * @param release   Can be empty because uniprot only one version at time on ftp                 
   */
  def downloadModule(localPath: String, release: String) : Unit = {
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

  /**
   * Downloads the latest release of Uniprot annotation files for hg38.
   *
   * @param localPath The local directory where the files should be saved.
   */
  def downloadModuleLatest(localPath: String): Unit = {
    downloadModule(localPath, "")
  }

  /**
   *  Uniprot Works on protein level doesnt need overlift
   *
   */
  def overLiftToT2T(outputPath: String, releaseNumber: String, downloadPath: String, filePath: String, fileNames: List[String]): Unit = {
    println("Works on protein level doesnt need overlift.")
  }

  /**
   * Removes a Uniprot module by its unique ID.
   *
   * @param id The module ID.
   */
  def removeModuleById(id: Int): Unit = {
    val module = ServiceModules.getModuleFromDatabaseById(id)
    module match {
      case Some(module) =>
        RepositoryManager.deleteRepository(module.locationPath.getOrElse("N/A")) //delete if location path present
      case None =>
        println("No module found with this information.")
    }
    ServiceModules.deleteModuleFromDatabaseById(id) //delete from database
  }

  /**
   * Prints information about all installed 1000genomes modules.
   */
  def printAllClassModules(): Unit = {
    val modules = ServiceModules.getModulesByName("uniprot")
    if (modules.isEmpty) {
      println("No Uniprot modules installed")
    } else {
      println("Uniprot MODULES -")
      modules.foreach(_.print())
    }
  }

  /**
   * Prints information about all modules.
   */
  def printAllModules(): Unit = {
    val modules = ServiceModules.getModules
    if (modules.isEmpty) {
      println("No modules installed")
    } else {
      println("ALL MODULES -")
      modules.foreach(_.print())
    }
  }

  /**
   * Help function to get uniprot directory path helps to check if module installed
   */
  def getPath(): String = {
    val path = ServiceModules.getUnitProtPath

    path match {
      case Some(path) => path
      case None => ""
    }
  }
}
