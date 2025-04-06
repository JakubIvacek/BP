package module

import java.util.concurrent.{Executors, ScheduledExecutorService, TimeUnit}
import scala.concurrent.duration.*
import ftp.{FtpClient, FtpClientGencode}
import database.modules.{RepositoryModules, ServiceModules}
import logfiles.RefChainDirManager
import utils.{FileStuff, Gunzip, LiftOverTool, RepositoryManager}

/**
 * Gencode module
 * This module is responsible for managing Gencode annotation files and their associated operations.
 * It provides functionalities to download, manage, and remove Gencode annotation files for hg38 and T2T references.
 *
 * Features:
 * 1. Download specific Gencode annotation releases or the latest release.
 * 2. Overlift annotation files from hg38 to T2T reference versions.
 * 3. Remove Gencode modules by ID or by specific information.
 * 4. Print saved module information for Gencode or all modules.
 */
object GenCodeModule extends ModuleManager {

  private val referenceFile = "GRCh38.primary_assembly.genome.fa.gz"  // Name of the reference file on the FTP server
  private val server = "ftp.ebi.ac.uk"                                // FTP server hosting the Gencode files
  //private var scheduler: Option[ScheduledExecutorService] = None

  /**
   * Downloads a specific release of Gencode annotation files for hg38.
   *
   * @param localPath      The local directory where the files should be saved.
   * @param releaseNumber  The Gencode release number (e.g., "39").
   */
  override def downloadModule(localPath: String, releaseNumber: String): Unit = {
    val latestRelease = FtpClientGencode.findLatestVersionGencode()
    var release = s"release_$releaseNumber"

    // If a release higher than the latest is entered, default to the latest release
    release = if releaseNumber > latestRelease.stripPrefix("release_") then latestRelease else release
    val newReleaseNumber = if releaseNumber > latestRelease.stripPrefix("release_") then latestRelease.stripPrefix("release_") else releaseNumber

    val directory = s"/pub/databases/gencode/Gencode_human/$release/"
    val fileName = s"gencode.v${release.stripPrefix("release_")}.annotation.gff3.gz"

    // Construct the local path for saving the files
    val finalLocalPath = if localPath == "" then s"gencode/$newReleaseNumber/hg38" else s"$localPath/gencode/$newReleaseNumber/hg38"

    // Check if this not already installed
    val versionInstalledCheck = ServiceModules.getModuleFromDatabase("gencode", newReleaseNumber, "hg38")
    if (release.nonEmpty && versionInstalledCheck.isEmpty) {
      // Download module and save
      FtpClient.downloadSpecificFile(finalLocalPath, fileName, server, directory)
      FtpClient.downloadSpecificFile(finalLocalPath, referenceFile, server, directory)
      //Add to database
      ServiceModules.addModuleToDatabase("gencode", newReleaseNumber, finalLocalPath, s"$server$directory", false, "hg38")
      // Overlift module to T2T
      val finalOverLiftPath = if localPath == "" then s"gencode/$newReleaseNumber/t2t" else s"$localPath/gencode/$newReleaseNumber/t2t"
      overLiftToT2T(finalOverLiftPath, newReleaseNumber, server + directory, finalLocalPath, List(fileName))
    } else {
      println("Could not determine the latest Gencode release or version installed already.")
    }
  }

  /**
   * Downloads the latest release of Gencode annotation files for hg38.
   *
   * @param localPath  The local directory where the files should be saved.
   */
  override def downloadModuleLatest(localPath: String): Unit = {
    val latestRelease = FtpClientGencode.findLatestVersionGencode()
    if (latestRelease.nonEmpty) {
      downloadModule(localPath, latestRelease.stripPrefix("release_"))
    } else {
      println("Could not determine the latest Gencode release.")
    }
  }

  /**
   * Overlifts Gencode annotation files from hg38 to the T2T reference version.
   *
   * @param outputPath    The local directory where the files should be saved.
   * @param releaseNumber The Gencode release number.
   * @param downloadPath  The URL path to the FTP server.
   * @param filePath      The path to the input file to be overlifted.
   * @param fileNames      The List of the files to be overlifted.
   */
  override def overLiftToT2T(outputPath: String, releaseNumber: String, downloadPath: String, filePath: String,
                             fileNames: List[String]): Unit = {
    fileNames.foreach( fileName =>
      LiftOverTool.liftOverGFF(s"$filePath/$fileName", outputPath, fileName)
    )
    val refPath = RefChainDirManager.getReferenceFileDir.getOrElse("")
    FileStuff.copyFile(s"$refPath/chm13v2.0.fa", s"$outputPath/chm13v2.0.fa")
    ServiceModules.addModuleToDatabase("gencode", releaseNumber, outputPath, downloadPath,  true, "t2t")
  }

  /**
   * Removes a Gencode module by its unique ID.
   *
   * @param id  The module ID.
   */
  override def removeModuleById(id: Int): Unit = {
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
   * Prints information about all installed Gencode modules.
   */
  override def printAllClassModules(): Unit = {
    val modules = ServiceModules.getModulesByName("gencode")
    if (modules.isEmpty){
      println("No gencode modules installed")
    }else{
      println("GENCODE MODULES -")
      modules.foreach(_.print())
    }
  }

  /**
   * Prints information about all installed modules.
   */
  override def printAllModules(): Unit = {
    val modules = ServiceModules.getModules 
    if (modules.isEmpty) {
      println("No modules installed")
    } else {
      println("ALL MODULES -")
      modules.foreach(_.print())
    }
  }
}
