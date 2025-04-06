package module

import database.modules.ServiceModules
import downloader.CosmicDownload
import logfiles.ConfigCredentials
import utils.{Gunzip, RepositoryManager, TarExtractor}

import java.io.File
import scala.sys.process.*
import scala.util.{Failure, Success, Try}

object CosmicModule {
    var latestVersion = "v101"
    val filePath: String = s"grch38/cosmic/${latestVersion}/"
    val tarNames: List[String] = List(
      s"Cosmic_CancerGeneCensus_Tsv_${latestVersion}_GRCh38.tar",
      s"Cosmic_CancerGeneCensusHallmarksOfCancer_Tsv_${latestVersion}_GRCh38.tar",
      s"Cosmic_NonCodingVariants_Tsv_${latestVersion}_GRCh38.tar",
      s"Cosmic_ResistanceMutations_Tsv_${latestVersion}_GRCh38.tar"
    )
    val fileNames: List[String] = List(
      s"Cosmic_CancerGeneCensus_${latestVersion}_GRCh38.tsv.gz",
      s"Cosmic_CancerGeneCensusHallmarksOfCancer_${latestVersion}_GRCh38.tsv.gz",
      s"Cosmic_NonCodingVariants_${latestVersion}_GRCh38.tsv.gz",
      s"Cosmic_ResistanceMutations_${latestVersion}_GRCh38.tsv.gz"
    )
    def downloadModule(localPath: String, version: String = latestVersion): Unit = {
      val finalLocalPath = if localPath == "" then s"cosmic/$version/hg38" else s"$localPath/cosmic/$version/hg38"
      val versionInstalledCheck = ServiceModules.getModuleFromDatabase("cosmic", version, "hg38")
      if (versionInstalledCheck.isEmpty){
        val config = ConfigCredentials.loadConfig()
        config match {
          case Some((email, password)) =>
            val authString = CosmicDownload.generateAuthString(email, password)
            println(s"Generated auth string: $authString")
            tarNames.foreach{ file =>
              CosmicDownload.getDownloadURL(authString, s"$filePath$file")  match {
                case Some(downloadLink) =>
                  val outputFileName = s"$finalLocalPath/$file"
                  val success = CosmicDownload.downloadFile(downloadLink, outputFileName)
                  if (success) {
                    println(s"File downloaded successfully: $file")
                    TarExtractor.unzipTar(outputFileName, finalLocalPath)
                  } else {
                    println("Failed to download the file.")
                  }
                case None =>
                  println("Failed to obtain the download URL.")
              }
            }
            ServiceModules.addModuleToDatabase("cosmic", version, finalLocalPath, s"$filePath", false, "hg38")
          //OVERLIFT ESTE HERE
          case None =>
            println("Config not found. cred.config with email and password")

        }
      }
    }

    def downloadModuleLatest(localPath: String): Unit = {
        downloadModule(localPath, latestVersion)
    }
    /**
     * Removes a COSMIC module by its unique ID.
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
     * Prints information about all installed Cosmic modules.
     */
    def printAllClassModules(): Unit = {
      val modules = ServiceModules.getModulesByName("cosmic")
      if (modules.isEmpty) {
        println("No Cosmic modules installed")
      } else {
        println("Cosmic MODULES -")
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
}
