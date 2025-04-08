package module

import cosmic.FAtoGFFaLoadCOSMIC
import cosmic.{TSVtoGFFGeneCensus, TSVtoGFFNonCoding, TSVtoGFFResistance}
import database.modules.ServiceModules
import downloader.CosmicDownload
import logfiles.ConfigCredentials
import utils.{LiftOverTool, RepositoryManager, TarExtractor}

import scala.util.control.Breaks.*

object CosmicModule extends ModuleManager {

    private val defaultVersion = 100
    var latestVersionNum: Int = if ServiceModules.getNewestModuleVersion("cosmic") != "0" then ServiceModules.getNewestModuleVersion("cosmic").toInt else defaultVersion
    def latestVersion = s"v$latestVersionNum"
    def filePath: String = s"grch38/cosmic/${latestVersion}/"
    def tarNames: List[String] = List(
      s"Cosmic_CancerGeneCensus_Tsv_${latestVersion}_GRCh38.tar",
      s"Cosmic_CancerGeneCensusHallmarksOfCancer_Tsv_${latestVersion}_GRCh38.tar",
      s"Cosmic_NonCodingVariants_Tsv_${latestVersion}_GRCh38.tar",
      s"Cosmic_ResistanceMutations_Tsv_${latestVersion}_GRCh38.tar",
      s"Cosmic_Genes_Fasta_${latestVersion}_GRCh38.tar"
    )
    def fileNames: List[String] = List(
      s"Cosmic_CancerGeneCensus_${latestVersion}_GRCh38.tsv.gz",
      s"Cosmic_CancerGeneCensusHallmarksOfCancer_${latestVersion}_GRCh38.tsv.gz",
      s"Cosmic_NonCodingVariants_${latestVersion}_GRCh38.tsv.gz",
      s"Cosmic_ResistanceMutations_${latestVersion}_GRCh38.tsv.gz",
      s"Cosmic_Genes_${latestVersion}_GRCh38.fasta.gz"
    )

    def downloadModule(localPath: String, version: String): Unit = {
      val finalLocalPath = if localPath == "" then s"cosmic/$version/hg38" else s"$localPath/cosmic/$version/hg38"
      val versionInstalledCheck = ServiceModules.getModuleFromDatabase("cosmic", version, "hg38")
      if (versionInstalledCheck.isEmpty){
        val config = ConfigCredentials.loadConfig()
        config match {
          case Some((email, password)) =>
            val authString = CosmicDownload.generateAuthString(email, password)
            println(s"Generated auth string: $authString")
            tarNames.foreach{ file =>
              val path = s"${filePath}$file"
              println(path)
              CosmicDownload.getDownloadURL(authString, path)  match {
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
                  println("Failed to obtain the download URL. Probably wrong version or credentials.")
              }
            }
            val localOverLiftPath = if localPath == "" then s"cosmic/$version/t2t" else s"$localPath/cosmic/$version/t2t"
            ServiceModules.addModuleToDatabase("cosmic", version.substring(1), finalLocalPath, s"$filePath", false, "hg38")
            println("Converting to .tsv to .vcf")
            TSVtoGFFGeneCensus.convertTSVToGFF(s"$finalLocalPath/Cosmic_CancerGeneCensus_${version}_GRCh38.tsv.gz", s"$finalLocalPath/Cosmic_CancerGeneCensus_${version}_GRCh38.gff")
            TSVtoGFFNonCoding.convertTSVToGFF(s"$finalLocalPath/Cosmic_NonCodingVariants_${version}_GRCh38.tsv.gz", s"$finalLocalPath/Cosmic_NonCodingVariants_${version}_GRCh38.gff")
            TSVtoGFFResistance.convertTSVToGFF(s"$finalLocalPath/Cosmic_ResistanceMutations_${version}_GRCh38.tsv.gz", s"$finalLocalPath/Cosmic_ResistanceMutations_${version}_GRCh38.gff")
            FAtoGFFaLoadCOSMIC.loadFastaFromGzip(s"$finalLocalPath/Cosmic_Genes_${version}_GRCh38.fasta.gz")
            FAtoGFFaLoadCOSMIC.writeGFF(s"$finalLocalPath/Cosmic_Genes_${version}_GRCh38.gff", FAtoGFFaLoadCOSMIC.loadedList.getOrElse(List()))
            println("Liftovering to T2T.")
            overLiftToT2T(localOverLiftPath, version, filePath, finalLocalPath, List(""))
            println("Liftover DONE module downloaded.")
          case None =>
            println("Config not found. cred.config with email and password")

        }
      }else{

      }
    }
    def downloadModuleLatest(localPath: String): Unit = {
      latestVersionNum = if ServiceModules.getNewestModuleVersion("cosmic") != "0" then ServiceModules.getNewestModuleVersion("cosmic").toInt else defaultVersion
      if checkNewVersion() then downloadModule(localPath, latestVersion) else println("Already downloaded latest cosmic.")
    }

  /**
   * Overlifts Gencode annotation files from hg38 to the T2T reference version.
   *
   * @param outputPath    The local directory where the files should be saved.
   * @param releaseNumber The Gencode release number.
   * @param downloadPath  The URL path to the FTP server.
   * @param filePath      The path to the input file to be overlifted.
   * @param fileNames     The List of the files to be overlifted.
   */
    def overLiftToT2T(outputPath: String, releaseNumber: String, downloadPath: String, filePath: String,
                               fileNames: List[String]): Unit = {
      LiftOverTool.liftOverGFF(s"$filePath/Cosmic_CancerGeneCensus_${releaseNumber}_GRCh38.gff", outputPath, s"Cosmic_CancerGeneCensus_${releaseNumber}_Chm13.gff")
      LiftOverTool.liftOverGFF(s"$filePath/Cosmic_NonCodingVariants_${releaseNumber}_GRCh38.gff", outputPath, s"Cosmic_NonCodingVariants_${releaseNumber}_Chm13.gff")
      LiftOverTool.liftOverGFF(s"$filePath/Cosmic_ResistanceMutations_${releaseNumber}_GRCh38.gff", outputPath, s"Cosmic_ResistanceMutations_${releaseNumber}_Chm13.gff")
      LiftOverTool.liftOverGFF(s"$filePath/Cosmic_Genes_${releaseNumber}_GRCh38.gff", outputPath, s"Cosmic_Genes_${releaseNumber}_Chm13.gff")
      ServiceModules.addModuleToDatabase("cosmic", releaseNumber.substring(1), outputPath, downloadPath, true, "t2t")

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

    /**
     * Function to check if a newer version exists
     * @return true/false
     */
    def checkNewVersion(): Boolean = {
      latestVersionNum = if ServiceModules.getNewestModuleVersion("cosmic") != "0" then ServiceModules.getNewestModuleVersion("cosmic").toInt else defaultVersion
      latestVersionNum += 1
      var isNewerVersionFound = false
      val config = ConfigCredentials.loadConfig()
      config match {
        case Some((email, password)) =>
          val authString = CosmicDownload.generateAuthString(email, password)
          breakable {
            tarNames.foreach { file =>
              CosmicDownload.getDownloadURL(authString, s"$filePath$file") match {
                case Some(downloadLink) =>
                  isNewerVersionFound = true
                  break
                case None =>
                // Continue checking next
              }
            }
          }
        case None =>
      }
      if isNewerVersionFound then {
        println(s"Newer version of cosmic found - $latestVersion .")
        true
      } else {
        println("No newer version of cosmic found.")
        latestVersionNum -= 1
        false
      }
    }
}
