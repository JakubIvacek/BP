package utils

import logfiles.PathSaver
import module.{CosmicModule, GenCodeModule, Genomes1000Module, UniprotModule}
/**
 * The NewVersionChecker object is responsible for checking if there is new version
 * of modules available if so downloading them
 */
object ModuleNewerVersionChecker {
  def checkNewVersions(): Unit = {
    // Check GenCode module
    if GenCodeModule.checkNewVersion() then {
      GenCodeModule.downloadModuleLatest(PathSaver.getPath.getOrElse(""))
    }
    // Check 1000 Genomes module
    if Genomes1000Module.checkNewVersion() then {
      Genomes1000Module.downloadModuleLatest(PathSaver.getPath.getOrElse(""))
    }
    // Check Uniprot module
    if UniprotModule.checkNewVersion() then {
      UniprotModule.downloadModuleLatest(PathSaver.getPath.getOrElse(""))
    }
    // Check Cosmic module
    if CosmicModule.checkNewVersion() then {
      CosmicModule.downloadModuleLatest(PathSaver.getPath.getOrElse(""))
    }
  }
}
