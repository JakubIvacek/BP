package module

abstract class ModuleManager {
    def downloadModule(localPath: String, release: String): Unit
    def downloadModuleLatest(localPath: String): Unit
    def removeModule(ModuleName: String, release: String, versionReference: String): Unit
    def removeModuleById(ModuleId: Int): Unit
    def printAllClassModules(): Unit
    def printAllModules(): Unit
    def overLiftToT2T(outputPath: String, releaseNumber: String, downloadPath: String, filePath: String): Unit
    //def checkNewerVersion(): Unit
}
