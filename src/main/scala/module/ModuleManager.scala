package module


/**
 * Abstract class `ModuleManager` serves as a blueprint for managing various modules,
 * including downloading, removing, and printing module details.
 * 
 */
abstract class ModuleManager {

    /**
     * Download a specific module release and save it to the given local path.
     *
     * @param localPath The local directory where the module files should be saved.
     * @param release   The release identifier for the module (e.g., version or number).
     */
    def downloadModule(localPath: String, release: String): Unit

    /**
     * Download the latest release of a module and save it to the given local path.
     *
     * @param localPath The local directory where the module files should be saved.
     */
    def downloadModuleLatest(localPath: String): Unit

    /**
     * Remove a module from the system based on its unique ID.
     *
     * @param ModuleId The unique identifier of the module to be removed.
     */
    def removeModuleById(ModuleId: Int): Unit

    /**
     * Print all installed modules of a specific class (e.g., Gencode).
     * This method filters and displays only the modules belonging to the specified class type.
     */
    def printAllClassModules(): Unit

    /**
     * Print all installed modules across all classes.
     * This method displays information about all installed modules in the system.
     */
    def printAllModules(): Unit

    /**
     * Overlift a module's annotation file to the T2T reference version.
     *
     * @param outputPath    The directory where the overlifted files should be saved.
     * @param releaseNumber The release identifier of the module (e.g., "34").
     * @param downloadPath  The FTP server path where the module files are located.
     * @param filePath      The local path to the original file that needs to be overlifted.
     * @param fileNames      The List of the files to be overlifted.
     */
    def overLiftToT2T(outputPath: String, releaseNumber: String, downloadPath: String, filePath: String, fileNames: List[String]): Unit
}

