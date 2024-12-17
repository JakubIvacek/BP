package module

abstract class ModuleManager {
    def downloadData(): Unit
    def removeData(): Unit
    def checkNewVersion(): Unit
    def moduleSize(): Unit
    def printInformation(): Unit
}
