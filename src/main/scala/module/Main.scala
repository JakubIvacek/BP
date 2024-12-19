package module
import module.GenCodeModule
object Main {
  def main(args: Array[String]): Unit = {
    //GenCodeModule.downloadModuleLatest("C:\\Users\\ivace")
    //GenCodeModule.downloadModule("C:\\Users\\ivace", "30")
    //GenCodeModule.removeModule("gencode", "release_47", "hg38")
    //GenCodeModule.removeModuleById(10)
    GenCodeModule.printAllClassModules()
  }
}
