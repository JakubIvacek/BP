package module
import module.GenCodeModule
object Main {
  def main(args: Array[String]): Unit = {
    //GenCodeModule.downloadModuleLatest("C:\\Users\\ivace")
    //GenCodeModule.downloadModule("C:\\Users\\ivace", "50")
    //GenCodeModule.removeModule("gencode", "47", "hg38")
    //GenCodeModule.removeModuleById(13)
    GenCodeModule.printAllClassModules()
  }
}
