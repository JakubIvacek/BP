package module
import ftp.FtpClientGencode
import module.GenCodeModule
import database.modules.ServiceModules
object Main {
  def main(args: Array[String]): Unit = {
    //GenCodeModule.downloadModuleLatest("C:\\Users\\ivace")
    //GenCodeModule.downloadModule("C:\\Users\\ivace", "50")
    //GenCodeModule.removeModule("gencode", "47", "hg38")
    //GenCodeModule.removeModuleById(17)
    //LiftOverVcf.liftOverVcf()
    //GenCodeModule.printAllClassModules()
    ServiceModules.getNewestModulePath("hg38")
    //println(FtpClientGencode.findLatestVersionGencode())
  }
}
