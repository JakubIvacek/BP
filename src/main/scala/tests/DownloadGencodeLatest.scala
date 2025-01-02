package tests
import module.GenCodeModule

object DownloadGencodeLatest {
  def main(args: Array[String]): Unit = {
    GenCodeModule.downloadModuleLatest(args(0))
  }
}
