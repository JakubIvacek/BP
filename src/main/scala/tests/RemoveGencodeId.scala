package tests
import module.GenCodeModule

object RemoveGencodeId {
  def main(args: Array[String]): Unit = {
    GenCodeModule.removeModuleById(args(0).toInt)
  }
}
