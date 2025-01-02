package tests
import module.GenCodeModule

object PrintGencodeModules {
  def main(args: Array[String]): Unit = {
    GenCodeModule.printAllClassModules()
  }
}
