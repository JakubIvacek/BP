import java.io.File

class Module(val name: String, var location: String, val installPath: String) {
  
  if (!new File(location).exists()) {
    println(s"Warning: Location directory '$location' does not exist.")
  }
  if (!new File(installPath).exists()) {
    println(s"Warning: Install path directory '$installPath' does not exist.")
  }
  installModuleData()
  
  def displayDetails(): Unit = {
    println(s"Module Name: $name")
    println(s"Location: $location")
    println(s"Install Path: $installPath")
  }

  def installModuleData(): Unit = {

  }
  def checkNewVersion(): Unit = {

  }
  def removeModuleData(): Unit = {
    val directory = new File(location)
    if (directory.exists() && directory.isDirectory) {
      if (deleteDirectory(directory)) {
        println(s"All data in $location has been removed.")
      } else {
        println(s"Failed to remove some files in $location.")
      }
    } else {
      println(s"$location is not a valid directory or does not exist.")
    }
  }

  def updateInstallPath(newInstallPath: String): Unit = {
    installPath = newInstallPath
    println(s"Install path updated to: $installPath")
  }

  def moduleSize(): Long = {
    val directory = new File(location)
    if (directory.exists() && directory.isDirectory) {
      calculateSize(directory)
    } else {
      throw new IllegalArgumentException(s"$location is not a valid directory.")
    }
  }

  private def calculateSize(file: File): Long = {
    if (file.isFile) {
      file.length()
    } else {
      val files = file.listFiles()
      if (files != null) files.map(calculateSize).sum else 0L
    }
  }

  private def deleteDirectory(directory: File): Boolean = {
    val files = directory.listFiles()
    if (files != null) {
      files.foreach { file =>
        if (!file.delete()) {
          println(s"Failed to delete file: ${file.getAbsolutePath}")
          return false
        }
      }
    }
    directory.delete()
  }
}
