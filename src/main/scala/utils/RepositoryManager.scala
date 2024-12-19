package utils

import java.io.File

object RepositoryManager {
  /**
   * Delete repository and all its content
   *
   * @param path The local directory where the data is saved  
   */
  def deleteRepository(path: String): Boolean = {
    val directory = new File(path)

    // Check if the directory exists
    if (!directory.exists()) {
      println(s"Directory $path does not exist.")
      return false
    }
    // Call the recursive function and return whether the deletion was successful
    val success = deleteRecursively(directory)

    if (success) {
      println(s"Directory $path has been deleted successfully.")
    } else {
      println(s"Failed to delete directory $path.")
    }
    success
  }

  /**
   * Recursive function to delete whole directory
   *
   * @param file The path of file to delete recursively
   */
  private  def deleteRecursively(file: File): Boolean = {
    if (file.isDirectory) {
      // Delete all files and subdirectories within the directory
      file.listFiles().forall(deleteRecursively)
    }
    // Delete the current file or empty directory
    file.delete()
  }

  /**
   * Get the total size of a directory.
   *
   * @param dirPath The path of the directory for which the size is to be calculated.
   * @return The size of the directory in bytes.
   */
  def getDirectorySize(dirPath: String): Long = {
    val dir = new File(dirPath)

    if (!dir.exists()) {
      throw new IllegalArgumentException(s"Directory $dirPath does not exist.")
    }

    if (!dir.isDirectory) {
      throw new IllegalArgumentException(s"$dirPath is not a directory.")
    }

    calculateSize(dir)
  }

  /**
   * Recursive function to calculate size of directory
   *
   * @param file The path of file to check recursively
   * @return The size of the directory in bytes.
   */
  def calculateSize(file: File): Long = {
    if (file.isFile) {
      file.length()
    } else {
      val children = file.listFiles()
      if (children != null) {
        children.map(calculateSize).sum
      } else {
        0L
      }
    }
  }

}
