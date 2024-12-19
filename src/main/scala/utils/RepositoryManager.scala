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
}
