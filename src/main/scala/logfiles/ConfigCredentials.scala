package logfiles

import java.io.{FileInputStream, FileOutputStream, IOException}
import java.util.Properties

object ConfigCredentials {

  // Path to the configuration file
  val configFilePath = "cred.config"

  def saveConfig(email: String, password: String): Unit = {
    val properties = new Properties()

    properties.setProperty("email", email)
    properties.setProperty("password", password)

    try {
      val outputStream = new FileOutputStream(configFilePath)
      properties.store(outputStream, "COSMIC Configurations")
      outputStream.close()
    } catch {
      case e: IOException => println(s"Error saving config: ${e.getMessage}")
    }
  }

  // Method to retrieve email and password from the config file
  def loadConfig(): Option[(String, String)] = {
    val properties = new Properties()

    try {
      val inputStream = new FileInputStream(configFilePath)
      properties.load(inputStream)
      inputStream.close()

      val email = properties.getProperty("email")
      val password = properties.getProperty("password")

      if (email != null && password != null) {
        Some(email, password)
      } else {
        println("Email or password not found in the config.")
        None
      }
    } catch {
      case e: IOException =>
        println(s"Error loading config: ${e.getMessage}")
        None
    }
  }
}
