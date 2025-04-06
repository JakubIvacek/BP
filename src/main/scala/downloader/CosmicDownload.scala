package downloader


import java.net.{URI, HttpURLConnection}
import java.io.{BufferedReader, InputStreamReader, FileOutputStream}
import scala.util.{Try, Success, Failure}
import play.api.libs.json._

object CosmicDownload {

  /** STEP 1
   * Generate an authentication string
   * Combine the email address and password and then encode them in Base64.
   */
  def generateAuthString(email: String, password: String): String = {
    val credentials = s"$email:$password"
    java.util.Base64.getEncoder.encodeToString(credentials.getBytes)
  }

  /** STEP 2
   * Obtain a download URL
   * Make a request to the following URL, passing the authentication string from Step 1:
   * E.g.: curl -H "Authorization: Basic AuthTokenGenerated"
   * "https://cancer.sanger.ac.uk/api/mono/products/v1/downloads/scripted?path=$filePath&bucket=downloads"
   */
  def getDownloadURL(authString: String, filePath: String): Option[String] = {
    val uri = new URI(s"https://cancer.sanger.ac.uk/api/mono/products/v1/downloads/scripted?path=$filePath&bucket=downloads")
    val url = uri.toURL

    val connection = url.openConnection().asInstanceOf[HttpURLConnection]
    connection.setRequestMethod("GET")
    connection.setRequestProperty("Authorization", s"Basic $authString")

    Try {
      val reader = new BufferedReader(new InputStreamReader(connection.getInputStream))
      val response = reader.readLine()
      reader.close()


      val jsonResponse = Json.parse(response)
      (jsonResponse \ "url").asOpt[String]
    } match {
      case Success(urlOption) => urlOption
      case Failure(_) => None
    }
  }


  /** STEP 3
   * Download the data file
   * curl "Insert download link from step 2 between the quotes" --output Name of the file on device
   */
  def downloadFile(downloadLink: String, outputFileName: String): Boolean = {
    val uri = new URI(downloadLink)
    val url = uri.toURL

    val connection = url.openConnection().asInstanceOf[HttpURLConnection]
    connection.setRequestMethod("GET")

    Try {
      val inputStream = connection.getInputStream
      val fileOutputStream = new FileOutputStream(outputFileName)
      val buffer = new Array[Byte](4096) // 8192 buffer size
      var bytesRead = 0

      while ( { bytesRead = inputStream.read(buffer); bytesRead != -1 }) {
        fileOutputStream.write(buffer, 0, bytesRead)
      }

      fileOutputStream.close()
      inputStream.close()
      true
    } match {
      case Success(_) => true
      case Failure(_) => false
    }
  }

  def main(args: Array[String]): Unit = {
    val email = "xivacek@stuba.sk" // Replace with your email
    val password = "pyN.def.9.ire" // Replace with your password
    val filePath = "grch38/cosmic/v101/Cosmic_CancerGeneCensus_Tsv_v101_GRCh38.tar" // Example file path
    val outputFileName = "Cosmic_CancerGeneCensus_Tsv_v101_GRCh38.tar" // File will be saved here

    // Step 1: Generate authentication string
    val authString = generateAuthString(email, password)
    println(s"Generated auth string: $authString")

    // Step 2: Get the download URL
    getDownloadURL(authString, filePath) match {
      case Some(downloadLink) =>
        println(s"Download link obtained: $downloadLink")

        // Step 3: Download the file
        val success = downloadFile(downloadLink, outputFileName)
        if (success) {
          println(s"File downloaded successfully: $outputFileName")
        } else {
          println("Failed to download the file.")
        }

      case None =>
        println("Failed to obtain the download URL.")
    }
  }
}