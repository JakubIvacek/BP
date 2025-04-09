package cosmic

import java.io.PrintWriter
import scala.util.Try
/**
 * Object Utils for cosmic
 */
object Utils {
  /**
   * Writes GFF Header to file
   * @param writer PrintWriter for the file where header is printed
   */
  def writeGFFHeader(writer: PrintWriter): Unit = {
    writer.println("##gff-version 3")
  }

  /**
   * Helper method to parse string into Long
   *
   * @param str String to be parsed
   */
  def safeParseLong(str: String): Option[Long] = {
    Try(str.toLong).toOption
  }
}
