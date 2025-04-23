package utils

import java.io.File
import scala.language.postfixOps
import scala.sys.process.*

object VcfCleaner {

  /**
   * Filters a VCF to remove any record with empty or “.” alleles.
   *
   * @param inputVcf  Path to the input VCF.
   * @param outputVcf Path where the cleaned VCF will be written.
   * @throws RuntimeException if the awk command exits with non‑zero.
   */
  def filterEmptyAlleles(inputVcf: String, outputVcf: String): Unit = {
    // awk script:
    //  - keep any header line (^#)
    //  - keep data lines only if $4 (REF) and $5 (ALT) are non-empty and not "."
    val awkScript =
      """/^#/ { print; next }
        |($4 != "" && $4 != "." && $5 != "" && $5 != ".") { print }
      """.stripMargin

    // Build the Process. Redirect stdout to the output file.
    val cmd = Seq("awk", "-F", "\t", awkScript, inputVcf)
    val exitCode = Process(cmd) #> new File(outputVcf) !

    if (exitCode != 0) {
      throw new RuntimeException(s"VCF cleaning failed (exit code $exitCode)")
    }
  }
  
}

