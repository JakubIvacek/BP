
import database.DatabaseConnection
//import utils.LiftOverVcf

object  AnnotateMainTest {
  def main(args: Array[String]): Unit = {
    Annotation.annotate("Small.vcf", "Annotated.maf", "hg38")
  }
}
