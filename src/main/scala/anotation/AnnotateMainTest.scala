package anotation


object  AnnotateMainTest {
  def main(args: Array[String]): Unit = {Annotation.annotateInBatches("Lynch.vcf", "Lynch.maf", "hg38")}
}
