package anotation


object  AnnotateMainTest {
  def main(args: Array[String]): Unit = {Annotation.annotateInBatches("Small.vcf", "Small.maf", "hg38")}
}
