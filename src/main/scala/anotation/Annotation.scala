package anotation

import data.DnaVariant
import database.modules.ServiceModules
import files.{FileReaderVcf, GFFReaderSW, WriteToMaf}
import logfiles.PathSaver
import java.io.File
import utils.{LiftOverTool, VcfCleaner}


/**
 * The `Annotation` object is responsible for annotating DNA variants with genomic information from VCF files
 * data sources and outputting the results in the MAF format.
 */
object Annotation {


  /**
   * Main method to process the input VCF file, annotate the variants, and write the result to an output MAF file.
   * Also creates log file where annotation process + entries and relevant information will be added
   * @param inputFile       The path to the input VCF file containing DNA variants.
   * @param outputPath      The path to the output MAF file where the annotated variants will be written.
   * @param referenceGenome The reference genome used for annotation (e.g., "hg38").
   * @param batchSize       Size of vcf batch to annotate at once
   */
  def annotateInBatches(
                         inputFile: String,
                         outputPath: String,
                         referenceGenome: String,
                         batchSize: Int = 1000): Unit = {

    // overlift to T2T if hg38
    val (newPath, newGenome) = (inputFile, referenceGenome)//if referenceGenome == "hg38" then (overliftToT2T(inputFile),"t2t") else (inputFile,"t2t")

    val fileOutputPath = s"$outputPath/${inputFile.split("/").last.dropRight(4)}.maf"
    val logFilePath = s"$outputPath/annotation.log"
    val outFile = new File(fileOutputPath)
    outFile.getParentFile.mkdirs()

    val logWriter = new java.io.PrintWriter(new java.io.FileOutputStream(logFilePath, true))
    var timestamp = java.time.LocalDateTime.now()
    logWriter.println(s"-------ANNOTATION START INFORMATION--------")
    logWriter.println(s"Annotation started [$timestamp]")
    logWriter.println(s"Input VCF file      : $newPath")
    logWriter.println(s"Output MAF file     : $fileOutputPath")
    logWriter.println(s"Reference genome    : $newGenome")
    logWriter.println(s"Batch size          : $batchSize")
    addModuleInformations(newGenome, logWriter)

    FileReaderVcf.open(newPath)
    var batchCount = 1
    var hasMoreVariants = true

    while (hasMoreVariants) {
      val dnaVariants = FileReaderVcf.readBatch(batchSize)

      if (dnaVariants.isEmpty) {
        hasMoreVariants = false
      } else {
        println(s"Processing batch $batchCount... ${dnaVariants.toList.head.contig}")
        timestamp = java.time.LocalDateTime.now()
        logWriter.println(s"Processing batch $batchCount with ${dnaVariants.size} variants [$timestamp]")
        annotateVariants(dnaVariants.toList, newGenome)
        WriteToMaf.writeMafFile(dnaVariants, fileOutputPath, append = batchCount > 1)
        batchCount += 1
      }
    }

    FileReaderVcf.close()
    GFFReaderSW.close()
    timestamp = java.time.LocalDateTime.now()
    logWriter.println(s"------------ANNOTATION COMPLETED---------------")
    logWriter.println(s"[$timestamp] completion time. Total batches: ${batchCount - 1}\n\n")
    logWriter.println()
    logWriter.close()
  }
  /**
   * Annotate a list of DNA variants
   *
   * @param dnaVariants     A list of DNA variants to annotate.
   * @param referenceGenome The reference genome to use for annotation (e.g., "hg38").
   */
  def annotateVariants(dnaVariants: List[DnaVariant], referenceGenome: String): Unit = {
    dnaVariants.foreach(variant =>
      AnnotationGencode.annotateVariantGencode(variant, referenceGenome) //GENCODE
    )
    dnaVariants.foreach(
      variant => Annotation1000Genomes.annotateVariant1000Genomes(variant, referenceGenome)   //1000GENOMES))
    )
    
    //dnaVariants.foreach(
      //variant => AnnotationCosmic.annotateVariantCosmic(variant, referenceGenome) // COSMIC
    //)
  }

  /**
   * Adds used scientific databases relevant information used to annotate into .log file
   *
   * @param referenceGenome The reference genome to use for annotation (e.g., "hg38").
   * @param logWriter       Logwriter to .log file
   */
  private def addModuleInformations(referenceGenome: String, logWriter: java.io.PrintWriter): Unit = {
    logWriter.println(s"--------USED DATABASES INFORMATIONS-----------")
    val gencode = ServiceModules.getNewestModule("gencode", referenceGenome)
    if gencode.nonEmpty then logWriter.println(s"Gencode module version: v${gencode.get.version}, downloaded: ${gencode.get.created.get}, path: ${gencode.get.locationPath}")

    val cosmic = ServiceModules.getNewestModule("cosmic", referenceGenome)
    if cosmic.nonEmpty then logWriter.println(s"Cosmic module version: ${cosmic.get.version}, downloaded: ${cosmic.get.created.get}, path:  ${cosmic.get.locationPath}")

    val genomes1000 = ServiceModules.getNewestModule("1000genomes", referenceGenome)
    if genomes1000.nonEmpty then logWriter.println(s"1000genomes module version: ${genomes1000.get.version}, downloaded: ${genomes1000.get.created.get}, path: ${genomes1000.get.locationPath}")

    val uniprot = ServiceModules.getNewestModule("uniprot", "")
    if uniprot.nonEmpty then logWriter.println(s"Uniprot module version: ${uniprot.get.version}, downloaded: ${uniprot.get.created.get}, path: ${uniprot.get.locationPath}")
    logWriter.println(s"------------ANNOTATION START---------------")
    logWriter.flush()
  }

  private def overliftToT2T(filePath: String): String = {
    val outPath = PathSaver.getPath.getOrElse("") + "/overlift/"
    val outName = filePath.split("/").last
    LiftOverTool.liftOverVcf(filePath, outPath, outName)
    val wholePath = s"$outPath/$outName"
    val resultPath = s"$outPath/T2T_$outName"
    VcfCleaner.filterEmptyAlleles(wholePath, resultPath)
    resultPath
  }
}
