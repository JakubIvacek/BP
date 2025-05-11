package anotation

import anotation.Annotation1000Genomes.getVcfFile
import anotation.AnnotationGencode.{annotateGencodeSetup, faPathSaved}

import scala.concurrent.*
import scala.concurrent.duration.*
import ExecutionContext.Implicits.global
import data.DnaVariant
import database.annotationruns.ServiceAnnotationRuns
import database.modules.ServiceModules
import files.{FastaLoadCOSMIC, FastaReaderSW, FileReaderVcf, GFFReader, GFFReaderSW, VcfReaderSW, WriteToMaf}
import logfiles.{PathSaver, RefChainDirManager}
import utils.LiftOverTool.T2TRefName

import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration.Duration
import scala.util.{Failure, Success}
import java.io.{File, FileNotFoundException}
import utils.{LiftOverTool, VcfCleaner}


/**
 * The `Annotation` object is responsible for annotating DNA variants with genomic information from VCF files
 * data sources and outputting the results in the MAF format.
 */
object Annotation {
  var currentContig1000Genomes: String = ""

  /**
   * Main method to process the input VCF file, annotate the variants, and write the result to an output MAF file.
   * Also creates log file where annotation process + entries and relevant information will be added
   * @param inputFile       The path to the input VCF file containing DNA variants.
   * @param outputPath      The path to the output dir where MAF file will be and annotation.log
   * @param referenceGenome The reference genome used for annotation (e.g., "hg38").
   * @param batchSize       Size of vcf batch to annotate at once
   */
  def annotateInBatches(
                         inputFile: String,
                         outputPath: String,
                         referenceGenome: String,
                         batchSize: Int = 1000): Unit = {


    //check if input vcf exits
    val inFile = new File(inputFile)
    if (!inFile.exists() || !inFile.isFile) {
      println(s"Input VCF not found or not a file path: $inputFile")
      return
    }
    // overlift to T2T if hg38
    val (newPath, newGenome) = (inputFile, referenceGenome) // NOT AUTO OVERLIFT TO T2T
    //val (newPath, newGenome) = if referenceGenome == "hg38" then (overliftToT2T(inputFile),"t2t") else (inputFile,"t2t") // AUTO OVERLIFT TO T2T
    val fileOutputPath = s"$outputPath/${inputFile.split("/").last.dropRight(4)}.maf"
    val logFilePath = s"$outputPath/annotation.log"
    val outFile = new File(fileOutputPath)
    outFile.getParentFile.mkdirs()
    FileReaderVcf.open(newPath)

    val logWriter = new java.io.PrintWriter(new java.io.FileOutputStream(logFilePath, true))
    var timestamp = java.time.LocalDateTime.now()
    logWriter.println(s"-------ANNOTATION START INFORMATION--------")
    logWriter.println(s"Annotation started [$timestamp]")
    logWriter.println(s"Input VCF file      : $newPath")
    logWriter.println(s"Output MAF file     : $fileOutputPath")
    logWriter.println(s"Reference genome    : $newGenome")
    logWriter.println(s"Batch size          : $batchSize")
    val (gencodeInfo, cosmicInfo, genomes1000Info, uniprotInfo) = addModuleInformations(newGenome, logWriter)
    //ADD ANNOTATIONS FREEZE TO DATABASE
    ServiceAnnotationRuns.addAnnotationRun(newPath, fileOutputPath, gencodeInfo, uniprotInfo, cosmicInfo, genomes1000Info, newGenome)
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
        val variants = dnaVariants.toList
        variants.foreach{
          variant => variant.positionEnd = VariantTypeAnnotation.calculateEndPosition(variant)
        }
        annotateVariantsThreads(variants, newGenome)
        //annotateVariants(variants, newGenome)
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
   * Annotate a list of DNA variants with threads
   *
   * @param dnaVariants     A list of DNA variants to annotate.
   * @param referenceGenome The reference genome to use for annotation (e.g., "hg38").
   */
  def annotateVariantsThreads(dnaVariants: List[DnaVariant], referenceGenome: String): Unit = {
    // 1) Prvá: GENCODE, batchy po 3 vars, 3 paralelné futures v každom kroku
    val f1: Future[Unit] = Future {
      for (batch <- dnaVariants.grouped(3)) {
        val faPath = faPathSaved.getOrElse {
          faPathSaved = annotateGencodeSetup(referenceGenome)
          faPathSaved.getOrElse("")
        }
        if (faPath.nonEmpty) {
          // Ensure the genome build is loaded
          FastaReaderSW.loadFastaFile(faPath)
          val v1 = batch.head // "variant1"
          val vLast = batch.last // posledný variant v batchi

          //vLast.positionEnd = VariantTypeAnnotation.calculateEndPosition(vLast)
          GFFReaderSW.ensureVariantInWindow(
            vLast.positionEnd.toInt,
            v1.position.toInt,
            vLast.contig
          )
          FastaReaderSW.loadNextBatch(vLast.contig, v1.position.toInt, vLast.positionEnd.toInt)


          // ------------------------------------------------------
          val tasks: Seq[Future[Unit]] = batch.map { v =>
            Future {
              AnnotationGencode.annotateVariantGencode(v, referenceGenome)
            }
          }

          Await.result(Future.sequence(tasks), Duration.Inf)
        }
      }
    }


    val f2: Future[Unit] = Future {
      dnaVariants.foreach{
        variant => Annotation1000Genomes.annotateVariant1000Genomes(variant, referenceGenome)
      }
      
    }


    val f3: Future[Unit] = Future {
      // Reload annotation data if none loaded or new reference Genome
      if FastaLoadCOSMIC.getLoadedList.isEmpty || FastaLoadCOSMIC.loadedGenome != referenceGenome then {

        val path = ServiceModules.getNewestModulePath("cosmic", referenceGenome)
        val reference = if referenceGenome == "hg38" then Some("GRCh38") else Some("Chm13")
        val version = Some(ServiceModules.getNewestModuleVersion("cosmic"))
        val fPath = path.getOrElse("")
        val fReference = reference.getOrElse("")
        val fVersion = version.getOrElse("")
        //val fFormat = if referenceGenome == "hg38" then "fasta.gz" else "fasta"
        FastaLoadCOSMIC.loadFastaFromGzip(s"$fPath/Cosmic_Genes_v${fVersion}_$fReference.fasta.gz", referenceGenome)
        AnnotationCosmic.geneCensusEntries = Some(GFFReader.loadGffFileReturnList(s"$fPath/Cosmic_CancerGeneCensus_v${fVersion}_$fReference.gff"))
        AnnotationCosmic.resistanceMutationsEntries = Some(GFFReader.loadGffFileReturnList(s"$fPath/Cosmic_ResistanceMutations_v${fVersion}_$fReference.gff"))
      }
      for (batch <- dnaVariants.grouped(3)) {
        val tasks = batch.map { v =>
          Future {
            AnnotationCosmic.annotateVariantCosmic(v, referenceGenome)
          }
        }
        Await.result(Future.sequence(tasks), Duration.Inf)
      }
    }
    f1.onComplete {
      case Success(_) => println("🟢 f1 (GENCODE) dokončené")
      case Failure(error) => println(s"❌ f1 zlyhalo: ${error.getMessage}")
    }
    f2.onComplete {
      case Success(_) => println("🟢 f2 (1000Genomes) dokončené")
      case Failure(error) => println(s"❌ f2 zlyhalo: ${error.getMessage}")
    }
    f3.onComplete {
      case Success(_) => println("🟢 f3 (Cosmic) dokončené")
      case Failure(error) => println(s"❌ f3 zlyhalo: ${error.getMessage}")
    }
    Await.result(Future.sequence(Seq(f1, f2, f3)), Duration.Inf)
  }

  /**
   * Annotate a list of DNA variants without threads
   *
   * @param dnaVariants     A list of DNA variants to annotate.
   * @param referenceGenome The reference genome to use for annotation (e.g., "hg38").
   */
  def annotateVariants(dnaVariants: List[DnaVariant], referenceGenome: String): Unit = {
    dnaVariants.foreach {
      variant => AnnotationGencode.annotateVariantGencode(variant, referenceGenome)
    }
    dnaVariants.foreach {
      variant => AnnotationCosmic.annotateVariantCosmic(variant, referenceGenome)
    }
    dnaVariants.foreach {
      variant => Annotation1000Genomes.annotateVariant1000Genomes(variant, referenceGenome)
    }
  }
  /**
   * Adds used scientific databases relevant information used to annotate into .log file
   *
   * @param referenceGenome The reference genome to use for annotation (e.g., "hg38").
   * @param logWriter       Logwriter to .log file
   * @return                (gencodeInfo, cosmicInfo, genomes1000Info, uniprotInfo)
   */
  private def addModuleInformations(referenceGenome: String, logWriter: java.io.PrintWriter): (String, String, String, String) = {
    logWriter.println(s"--------USED DATABASES INFORMATIONS-----------")
    val gencode = ServiceModules.getNewestModule("gencode", referenceGenome)
    val gencodeInfo = gencode.map(m => s"gencode ${m.version}").getOrElse("")
    if gencode.nonEmpty then logWriter.println(s"Gencode module version: v${gencode.get.version}, downloaded: ${gencode.get.created.get}, path: ${gencode.get.locationPath.get}")

    val cosmic = ServiceModules.getNewestModule("cosmic", referenceGenome)
    val cosmicInfo = cosmic.map(m => s"cosmic ${m.version}").getOrElse("")
    if cosmic.nonEmpty then logWriter.println(s"Cosmic module version: ${cosmic.get.version}, downloaded: ${cosmic.get.created.get}, path:  ${cosmic.get.locationPath.get}")

    val genomes1000 = ServiceModules.getNewestModule("1000genomes", referenceGenome)
    val genomes1000Info = genomes1000.map(m => s"1000genomes ${m.version}").getOrElse("")
    if genomes1000.nonEmpty then logWriter.println(s"1000genomes module version: ${genomes1000.get.version}, downloaded: ${genomes1000.get.created.get}, path: ${genomes1000.get.locationPath.get}")

    val uniprot = ServiceModules.getNewestModule("uniprot", "")
    val uniprotInfo = uniprot.map(m => s"uniprot ${m.version}").getOrElse("")
    if uniprot.nonEmpty then logWriter.println(s"Uniprot module version: ${uniprot.get.version}, downloaded: ${uniprot.get.created.get}, path: ${uniprot.get.locationPath.get}")
    logWriter.println(s"------------ANNOTATION START---------------")
    logWriter.flush()
    (gencodeInfo, cosmicInfo, genomes1000Info, uniprotInfo)
  }

  private def overliftToT2T(filePath: String): String = {
    val outPath = PathSaver.getPath.getOrElse("") + "/overlift/"
    val outName = filePath.split("/").last
    LiftOverTool.liftOverVcf(filePath, outPath, outName)
    val wholePath = s"$outPath/$outName"
    val resultPath = s"$outPath/T2Tnonempty_$outName"
    VcfCleaner.filterEmptyAlleles(wholePath, resultPath)
    //VcfCleaner.dropMultiAllelic(resultPath, s"${outPath}T2T_$outName")
    s"${outPath}T2T_$outName"
  }
}
