package pdb

import anotation.pdbID
import pdb.UniProtSequenceLoader
import data.UniProtEntry
import scala.io.StdIn

object Main {
  def main(args: Array[String]): Unit = {
    val uniprotDatFile = "uniprot/uniprot_sprot.dat.gz"
    val pdbMappingFile = "uniprot_pdb_mappings.txt"
    val isoformsFastaFile = "uniprot_isoforms.fasta"

    // 1️⃣ Extract PDB mappings
    //println("Extracting PDB mappings from UniProt data...")
    //ExtractPDB.extractPdbMappings(uniprotDatFile, pdbMappingFile)

    
  }
}

