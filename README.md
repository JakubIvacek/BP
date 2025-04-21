# DNA Variant Annotation Tool

This tool provides annotation of structural DNA variants using standardized formats (VCF/MAF), with support for multiple scientific databases. It is designed for use on Linux systems, written in Scala, and optimized for command-line workflows.

## ✨ Features

- Supports the latest human reference genome **Chm13 (T2T)**, as well as **GRCh38**
- Coordinate lifting (overlift) between genome versions using **CrossMap** automatic download 
- Standard input/output formats: **VCF (input)** → **MAF (output)**
- Built-in support for scientific databases:
  - Gencode
  - 1000 Genomes
  - COSMIC
  - UniProt
- Modular system with SQLite storage
- Command-line interface with Scallop

## ⚙️ Requirements

- Linux OS
- Java (JDK 11 or later)
- [sbt (Scala Build Tool)](https://www.scala-sbt.org/)

## 🚀 Running the Tool

You can run the tool using `sbt run` with various commands:

### 🆘 Help

sbt run -h

### 📥 Download a scientific module

sbt run -d gencode -v v42

(If no version is specified, the latest will be downloaded.)

### 🗑️ Remove a module by ID

sbt run -r 6

### 📄 List downloaded modules

sbt run -i

### 📦 Set paths for chain and reference files (used for overlift)

sbt run -c /path/to/chains -a /path/to/reference

### 🔍 Annotate a VCF file (requires modules and reference set)

sbt run -f sample.vcf -a t2t -o output.maf

## 🧪 Supported Scientific Databases

Each database is downloaded via CLI and stored in SQLite for efficient access.

- **Gencode** – Transcript annotation and HGVS mapping
- **1000 Genomes** – Variant population frequencies
- **COSMIC** – Somatic mutations in cancer
- **UniProt** – Protein isoforms and functional annotations


