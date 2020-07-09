# REVAvis
Visualization Software for REVAvis

## Install dependencies
```
install.packages(c("shiny", "waiter", "ggplot2", "dplyr", "tidyverse"))

if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")

BiocManager::install("BiocGenerics")
BiocManager::install("S4Vectors")
BiocManager::install("IRanges")
BiocManager::install("GenomicRanges")
BiocManager::install("biomaRt")
BiocManager::install("GenomicFeatures")
BiocManager::install("GenomeInfoDb")
BiocManager::install("biovizBase")
BiocManager::install("AnnotationFilter")
BiocManager::install("ensembldb")
BiocManager::install("rtracklayer")
BiocManager::install("VariantAnnotation")
BiocManager::install("Biostrings")
BiocManager::install("XVector")
BiocManager::install("Rsamtools")
BiocManager::install("BSgenome")
BiocManager::install("Gviz")
BiocManager::install("BSgenome.Hsapiens.UCSC.hg38")
```

## Install REVAvis
Clone REVAvis from GitHub:
get clone https://github.com/mjmccoy/REVAvis.git

Alternatively, download REVAvis binary:
https://github.com/mjmccoy/REVAvis/blob/master/REVAvis_0.0.0.9000_R_x86_64-pc-linux-gnu.tar.gz

Then install package from source:
```install.packages("REVAvis_0.0.0.9000_R_x86_64-pc-linux-gnu.tar.gz", repos = NULL, type="source")```
