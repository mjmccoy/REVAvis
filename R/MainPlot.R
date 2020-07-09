# Function MainPlot.

MainPlot <- function(data, data2 = NULL, gen = "hg38", chr = NULL, start, end, log_scale = NULL){
  if(is.null(data)){
    return(NULL)
  }
  chr <- as.character(chr)
  data.df <- subset(data, Chr %in% chr) %>%
    group_by(Condition, Chr) %>%
    filter(duplicated(Chr) | n()==1) # removes the first instance of duplicated values (summary of each chr)
  data.df <- data.df %>%
    group_by(Condition) %>%
    mutate(Relative_Representation_Sense_ThisBin = (UniqueKCountS/UniqueKNumber)/(sum(UniqueKCountS)/sum(UniqueKNumber)))
  data.df <- data.df %>%
    dplyr::select(BinStart = BinStart, BinLength = BinLength, y.data = Relative_Representation_Sense_ThisBin, Chr = Chr, Condition = Condition) %>%
    as.data.frame

  if(dir.exists(paste("../extdata/", gen, sep = ""))){
   geneModels <- readRDS(file = paste("../extdata/", gen, "/", gen, ".RDS", sep = ""))
  } else {
    # mart <- useMart('ensembl', dataset='hsapiens_gene_ensembl') # create a mart object
    # entrez2gene <- getBM(mart = mart,
    #                      attributes = c(
    #                        'hgnc_symbol',
    #                        'ensembl_gene_id',
    #                        "entrezgene_id",
    #                        "external_gene_name"),
    #                      filters = 'entrezgene_id',
    #                      values = genes$gene_id)
    # geneModels <- data.frame(genes)
    # geneModels$symbol <- entrez2gene$hgnc_symbol[match(geneModels$gene_id, entrez2gene$entrezgene_id)]
    # geneModels <- na.omit(geneModels)
  }

  # For data
  REVA1 <- data.frame(
    chrom = data.df$Chr,
    start = data.df$BinStart,
    end = data.df$BinStart + data.df$BinLength - 1,
    name = NA,
    value = data.df$y.data,
    condition = data.df$Condition)
  REVA1 <- subset(REVA1, chrom %in% chr)
  # REVA1 <- REVA1[-1,]
  # REVA1 <- subset(REVA1, value > 0)
  REVA2 <- unique(REVA1[,1:4])

  for (i in 1:length(unique(REVA1$condition))){
    condition_name <- unique(REVA1$condition)[i]
    condition_subset <- data.frame(condition = REVA1$value[REVA1$condition %in% condition_name])
    names(condition_subset) <- condition_name
    REVA2 <- cbind(REVA2, condition_subset)
  }

  REVA2[,-c(1:4)][REVA2[,-c(1:4)] == 0] <- NA

  dtrack <- DataTrack(data = REVA2[,-c(1:4)], start = REVA2$start,
                      end = REVA2$end, chromosome = chr, genome = gen,
                      name = "REVA", groups = unique(REVA1$condition),
                      na.rm = TRUE)

  if(!is.null(data2)){
    data2.df <- subset(data2, Chr %in% chr) %>%
      group_by(Condition, Chr) %>%
      filter(duplicated(Chr) | n()==1) # removes the first instance of duplicated values (summary of each chr)
    data2.df <- data2.df %>%
      group_by(Condition) %>%
      mutate(Relative_Representation_Sense_ThisBin = (UniqueKCountS/UniqueKNumber)/(sum(UniqueKCountS)/sum(UniqueKNumber)))
    data2.df <- data2.df %>%
      dplyr::select(BinStart = BinStart, BinLength = BinLength, y.data = Relative_Representation_Sense_ThisBin, Chr = Chr, Condition = Condition) %>%
      as.data.frame
    REVA3 <- data.frame(
      chrom = data2.df$Chr,
      start = data2.df$BinStart,
      end = data2.df$BinStart + data2.df$BinLength - 1,
      name = NA,
      value = data2.df$y.data,
      condition = data2.df$Condition)
    REVA3 <- subset(REVA3, chrom %in% chr)
    # REVA3 <- REVA3[-1,]
    # REVA3 <- subset(REVA3, value > 0)
    REVA4 <- unique(REVA3[,1:4])

    for (i in 1:length(unique(REVA3$condition))){
      condition_name <- unique(REVA3$condition)[i]
      condition_subset <- data.frame(condition = REVA3$value[REVA3$condition %in% condition_name])
      names(condition_subset) <- condition_name
      REVA4 <- cbind(REVA4, condition_subset)
    }

    REVA4[,-c(1:4)][REVA4[,-c(1:4)] == 0] <- NA

    dtrack2 <- DataTrack(data = REVA4[,-c(1:4)], start = REVA4$start,
                         end = REVA4$end, chromosome = chr, genome = gen,
                         name = "REVA2", groups = unique(REVA3$condition),
                         na.rm = TRUE)
  }

  strack <- SequenceTrack(Hsapiens, chromosome = chr)

  itrack <- IdeogramTrack(genome = gen, chromosome = chr)

  if(!is.null(start)){
    start_pos <- start
  } else {
    start_pos <- subset(itrack@bandTable, chrom %in% chr) %>% dplyr::select(chromStart) %>% min
  }

  if(!is.null(end)){
    end_pos <- end
  } else {
    end_pos <- subset(itrack@bandTable, chrom %in% chr) %>% dplyr::select(chromEnd) %>% max
  }

  if(nrow(subset(geneModels, start >= start_pos & end <= end_pos & seqnames %in% chr)) < 500){
    grtrack <- GeneRegionTrack(subset(geneModels, seqnames %in% chr), genome = gen,
                               chromosome = chr, name = "UCSC Genes",
                               transcriptAnnotation = "symbol",
                               background.panel = "#FFFEDB",
                               background.title = "darkblue")
  } else {
    grtrack <- GeneRegionTrack(subset(geneModels, seqnames %in% chr), genome = gen,
                               chromosome = chr, name = "UCSC Genes",
                               background.panel = "#FFFEDB",
                               background.title = "darkblue")
  }

  gtrack <- GenomeAxisTrack()

  if(exists("dtrack2")){
    plotTracks(list(itrack, grtrack, dtrack, dtrack2, gtrack, strack),
               from = start_pos, to = end_pos)
  } else {
    plotTracks(list(itrack, grtrack, dtrack, gtrack, strack),
               from = start_pos, to = end_pos)
  }
}
