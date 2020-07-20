# Function chromPlot

chromPlot <- function(data, chr, start, end, log_scale = NULL){
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

  if(!is.null(start)){
    data.df <- subset(
      data.df,
      BinStart >= as.numeric(as.character(start))
    )
  }
  if(!is.null(end)){
    data.df <- subset(
      data.df,
      BinStart <= as.numeric(as.character(end))
    )
  }

  g <- ggplot(subset(data.df, y.data > 0)) +
    xlab("Genomic coordinates (Mb)") +
    theme(panel.border = element_rect(colour = "black", fill = NA, size = 1)) +
    theme_classic() +
    geom_hline(yintercept = 0)
  if(log_scale){
    g <- g +
      geom_point(aes(x = BinStart/1000000, y = log2(y.data), col = Chr, shape = Condition)) +
      ylab("log2(Relative Representation)")
  } else {
    g <- g +
      geom_point(aes(x = BinStart/1000000, y = y.data, col = Chr, shape = Condition)) +
      ylab("Relative Representation")
  }
  # ggplotly(g)
  return(g)
}
