# Function chromPlot

chromPlot <- function(data, chr, start, end, feature, norm_feature = "", log_scale = NULL, point_size){

  if(is.null(data)){
    return(NULL)
  }

  data.df <- subset(data, Chr %in% chr) %>%
    group_by(Condition, Chr) %>%
    filter(duplicated(Chr) | n()==1) # removes the first instance of duplicated values (summary of each chr)

  if(norm_feature != ""){
    data.df <- data.df %>%
      select(BinStart = BinStart, y.data = feature, y.norm.data = norm_feature, Chr = Chr, Condition = Condition) %>%
      as.data.frame
    data.df$y.data <- data.df$y.data/data.df$y.norm.data
  } else {
    data.df <- data.df %>%
      select(BinStart = BinStart, y.data = feature, Chr = Chr, Condition = Condition) %>%
      as.data.frame
  }

  if(!is.na(start)){
    data.df <- subset(
      data.df,
      BinStart >= as.numeric(as.character(start))
    )
  }

  if(!is.na(end)){
    data.df <- subset(
      data.df,
      BinStart <= as.numeric(as.character(end))
    )
  }

  g <- ggplot(subset(data.df, y.data > 0)) +
    xlab("Genomic coordinates (Mb)") +
    theme(panel.border = element_rect(colour = "black", fill = NA)) +
    theme_classic() +
    geom_hline(yintercept = 0)

  if(log_scale){
    g <- g +
      geom_point(
        aes(x = BinStart/1000000,
            y = log2(y.data),
            col = Chr,
            shape = Condition),
        size = point_size) +
      ylab(
        ifelse(
          norm_feature == "",
          paste(
            "log2(",
            feature,
            ")",
            sep = ""
          ),
          paste(
            "log2(",
            feature,
            "/",
            norm_feature,
            ")",
            sep = ""
          )
        )
      )
  } else {
    g <- g +
      geom_point(
        aes(x = BinStart/1000000,
            y = y.data,
            col = Chr,
            shape = Condition),
        size = point_size) +
      ylab(
        ifelse(
          norm_feature == "",
          feature,
          paste(
            feature,
            norm_feature,
            sep = "/"
          )
        )
      )
  }
  # ggplotly(g)
  return(g)
}
