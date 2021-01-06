# Function chromPlot

chromPlot <- function(
  data,
  chr,
  start,
  end,
  feature,
  norm_feature = "",
  log_scale = F,
  point_size,
  point_color,
  text_size,
  genes,
  ymin,
  ymax = NULL,
  FeatureFile = TRUE){

  if(is.null(data)){
    return(NULL)
  }

  if(FeatureFile){
    data.df <- subset(data, Chr %in% chr)
  } else {
    data.df <- subset(data, Chr %in% chr) %>%
      group_by(Condition, Chr) %>%
      filter(duplicated(Chr) | n()==1) %>% # removes the first instance of duplicated values (summary of each chr)
      as.data.frame
  }

  if(norm_feature != ""){
    # data.df <- data.df %>%
    #   select(
    #     BinStart = BinStart,
    #     y.data = feature,
    #     y.norm.data = norm_feature,
    #     Chr = Chr,
    #     Condition = Condition) %>%
    #   as.data.frame
    # data.df$y.data <- data.df$y.data/data.df$y.norm.data

    #data.df <- data.df %>% mutate(y.data = feature/norm_feature) %>% as.data.frame()
    data.df$y.data <- data.df[feature][,1]/data.df[norm_feature][,1]
  } else {
    # data.df <- data.df %>%
    #   select(
    #     BinStart = BinStart,
    #     y.data = feature,
    #     Chr = Chr,
    #     Condition = Condition)

    #data.df <- data.df %>% mutate(y.data = feature) %>% as.data.frame()
    data.df$y.data <- data.df[feature][,1]
  }

  # if(labels & FeatureFile){
  #   data.df$labels <- attribute_labels
  #   cat(file=stderr(), "HELP")
  # }

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

  g <- subset(data.df, y.data > 0) %>%
    ggplot() +
    xlab("Genomic coordinates (Mb)") +
    theme(panel.border = element_rect(colour = "black", fill = NA)) +
    theme_classic() +
    geom_hline(yintercept = 0) +
    theme(text = element_text(size = text_size)) +
    ggtitle(label = chr)

  if(!is.null(ymax)){
    g <- g + ylim(c(ymin, ymax))
  }

  if(log_scale){
    g <- g +
      geom_point(
        aes(x = BinStart/1000000,
            y = log10(y.data),
            shape = Condition),
        size = point_size,
        col = point_color) +
      ylab(
        ifelse(
          norm_feature == "",
          paste(
            "log10(",
            feature,
            ")",
            sep = ""
          ),
          paste(
            "log10(",
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
            shape = Condition),
        col = point_color,
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

  if(!is.null(genes) & log_scale){
    g <- g + geom_text_repel(
      data = subset(g$data, gene %in% genes),
      aes(
        x = BinStart/1000000,
        y = log10(y.data),
        label = gene))
  } else if(!is.null(genes) & !log_scale){
    g <- g + geom_text_repel(
      data = subset(g$data, gene %in% genes),
      aes(
        x = BinStart/1000000,
        y = y.data,
        label = gene))
  }
  # ggplotly(g)
  return(g)
}
