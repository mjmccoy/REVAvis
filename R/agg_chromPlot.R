# Function agg_chromPlot

agg_chromPlot <- function(
  data1,
  data2,
  chr,
  feature,
  norm_feature = "",
  mean_se = FALSE,
  log_scale = NULL,
  ymin,
  ymax = NULL,
  point_color_1,
  point_color_2,
  point_size,
  text_size){
  if(is.null(data1) || is.null(data2)){
    return(NULL)
  }
  data.df <- rbind(data1, data2) %>%
    subset(Chr %in% chr) %>%
    group_by(Condition, Chr) %>%
    filter(duplicated(Chr) | n()==1) %>%
    mutate(group = gsub("\\_.*", "", Condition))
  if(norm_feature == ""){
    data.df <- data.df %>%
    select(BinStart = BinStart, y.data = feature, Chr = Chr, Condition = Condition, group = group) %>%
    as.data.frame
  } else {
    data.df <- data.df %>%
      select(BinStart = BinStart, y.data = feature, y.norm.feature = norm_feature, Chr = Chr, Condition = Condition, group = group) %>%
      as.data.frame
    data.df$y.data <- data.df$y.data/data.df$y.norm.feature
  }
  if(log_scale){
    g <- ggplot(subset(data.df, y.data > 0), aes(x = BinStart/1000000, y = log10(y.data), col = group)) +
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
    g <- ggplot(subset(data.df, y.data > 0), aes(x = BinStart/1000000, y = y.data, col = group)) +
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
  g <- g +
    xlab("Genomics coordinates (Mb)") +
    theme(panel.border = element_rect(colour = "black", fill = NA, size = 1), text = element_text(size = text_size)) +
    geom_hline(yintercept = 0) +
    scale_color_manual(values = c(point_color_1, point_color_2)) +
    theme_classic()

  if(mean_se){
    g <- g + stat_summary()
  } else {
    g <- g + geom_point()
  }

  if(!is.null(ymax)){
    g <- g + ylim(c(ymin, ymax))
  }

  # ggplotly(g)
  return(g)
}
