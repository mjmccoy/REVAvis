# Function ManhattanPlot

ManhattanPlot <- function(
  data,
  data2 = NULL,
  chr,
  feature,
  norm_feature = "",
  log_scale = FALSE,
  ymin,
  ymax = NULL,
  point_size,
  text_size,
  condition1_name = NULL,
  condition2_name = NULL){
  if(is.null(data)){
    return(NULL)
  }
  # req(chr)
  if(norm_feature != ""){
    data.df <- subset(data, Chr %in% chr) %>%
      select(c("Condition", "Chr", "BinStart", feature, norm_feature)) %>%
      group_by(Condition, Chr) %>%
      filter(duplicated(Chr) | n()==1) # removes the first instance of duplicated values (summary of each chr)
  } else {
    data.df <- subset(data, Chr %in% chr) %>%
      select(c("Condition", "Chr", "BinStart", feature)) %>%
      group_by(Condition, Chr) %>%
      filter(duplicated(Chr) | n()==1) # removes the first instance of duplicated values (summary of each chr)
  }
  if(!is.null(data2)){
    if(norm_feature != ""){
      data2.df <- subset(data2, Chr %in% chr) %>%
        select(c("Condition", "Chr", "BinStart", feature, norm_feature)) %>%
        group_by(Condition, Chr) %>%
        filter(duplicated(Chr) | n()==1)# removes the first instance of duplicated values (summary of each chr)
      data.df[,4] <- data.df[,4]/data.df[,5] # normalize feature 1 to feature 2 in condition 1
      data2.df[,4] <- data2.df[,4]/data2.df[,5] # normalize feature 1 to feature 2 in condition 2
      data.df[,4] <- data.df[,4]/data2.df[,4] # calculate normalized ratio of condition1/condition2
    } else {
      data2.df <- subset(data2, Chr %in% chr) %>%
        select(c("Condition", "Chr", "BinStart", feature)) %>%
        group_by(Condition, Chr) %>%
        filter(duplicated(Chr) | n()==1)# removes the first instance of duplicated values (summary of each chr)
      data.df[,4] <- data.df[,4]/data2.df[,4] # calculate ratio condition1/condition2
    }
  }
  data.df$Chr <- factor(data.df$Chr, levels = chr) # force chr order by selection
  data.df <- data.df[with(data.df, order(Chr, BinStart)), ]
  # data.df <- data.df %>% group_by(Condition) %>% mutate(Feature = 1:length(BinStart))
  data.df <- data.df %>%
    group_by(Condition) %>%
    mutate(Feature = 1:length(BinStart)) %>%
    select(Feature = Feature, y.data = feature, Chr = Chr, Condition = Condition) %>%
    as.data.frame
  # g <- data.df %>% ggplot(aes_string(x = "Feature", y = feature, col = "Chr", shape = "Condition")) +
  g <- subset(data.df, y.data > 0) %>%
    ggplot() +
    xlab("") +
    theme(panel.border = element_rect(colour = "black", fill = NA, size = 1)) +
    geom_hline(yintercept = 0) +
    theme_classic() +
    theme(
      axis.ticks = element_blank(),
      axis.text.x = element_blank(),
      text = element_text(size = text_size))
  if(log_scale){
    g <- g +
      geom_point(aes(x = Feature, y = log10(y.data), col = Chr, shape = Condition), size = point_size) +
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
      geom_point(aes(x = Feature, y = y.data, col = Chr, shape = Condition), size = point_size) +
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

  if(!is.null(data2)){
    if(norm_feature != ""){
      g <- g + ylab(paste(feature, "/", norm_feature, " (", condition1_name, "/", condition2_name, ")", sep = ""))
    } else {
      g <- g + ylab(paste(feature, " (", condition1_name, "/", condition2_name, ")", sep = ""))
    }
  }

  if(!is.null(ymax)){
    g <- g + ylim(c(ymin, ymax))
  }

  return(g)
}
