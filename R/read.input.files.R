# Function read.input.files reads in REVA output files

read.input.files <- function(file.list, condition, normalized, FeatureFile){
  upload = list()
  for(nr in 1:length(file.list[, 1])){
    data.df <- read.table(
      file = file.list[[nr, 'datapath']],
      sep = c("\t"),
      header = T,
      fill = T,
      quote = c(""),
      blank.lines.skip = T,
      strip.white = T,
      flush = T,
      allowEscapes = T,
      stringsAsFactors = F,
      encoding = "latin1",
      skip = max(grep(pattern = "<!", readLines(file.list[[nr, 'datapath']]), value = FALSE)) - 1
    )
    data.df[, c((ncol(data.df) - 1), ncol(data.df))] <- NULL
    names(data.df) <- gsub(pattern = "\\__.*", "", names(data.df))

    if(normalized){
      if(FeatureFile != "Feature Summary"){
        for(i in 4:ncol(data.df)){
          data.df[2:nrow(data.df), i] <- data.df[2:nrow(data.df), i]/(data.df[1, i]/1e6)
        }
      } else {
        for(i in 10:ncol(data.df)){
          data.df[, i] <- data.df[, i]/(sum(data.df[, i])/1e6)
        }
      }
    }

    if(FeatureFile == "Feature Summary"){
      names(data.df)[1] <- "Chr"
      names(data.df)[names(data.df) %in% c("BinStart", "Start")] <- "BinStart"
      data.df$gene <- sapply(
        X = data.df$Attribute,
        FUN = function(x){
          strsplit(x, split = ";")[[1]][2] %>%
            gsub(pattern = "Name=", replacement = "")
        }
      )
    } else {
      names(data.df)[1] <- ifelse(names(data.df)[1] == "Chromosome", "Chr", names(data.df)[1])
    }

    upload[[nr]] <- data.df
    names(upload)[[nr]] <- paste(condition, nr, sep = "_")
  }
  return(bind_rows(upload, .id = "Condition")) # append condition
}
