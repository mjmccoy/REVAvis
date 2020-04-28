# Function read.input.files reads in REVA output files

read.input.files <- function(file.list, condition, normalized, header, feature){
  upload = list()
  for(nr in 1:length(file.list[, 1])){
    data.df <- read.table(
      file = pipe(paste("tr '\r' '\n' < ", file.list[[nr, 'datapath']], " | ",  "cut -f1-3,", which(names(header) == feature), sep = "")),
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
    names(data.df) <- gsub(pattern = "\\__.*", "", names(data.df))
    if(normalized){
      for (i in 2:nrow(data.df)){
        data.df[i, 4] <- (data.df[i, 4]*1e6)/data.df[1, 4]
      }
    }
    upload[[nr]] <- data.df
    names(upload)[[nr]] <- paste(condition, nr, sep = "_")
  }
  return(bind_rows(upload, .id = "Condition")) # append condition
}
