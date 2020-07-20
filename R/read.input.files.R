# Function read.input.files reads in REVA output files

read.input.files <- function(file.list, condition){
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
    names(data.df)[1] <- ifelse(names(data.df)[1] == "Chromosome", "Chr", names(data.df)[1])
    upload[[nr]] <- data.df
    names(upload)[[nr]] <- paste(condition, nr, sep = "_")
  }
  return(bind_rows(upload, .id = "Condition")) # append condition
}
