# Function read.header.input.files reads only the column names of REVA output files

read.header.input.files <- function(file.list, condition){
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
      nrows = 2,
      skip = max(grep(pattern = "<!", readLines(file.list[[nr, 'datapath']]), value = FALSE)) - 1
    )
    data.df[, c((ncol(data.df) - 1), ncol(data.df))] <- NULL
    names(data.df) <- gsub(pattern = "\\__.*", "", names(data.df))
    upload[[nr]] <- data.df
  }
  return(bind_rows(upload))
}
