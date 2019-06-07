setwd("M:/soildata/TAMU_NASDM_Full-2013-12-10")

f <- list.files()

get_TAMU <- function(folder, file) {
  f = lapply(folder, function(x) {
    fp = paste0(x, "/", x, "_", file, ".txt")
    if (file.exists(fp) & length(readLines(fp)) > 1) {
    f2 = read.table(fp, 
                    header = TRUE, 
                    sep = "\t", 
                    stringsAsFactors = FALSE,
                    fill = TRUE)
    }})
  do.call("rbind", f)
  }

depths <- get_TAMU(f, "depths")
deps_sub <- subset(deps, !grepl("-", SHC))

stations <- get_TAMU(f, "stations")
