library(dplyr)

base.url <- "http://cran-logs.rstudio.com"
lookup.table <- data.frame(code = 1, value = NA, category = "general", stringsAsFactors = FALSE)
dates <- seq(as.Date("2012-10-01"), Sys.Date(), by = 1)

# for(d in dates) {
#   
#   d <- as.Date(d, origin = as.Date("1970-01-01"))
#   y <- format(d, "%Y")
#   fn <- format(d, "%Y-%m-%d.csv.gz")
#   ds <- paste0("logs/", fn)
#   if(!file.exists(ds)) {
#     if(class(try(download.file(paste(base.url, y, fn, sep = "/"), ds))) == "try-error") {
#       unlink(ds)
#     }
#   }
#   
# }

buildLookup <- function(lut, df, col, cat) {
  v <- unique(df[[col]])
  v <- v[!v %in% lut$value[lut$category == cat]]
  if(length(v) > 0) {
    pk <- max(lut$code)
    pk <- (pk+1):(pk+length(v))
    return(rbind(lut, data.frame(code = pk, value = v, category = cat)))
  } else {
    return(lut)
  }
}

for(f in list.files("logs/")) {
  df <- read.csv(gzfile(paste0("logs/", f)), stringsAsFactors = FALSE)
  lookup.table <- lookup.table %>% 
    buildLookup(df, "r_version", "r_version") %>%
    buildLookup(df, "r_arch", "arch") %>%
    buildLookup(df, "r_os", "os") %>%
    buildLookup(df, "package", "package") %>%
    buildLookup(df, "country", "country") %>%
    arrange(category)
}

save(lookup.table, file = "data/lookupTable.rda")
  