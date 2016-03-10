sub <- function(x, replace=c("--"), by=NA) {
  x <- as.character(x)
  idx <- which(x %in% replace)
  if (length(idx) > 0) x[idx] <- by
  x
}

fixYear <- function(date) {
  month <- month(date)
  dm <- c(0, diff(month))
  cdm <- cumsum(dm > 2)
  2016 - cdm
}

fixName <- function(name) {
  name <- gsub("*","",name, fixed=TRUE)
  name <- gsub(" (Tracking)", "", name, fixed=TRUE)
  name <- gsub("Mrng.", "Morning", name, fixed=TRUE)
  name <- name %>% str_replace_all("CBS News", "CBS") %>%
    str_replace_all("Opinion Research", "ORC") %>%
    str_replace_all("ABC News", "ABC") %>%
    str_replace_all("NY Times", "NYT") %>%
    str_replace_all("NBC News", "NBC") %>%
    str_replace_all("Wash Post", "WP") %>%
    str_replace_all("Wall St. Jrnl", "WSJ") %>%
    str_replace_all("Gravis Marketing", "Gravis") %>%
    str_replace_all("Suffolk University", "Suffolk") %>%
    str_replace_all("St. Anselm", "Saint Anselm") %>%
    str_replace_all("UMass Lowell", "UMass") %>%
    str_replace_all("University", "U.") %>%
    str_replace_all("SUSA", "SurveyUSA") %>%
    str_replace_all("Texas Tribune", "TX Tribune") %>%
    str_replace_all("Christopher Newport Univ.", "CNU") %>%
    str_replace_all("Univ. of Mary Washington", "UMW") 
    
  name <- gsub("([A-Z][A-Z a-z()0-9]+)(.*)\\1", "\\1\\2", name)
  name <- gsub("([A-Z][A-Z a-z()0-9]+)(.*)\\1", "\\1\\2", name)
  splits <- strsplit(name, split="/") 
  sapply(splits, function(x) paste(unique(x),sep="/", collapse="/"))
}