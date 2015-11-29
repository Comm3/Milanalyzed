str_match <- function(word1, words){
  dst <- 100
  word <- ""
  for (wd in words) {
    wd <- as.character(wd)
    distance <- stringdist(tolower(word1), tolower(encodeString(wd)), method = "jw")
    if (distance < dst) {
      dst <- distance
      word <- wd
    } 
  }
  return(word)
}
