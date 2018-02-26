frac <- function(A){
  B <- MASS::fractions(A)
  id <- grep("(\\d+)/(\\d+)", B)
  if (length(id) > 0){
    num <- gsub("(\\d+)/(\\d+)", "\\1", B)
    den <- gsub("(\\d+)/(\\d+)", "\\2", abs(B))
  }
  else{
    num <- B
    den <- matrix(1, nrow(B), ncol(B))
  }
  return(list("num" = num, "den" = den))
}
