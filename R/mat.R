mat <- function(A,
                B,
                style = c("inline", "frac", "sfrac", "decimal"),
                bracket = c("parenthese", "crochet", "determinant"),
                digits = 2){

  if (!is.matrix(A)) stop("A n'est pas une matrice.")

  if (missing(style)  &&  missing(digits)) style = "frac"
  else if (missing(style)  &&  !missing(digits)) style = "decimal"
  else style = match.arg(style, c("inline", "frac", "sfrac", "decimal"))

  if (missing(bracket)) bracket = "crochet"
  else bracket <- match.arg(bracket, c("parenthese", "crochet", "determinant"))

  if (bracket == "parenthese") cat('\\left(\n')
  else if (bracket == "crochet") cat('\\left[\n')
  else if (bracket == "determinant") cat('\\left|\n')

  if (!is.null(attr(A, "style"))) style = attr(A, "style")
  if (!is.null(attr(A, "bracket"))) bracket = attr(A, "bracket")
  if (!is.null(attr(A, "digits"))) digits = attr(A, "digits")

  if (!missing(B)){
    cat(paste(c("\\begin{array}{",rep("r",nrow(A)),"|",rep("r",nrow(B)),"}\n"), collapse = ""))
    A <- cbind(A, B)
  }
  else{
    cat(paste(c("\\begin{array}{",rep("r",nrow(A)),"}\n"), collapse = ""))
  }

  for (i in (1:nrow(A))){
    cat(frac(A[i, ], style = style, digits = digits), sep = " & ")
    cat(" \\\\ \n")
  }

  cat("\\end{array}\n")

  if (bracket == "parenthese") cat('\\right)\n')
  else if (bracket == "crochet") cat('\\right]\n')
  else if (bracket == "determinant") cat('\\right|\n')

}
