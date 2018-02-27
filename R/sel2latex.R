#' @export

sel2latex <- function(A,
                      B,
                      sel = TRUE,
                      variables = c("x", "a", "xi"),
                      style = c("decimal", "inline", "frac", "sfrac", "tfrac", "dfrac"),
                      bracket = c("crochet", "parenthese", "determinant"),
                      verbose = TRUE,
                      copy2clip = FALSE,
                      digits = 2){

  toprint = NULL

  if ((!is.matrix(A)) || (!is.numeric(A)))
    stop("A doit etre une matrice numerique.")
  if ((!is.matrix(B)) || (!is.numeric(B)))
    stop("B doit etre une matrice numerique.")

  style <- match.arg(style, c("decimal", "inline", "frac", "sfrac", "tfrac", "dfrac"))
  bracket <- match.arg(bracket, c("crochet", "parenthese", "determinant"))
  variables <- match.arg(variables, c("x", "a", "xi"))

  # Attributs aux matrices pour simplifier le code
  attr(A, "style") <- style
  attr(B, "style") <- style
  attr(A, "bracket") <- bracket
  attr(B, "bracket") <- bracket
  attr(A, "verbose") <- FALSE
  attr(B, "verbose") <- FALSE
  attr(A, "copy2clip") <- FALSE
  attr(B, "copy2clip") <- FALSE
  attr(A, "digits") <- digits
  attr(B, "digits") <- digits


  if (sel){

  }
  else{
    latexA <- mat2latex(A)
    latexVar <- var2latex(ncol(A), bracket, variables)
    latexB <- mat2latex(B)
    toprint <- c(latexA, latexVar, "=", latexB)
  }

  # On enleve les attributs aux matrices
  attr(A, "style") <- NULL
  attr(B, "style") <- NULL
  attr(A, "bracket") <- NULL
  attr(B, "bracket") <- NULL
  attr(A, "verbose") <- NULL
  attr(B, "verbose") <- NULL
  attr(A, "copy2clip") <- NULL
  attr(B, "copy2clip") <- NULL
  attr(A, "digits") <- NULL
  attr(B, "digits") <- NULL

  if (copy2clip) writeClipboard(toprint)

  if (verbose){
    cat(toprint)
    return(invisible(toprint))
  }
  else return(toprint)

}
