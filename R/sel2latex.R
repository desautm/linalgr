#' @export
sel2latex <- function(A,
                      B,
                      sel = TRUE,
                      variables = c("x", "a", "xi"),
                      style = c("decimal", "inline", "frac", "sfrac", "tfrac", "dfrac"),
                      bracket = c("crochet", "parenthese", "determinant"),
                      verbose = TRUE,
                      concise = FALSE,
                      copy2clip = FALSE,
                      tolatex = TRUE,
                      digits = 2){

  if ((!is.matrix(A)) || (!is.numeric(A)))
    stop("A doit etre une matrice numerique.")
  if ((!is.matrix(B)) || (!is.numeric(B)))
    stop("B doit etre une matrice numerique.")
  if (ncol(B) > 1) stop("B doit posseder une seule colonne")

  style <- match.arg(style, c("decimal", "inline", "frac", "sfrac", "tfrac", "dfrac"))
  bracket <- match.arg(bracket, c("crochet", "parenthese", "determinant"))
  variables <- match.arg(variables, c("x", "a", "xi"))

  if (ncol(A) > 26 && variables == "a") stop("La matrice possede trop de colonnes pour utiliser l'option a, b, c, ...")
  if (ncol(A) > 4 && variables == "x") stop("La matrice possede trop de colonnes pour utiliser l'option w, x, y, z.")

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
    matA <- mat2latex(A, envir = FALSE, tolatex = FALSE)
    matB <- mat2latex(B, envir = FALSE, tolatex = FALSE)

    toprint <- vector("character", length = nrow(A))
    begin <- paste0(c("\\begin{array}{", rep("r", 2*ncol(A)+1),"}\n"), collapse = "")
    end <- c("\\end{array}")
    var <- paste("x_{",(1:ncol(A)),"}", sep = "")
    for (i in (1:nrow(A))){
      toprint[i] <- paste0(paste(matA[i, ], var, collapse = " & + & "), " & = & ", matB[i, ], " \\\\ \n")
    }

    # Sanitize en nettoyant la matrice
    toprint <- sanitize(toprint, concise)

    toprint <- paste0(c(begin, toprint, end), collapse = "")
    toprint <- convert_var(toprint, ncol(A), variables)
  }
  else{
    latexA <- mat2latex(A)
    latexVar <- var2latex(ncol(A), bracket, variables)
    latexB <- mat2latex(B)
    toprint <- c(latexA, latexVar, "=", latexB)
  }

  if (tolatex) toprint <- paste0(c("$$\n",toprint,"\n$$"))

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

  if (copy2clip){
    test2clip <- writeClipboard(toprint)
    if (test2clip) message("Le SEL a ete correctement copie dans le presse-papier.")
    else message("Le SEL n'a pas ete copie dans le presse-papier.")
  }

  if (verbose){
    cat(toprint)
    return(invisible(toprint))
  }
  else return(toprint)

}
