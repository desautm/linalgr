#' Affichage LaTeX d'une matrice
#'
#' \code{mat} permet l'affichage d'une matrice en code LaTeX. Nous pouvons choisir
#' le \code{style} pour afficher sous forme de fractions. Nous pouvons afficher
#' la matrice avec des parentheses, des crochets ou des determinants avec \code{bracket}.
#' Si la matrice \code{B} est presente, on affiche la matrice augmentee.
#'
#' @param A une matrice
#' @param B une matrice
#' @param style; Permet de choisir le style d'affichage des fractions. \code{"inline"}; permet d'ecrire la fraction sous la forme a/b.
#'   \code{"frac"}; permet d'ecrire la fraction sous la forme frac\{a\}\{b\}. \code{"sfrac"}; permet d'ecrire la fraction sous la
#'   forme sfrac\{a\}\{b\}. \code{"decimal"}; permet d'ecrire la fraction sous la forme decimale 0.34...
#' @param bracket; Permet de choisir la facon d'encadrer les matrices. \code{parenthese}; encadre la matrice de parentheses.
#'   \code{crochet}; encadre la matrice de crochets. \code{determinant}; encadre la matrice de valeurs absolues
#' @param digits Permet de choisir le nombre de chiffres significatifs a afficher.
#' @return Retourne le code LaTex de la matrice
#' @seealso \code{\link{frac}}
#' @export

mat <- function(A,
                B = NULL,
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

  if (!missing(B) & !is.matrix(B)) stop("B n'est pas une matrice.")
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
