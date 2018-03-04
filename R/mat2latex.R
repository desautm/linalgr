#' Affichage LaTeX d'une matrice
#'
#' \code{mat2latex} retourne un vecteur de caracteres qui contient le code LaTeX permettant d'ecrire une matrice \code{A}.
#' Si nous ajoutons une deuxieme matrice \code{B}, nous avons l'affichage de la matrice augmentee avec une barre verticale
#' separant les deux matrices. Nous pouvons choisir la facon d'afficher la matrice. Nous pouvons afficher des nombres entiers ou alors
#' choisir le nombre de chiffres a droite de la virgule. Nous pouvons afficher le resultat sous forme de fraction. Nous pouvons
#' afficher la fraction \code{inline} ou alors utiliser \code{frac}, \code{dfrac}, \code{sfrac} et \code{tfrac}. Le parametre \code{verbose}
#' permet d'afficher ou non la matrice. Le parametre \code{copy2clip} permet de copier ou non le resultat dans le presse-papier.
#'
#' @param A une matrice
#' @param B une matrice (facultatif)
#' @param style permet de choisir la facon d'afficher les elements de la matrice. Les choix possibles sont \code{inline}, \code{frac},
#'   \code{dfrac}, \code{sfrac} et \code{tfrac}. Par defaut, nous affichons des entiers ou des nombres decimaux.
#' @param bracket permet de choisir comment encadrer la matrice. Les choix sont des \code{crochet}s, des \code{parenthese}s ou un \code{determinant}.
#'   Par defaut, nous encadrons entre \code{crochet}s.
#' @param digits Le nombre de chiffres a droite de la virgule a afficher. Nous affichons 2 chiffres par defaut.
#' @param verbose Si \code{TRUE} nous affichons le code LaTeX, si \code{FALSE} nous retournons le vecteur de caracteres. \code{TRUE} par defaut.
#' @param copy2clip Si \code{TRUE} nous copions le resultat dans le presse papier. Par defaut \code{FALSE}.
#' @param envir Si \code{TRUE}, nous encadrons le resultat dans un array et un bracket. Si \code{FALSE}, on ne retourne que la matrice. Utile
#'   pour la fonction \code{sel2latex}.
#' @param tolatex Si \code{TRUE} nous encadrons le resultat par "$$ resultat $$".
#' @return Le vecteur de caracteres contenant le code LaTeX de la matrice
#' @importFrom MASS fractions
#' @export
#' @examples
#' A <- matrix(c(2, -4, 5,
#'               -3, 5, 7,
#'               10, 0, -1), 3, 3)
#' B <- matrix(c(0, -1, 5), 3, 1)
#' mat2latex(A)
#' mat2latex(A, bracket = "parenthese")
#' mat2latex(A,B)
#' mat2latex(A/3)
#' mat2latex(A/3, digits = 4)
#' mat2latex(A/3, style = "inline")
#' mat2latex(A/3, style = "frac")

mat2latex <- function(A,
                      B = NULL,
                      style = c("decimal", "inline", "frac", "sfrac", "tfrac", "dfrac"),
                      bracket = c("crochet", "parenthese", "determinant"),
                      verbose = TRUE,
                      copy2clip = FALSE,
                      envir = TRUE,
                      tolatex = TRUE,
                      digits = 2){

  if ((!is.matrix(A)) || (!is.numeric(A)))
    stop("A doit etre une matrice numerique.")

  style <- match.arg(style, c("decimal", "inline", "frac", "sfrac", "tfrac", "dfrac"))
  bracket <- match.arg(bracket, c("crochet", "parenthese", "determinant"))

  if (!is.null(attr(A, "bracket"))) bracket <- attr(A, "bracket")
  if (!is.null(attr(A, "style"))) style <- attr(A, "style")
  if (!is.null(attr(A, "verbose"))) verbose <- attr(A, "verbose")
  if (!is.null(attr(A, "copy2clip"))) copy2clip <- attr(A, "copy2clip")
  if (!is.null(attr(A, "digits"))) digits <- attr(A, "digits")
  if (!is.null(attr(A, "envir"))) envir <- attr(A, "envir")
  if (!is.null(attr(A, "tolatex"))) tolatex <- attr(A, "tolatex")

  # Creation de l'environnement array avec le bon nombre de {rrr...r}
  if (!is.null(B)){
    if ((!is.matrix(B)) || (!is.numeric(B)))
      stop("B doit etre une matrice numerique.")
    if (nrow(A) != nrow(B)) stop("Le nombre de lignes de A doit etre egal au nombre de lignes de B.")
    begin <- paste0(c("\\begin{array}{", rep("r", ncol(A)),"|", rep("r", ncol(B)),"}\n"), collapse = "")
    A <- cbind(A,B)
  }
  else{
    begin <- paste0(c("\\begin{array}{", rep("r", ncol(A)),"}\n"), collapse = "")
  }
  end <- c("\\end{array}")

  # Creation d'un vecteur vide
  toprint <- vector("character", length = nrow(A))

  # Creation de la matrice
  tempA <- A
  if (ndigits(A) > 0){
    if (style == "decimal") tempA <- specify_decimal(A, digits)
    else{
      tempA <- MASS::fractions(A)
      if (style == "frac") tempA <- gsub("(\\d+)/(\\d+)", "\\\\frac{\\1}{\\2}", tempA)
      else if (style == "tfrac") tempA <- gsub("(\\d+)/(\\d+)", "\\\\tfrac{\\1}{\\2}", tempA)
      else if (style == "sfrac") tempA <- gsub("(\\d+)/(\\d+)", "\\\\sfrac{\\1}{\\2}", tempA)
      else if (style == "dfrac") tempA <- gsub("(\\d+)/(\\d+)", "\\\\dfrac{\\1}{\\2}", tempA)
    }
  }

  for (i in (1:nrow(A))){
    toprint[i] <- paste(tempA[i, ], collapse = " & ")
  }

  # Encadrement de la sortie entre crochet, parenthese, determinant
  if (bracket == "crochet"){
    bra <- "\\left[\n"
    ket <- "\n\\right]"
  }
  else if (bracket == "parenthese"){
    bra <- "\\left(\n"
    ket <- "\n\\right)"
  }
  else{
    bra <- "\\left|\n"
    ket <- "\n\\right|"
  }

  # Encadrement de la sortie dans l'environnement array et bracket
  if (envir){
    toprint <- paste(toprint, collapse = " \\\\ \n")
    toprint <- paste(toprint, "\\\\ \n")
    toprint <- paste0(c(begin, toprint, end), collapse = "")
    toprint <- paste0(c(bra, toprint, ket), collapse = "")
  }
  else{
    toprint <- NULL
    toprint <- tempA
  }

  if (tolatex && envir) toprint <- paste0(c("$$\n",toprint,"\n$$"))

  if (copy2clip){
    test2clip <- writeClipboard(toprint)
    if (test2clip) message("La matrice a ete correctement copiee dans le presse-papier.")
    else message("La matrice n'a pas ete copiee dans le presse-papier.")
  }

  if (verbose){
    cat(toprint)
    return(invisible(toprint))
  }
  else return(toprint)

}
