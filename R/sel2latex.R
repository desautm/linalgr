#' Affichage LaTeX d'un systeme d'equations lineaires  (SEL)
#'
#' \code{sel2latex} retourne un vecteur de caracteres qui contient le code LaTeX permettant d'ecrire le SEL forme de la matrice \code{A}
#' et de la matrice \code{B}.
#' Nous pouvons choisir la facon d'afficher le SEL. Nous pouvons afficher des nombres entiers ou alors
#' choisir le nombre de chiffres a droite de la virgule. Nous pouvons afficher le resultat sous forme de fraction. Nous pouvons
#' afficher la fraction \code{inline} ou alors utiliser \code{frac}, \code{dfrac}, \code{sfrac} et \code{tfrac}. Le parametre \code{verbose}
#' permet d'afficher ou non le SEL. Le parametre \code{copy2clip} permet de copier ou non le resultat dans le presse-papier. Le parametre
#' \code{concise} permet de simplifier le SEL en enlevant les 0 lors de l'affichage du SEL
#'
#' @param A une matrice
#' @param B une matrice de constantes
#' @param sel Si \code{TRUE} nous affichons les equations et sinon nous affichons le SEL sous forme de matrices. \code{TRUE} par defaut
#' @param style permet de choisir la facon d'afficher les elements de la matrice. Les choix possibles sont \code{inline}, \code{frac},
#'   \code{dfrac}, \code{sfrac} et \code{tfrac}. Par defaut, nous affichons des entiers ou des nombres decimaux.
#' @param bracket permet de choisir comment encadrer la matrice. Les choix sont des \code{crochet}s, des \code{parenthese}s ou un \code{determinant}.
#'   Par defaut, nous encadrons entre \code{crochet}s.
#' @param digits Le nombre de chiffres a droite de la virgule a afficher. Nous affichons 2 chiffres par defaut.
#' @param verbose Si \code{TRUE} nous affichons le code LaTeX, si \code{FALSE} nous retournons le vecteur de caracteres. \code{TRUE} par defaut.
#' @param copy2clip Si \code{TRUE} nous copions le resultat dans le presse papier. Par defaut \code{FALSE}.
#' @param envir Si \code{TRUE}, nous encadrons le resultat dans un array et un bracket. Si \code{FALSE}, on ne retourne que la matrice.
#' @param tolatex Si \code{TRUE} nous encadrons le resultat par "$$ resultat $$".
#' @return Le vecteur de caracteres contenant le code LaTeX de la matrice
#' @export
#' @examples
#' A <- matrix(c(2, -4, 0,
#'               -3, 0, 7,
#'               0, 0, -1), 3, 3)
#' B <- matrix(c(0, -1, 5), 3, 1)
#' sel2latex(A, B)

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
                      digits = 2,
                      tol = sqrt(.Machine$double.eps)){

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
    var <- paste("x_{",(1:ncol(A)),"}", sep = "")
    for (i in (1:nrow(A))){
      toprint[i] <- paste0(paste(matA[i, ], var, collapse = " & + & "), " & = & ", matB[i, ], " \\\\ \n")
    }

    # Sanitize en nettoyant la matrice
    toprint <- sanitize(toprint, concise)

    if (concise){
      notzero <- apply(A, 1, function(x){sum(abs(x) > tol)})
      max_notzero <- max(notzero)
      begin <- paste0(c("\\begin{array}{", rep("r", 2*max_notzero+1),"}\n"), collapse = "")
      for (i in (1:nrow(A))){
        # On ajoute des & sur les ligne ou il en manque
        if (notzero[i] < max_notzero){
          num_esperluette <- 2*(max_notzero - notzero[i])
          toprint[i] <- paste0(c(rep("& ", num_esperluette), toprint[i]), collapse = "")
        }
      }
    }
    else begin <- paste0(c("\\begin{array}{", rep("r", 2*ncol(A)+1),"}\n"), collapse = "")

    end <- c("\\end{array}")

    toprint <- paste0(c(begin, toprint, end), collapse = "")
    toprint <- convert_var(toprint, ncol(A), variables)
  }
  else{
    latexA <- mat2latex(A, tolatex = FALSE)
    latexVar <- var2latex(ncol(A), bracket, variables)
    latexB <- mat2latex(B, tolatex = FALSE)
    toprint <- c(latexA, latexVar, "=", latexB)
  }

  if (tolatex) toprint <- paste0(c("$$\n",toprint,"\n$$"), collapse = "")

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
  else return(invisible(toprint))

}
