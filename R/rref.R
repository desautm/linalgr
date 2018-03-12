#' @export
rref <- function(A,
                 echelon = FALSE,
                 style = c("decimal", "inline", "frac", "sfrac", "tfrac", "dfrac"),
                 bracket = c("crochet", "parenthese", "determinant"),
                 verbose = TRUE,
                 copy2clip = FALSE,
                 tolatex = TRUE,
                 digits = 2,
                 tol = sqrt(.Machine$double.eps)){

  if (!is.matrix(A)) stop("A doit etre une matrice.")

  style <- match.arg(style, c("decimal", "inline", "frac", "sfrac", "tfrac", "dfrac"))
  bracket <- match.arg(bracket, c("crochet", "parenthese", "determinant"))

  row <- nrow(A)
  col <- ncol(A)

  oper <- matrix(0, row, 4)

  attr(A, "style") <- style
  #attr(oper, "style") <- style
  attr(A, "bracket") <- bracket
  attr(A, "verbose") <- FALSE
  #attr(oper, "verbose") <- FALSE
  attr(A, "digits") <- digits
  #attr(oper, "digits") <- digits

  toprint <- mat2latex(A, tolatex = FALSE, verbose = FALSE)

  i <- j <- 1
  while ((i <= row) && (j <= col)){
    # Trouver la valeur et l'indice du plus grand element dans le reste de la colonne j
    p <- max(abs(A[(i:row), j]))
    k <- which.max(abs(A[(i:row), j]))
    k <- k + i -1
    oper <- matrix(0, row, 4)
    if (p <= tol){
      # La colonne est negligable, on la transforme en zero
      A[(i:row), j] <- 0
      j <- j + 1
    }
    else{
      # On echange les lignes i et k
      if (i != k){
        oper[i, ] <- c(0, i, 0, k)
        oper[k, ] <- c(0, k, 0, i)
        #print_sel_oper(oper, cas =  "interchange")
        oper[i, ] <- 0
        oper[k, ] <- 0
        A[c(i, k), (j:col)] <- A[c(k, i), (j:col)]
        toprint <- paste0(toprint, mat2latex(A, tolatex = FALSE, verbose = FALSE), collapse = "")
      }
      # On divise la ligne du pivot par le pivot
      A[i, (j:col)] <- A[i, (j:col)]/A[i, j]
      toprint <- paste0(toprint, mat2latex(A, tolatex = FALSE, verbose = FALSE), collapse = "")
      # On soustrait des multiples de la ligne pivot des autres lignes
      # La variable sequence est une liste que nous devons traverser pour
      # parcourir toutes les lignes de la matrice
      # Nous devons separer le probleme de cette façon car R produit des listes
      # decroissantes par defaut.
      # Par exemple, 5:2 produit 5, 4, 3, 2 et j'ai besoin qu'elle produise NULL
      if (echelon){
        if (i == 1) sequence <- 2:row
        else if (i == row) sequence <- NULL
        else sequence <- (i+1):row
      }
      else{
        if (i == 1) sequence <- 2:row
        else if (i == row) sequence <- 1:(row-1)
        else sequence <- c(1:(i-1),(i+1):row)
      }
      for (k in sequence){
        oper[k, ] <- c(1, k, -A[k, j], i)
        A[k, (j:col)] <- A[k, (j:col)] - A[k, j]*A[i, (j:col)]
      }
      toprint <- paste0(toprint, mat2latex(A, tolatex = FALSE, verbose = FALSE), collapse = "")
      i <- i + 1
      j <- j + 1
    }
  }
  cat(toprint)

  attr(A, "style") <- NULL
  attr(A, "bracket") <- NULL
  attr(A, "verbose") <- NULL
  attr(A, "digits") <- NULL

  #return(A)
}

#' @export
echelon <- function(A,
                    tol = sqrt(.Machine$double.eps)){
  return(rref(A, echelon = TRUE))
}

#' @export
inverse <- function(A,
                    tol = sqrt(.Machine$double.eps)){

  row <- nrow(A)
  col <- ncol(A)

  if (row != col) stop("La matrice A doit etre carree.")

  inv <- rref(cbind(A,diag(row)))

  if (any(apply(abs(inv[,1:col]) <= sqrt(.Machine$double.eps), 1, all)))
    stop ("A est singuliere")

  return(inv[,(col+1):(2*col)])
}
