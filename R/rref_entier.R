#' @export
rref_entier <- function(A,
                        B = NULL,
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
  attr(oper, "style") <- style
  attr(A, "bracket") <- bracket
  attr(A, "verbose") <- FALSE
  attr(oper, "verbose") <- FALSE
  attr(A, "digits") <- digits
  attr(oper, "digits") <- digits

  if (!is.null(B)){
    attr(B, "style") <- style
    attr(B, "bracket") <- bracket
    attr(B, "verbose") <- FALSE
    attr(B, "digits") <- digits
  }

  toprint <- mat2latex(A, B, tolatex = FALSE, verbose = FALSE)

  i <- j <- 1
  while ((i <= row) && (j <= col)){
    # Trouver la valeur et l'indice du plus grand element dans le reste de la colonne j
    p <- max(abs(A[(i:row), j]))
    k <- which.max(abs(A[(i:row), j]))
    k <- k + i -1
    if (p <= tol){
      # La colonne est negligable, on la transforme en zero
      A[(i:row), j] <- 0
      j <- j + 1
    }
    else{
      # On echange les lignes i et k
      if (abs(A[i, j]) < tol){
        oper[i, ] <- c(0, i, 0, k)
        oper[k, ] <- c(0, k, 0, i)
        toprint <- paste0(toprint, print_sel_oper(oper, cas =  "interchange"))
        oper[i, ] <- c(0, 0, 0, 0)
        oper[k, ] <- c(0, 0, 0, 0)
        A[c(i, k), (j:col)] <- A[c(k, i), (j:col)]
        B[c(i, k)] <- B[c(k, i)]
        toprint <- paste0(toprint, mat2latex(A, B, tolatex = FALSE, verbose = FALSE), collapse = "")
      }

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
        lowestCM <- numbers::LCM(A[i, j], A[k, j])
        M1 <- abs(lowestCM/A[i, j])
        M2 <- abs(lowestCM/A[k, j])
        if (lowestCM < 0){
          oper[k, ] <- c(M2, k, M1, i)
          B[k, ] <- M2*B[k, ] + M1*B[i, ]
          A[k, ] <- M2*A[k, ] + M1*A[i, ]
        }
        else if (lowestCM > 0){
          oper[k, ] <- c(M2, k, -M1, i)
          B[k, ] <- M2*B[k, ] - M1*B[i, ]
          A[k, ] <- M2*A[k, ] - M1*A[i, ]
        }
      }
      if (sum(abs(oper)) != 0){
        toprint <- paste0(toprint, print_sel_oper(oper, cas =  "combinaison"))
        toprint <- paste0(toprint, mat2latex(A, B, tolatex = FALSE, verbose = FALSE), collapse = "")
        oper[(1:row), ] <- 0
      }
      i <- i + 1
      j <- j + 1
    }
  }
  if (!echelon){
    for (i in (1:row)){
      if (any(abs(A[i, ]) >= tol )){
        id <- which(abs(A[i, ]) >= tol)
        oper[i, 1] <- 1/A[i, id[1]]
        B[i, ] <- B[i, ]/A[i, id[1]]
        A[i, ] <- A[i, ]/A[i, id[1]]
      }
    }
    if (sum(abs(oper)) != 0){
      toprint <- paste0(toprint, print_sel_oper(oper, cas =  "reduit"))
      toprint <- paste0(toprint, mat2latex(A, B, tolatex = FALSE, verbose = FALSE), collapse = "")
      oper[i, 1] <- 0
    }
  }

  toprint <- paste0(toprint, "\n", collapse = "")

  attr(A, "style") <- NULL
  attr(A, "bracket") <- NULL
  attr(A, "verbose") <- NULL
  attr(A, "digits") <- NULL
  attr(B, "style") <- NULL
  attr(B, "bracket") <- NULL
  attr(B, "verbose") <- NULL
  attr(B, "digits") <- NULL
  attr(oper, "style") <- NULL
  attr(oper, "verbose") <- NULL
  attr(oper, "digits") <- NULL

  begin <- c("\\begin{array}{l}\n")
  end <- c("\\end{array}")
  toprint <- paste0(begin,toprint,end, collapse = "")
  toprint <- paste0("$$\n",toprint,"\n$$", collapse = "")

  if (copy2clip){
    test2clip <- writeClipboard(toprint)
    if (test2clip) message("La matrice a ete correctement copiee dans le presse-papier.")
    else message("La matrice n'a pas ete copiee dans le presse-papier.")
  }

  if (verbose){
    cat(toprint)
    return(invisible(toprint))
  }
  else{
    newlist <- list("A" = A, "B" = B)
    return(newlist)
  }
}

#' @export
echelon <- function(A,
                    ...){
  return(rref(A, ..., echelon = TRUE))
}

#' @export
inverse <- function(A,
                    ...,
                    tol = sqrt(.Machine$double.eps)){

  row <- nrow(A)
  col <- ncol(A)

  if (row != col) stop("La matrice A doit etre carree.")

  inv <- rref(A,diag(row), ..., verbose = FALSE)

  if (any(apply(abs(inv$A) <= sqrt(.Machine$double.eps), 1, all)))
    stop ("A est singuliere")

  return(mat2latex(inv$B, ...))
}
