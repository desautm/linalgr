rref <- function(A,
                 echelon = FALSE,
                 tol = sqrt(.Machine$double.eps)){
  if (!is.matrix(A)) stop("A doit etre une matrice.")
  row <- nrow(A)
  col <- ncol(A)

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
      A[c(i, k), (j:col)] <- A[c(k, i), (j:col)]
      # On divise la ligne du pivot par le pivot
      A[i, (j:col)] <- A[i, (j:col)]/A[i, j]
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
        A[k, (j:col)] <- A[k, (j:col)] - A[k, j]*A[i, (j:col)]
      }
      i <- i + 1
      j <- j + 1
    }
  }
  return(A)
}

echelon <- function(A,
                    tol = sqrt(.Machine$double.eps)){
  return(rref(A, echelon = TRUE))
}

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
