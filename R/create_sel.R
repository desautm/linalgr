#' @export
create_sel <- function(neq,
                       nvar,
                       type = c("unique", "aucune", "infinite"),
                       solution){

  if (missing(type)) type = "unique"
  else type <- match.arg(type, c("unique", "aucune", "infinite"))

  if (!missing(solution)){
    if (length(solution) != nvar) stop("Le nombre de solutions doit etre egal au nombre de variables.")
    if (type != "unique") stop("On peut specifier les solutions seulement dans le cas unique.")
  }

  champ <- -10:10

  if (type == "unique"){
    if (neq < nvar)
      stop("Pour une solution unique, il doit y avoir au moins autant d'equations que d'inconnues")
    A <- matrix(0, neq, nvar)
    B <- matrix(0, neq, 1)
    if (!missing(solution)) B[(1:length(solution))] <- solution
    else B[(1:nvar)] <- sample(champ,nvar)
    for (i in (1:nvar)) {
      A[i, i] <- 1
    }
  }
  else if (type == "aucune"){

  }
  else if (type ==  "infinite"){
    # Il doit y avoir moins de pivots que de variables
    A <- matrix(0, neq, nvar)
    B <- matrix(0, neq, 1)
    npivot <- sample(2:(nvar-1),1)
    ppivot <- sort(sample(2:nvar, npivot-1))
    ppivot <- c(1, ppivot)
    for (i in (1:neq)) {
      A[i, ppivot[i]] <- 1
    }
    id <- matrix(0,1,nvar)
    for (j in (1:nvar)){
      id[j] <- first_nonzero(A[, j])
    }
  }
  return(id)
  #return(inv_rref(cbind(A,B)))
}

# Permet de partir d'une forme echelonnee reduite et d'obtenir une matrice melangee pour donner en probleme
inv_rref <- function(A){

  row <- nrow(A)
  col <- ncol(A)

  for (i in (1:row)){
    id <- first_nonzero(A[i, ])
    if (id == 0) next
    if (i == 1) sequence <- 2:row
    else if (i == row) sequence <- 1:(row-1)
    else sequence <- c(1:(i-1),(i+1):row)
    for (j in sequence){
      fact <- sample(c(-5:-1,1:5),1)
      A[j, (id:col)] <- A[j, (id:col)]+fact*A[i, (id:col)]
    }
  }
  A <- A[sample(1:row), ] #On permute les lignes
  for (i in (1:row)){ #On simplifie, si possible, les lignes
    m <- numbers::mGCD(A[i, ])
    if (m != 0) A[i, ] <- A[i, ]/m
  }
  return(A)
}

# Permet de trouver le premier element non-nul d'un vecteur
first_nonzero <- function(v){
  id <- 0
  for (i in (1:length(v))){
    if (v[i] != 0){
      id <- i
      break
    }
  }
  return(id)
}
