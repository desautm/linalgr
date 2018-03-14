#' @export
create_sel <- function(neq,
                       nvar,
                       type = c("unique", "aucune", "infinite"),
                       solution,
                       intervalle = -10:10,
                       npivots){

  sel_rref <- create_rref(neq, nvar, type, solution, intervalle, npivots)
  sel <- inverse_rref(sel_rref$A, sel_rref$B)

  return(sel)
}


# Creation d'un SEL sous forme echelonnee reduite
create_rref <- function(neq = 3,
                        nvar = 3,
                        type = c("unique", "aucune", "infinite"),
                        solution,
                        intervalle,
                        npivots){

  type <- match.arg(type, c("unique", "aucune", "infinite"))

  if (!missing(solution)){
    if (!(type == "unique")) stop("On peut donner une solution seulement si la solution est unique.")
    if (!is.numeric(solution)) stop("La solution doit etre numerique.")
    if (!is.matrix(solution)) solution <- as.matrix(solution)
    if (ncol(solution) > 1) stop("La solution doit n'avoir qu'une seule colonne.")
    if (nrow(solution) > neq) stop("La solution ne doit pas avoir plus de colonnes que le nombre d'equations.")
  }

  A <- matrix(0, neq, nvar)
  B <- matrix(0, neq, 1)

  if (type == "aucune"){
    # Si aucune solution, on part avec une solution unique ou une infinite
    # de solutions et on ajoute une ligne absurde
    choix <- sample(1:2, 1)
  }
  else choix <- 0

  if ((type == "unique") || ((type == "aucune") && (choix == 1))){
    if (neq < nvar)
      stop("Pour une solution unique, il doit y avoir au moins autant d'equations que d'inconnues")
    if (!missing(solution)){
      if (nrow(solution) > nvar) stop("La solution est trop longue.")
      B[(1:nrow(solution))] <- solution
    }
    else B[1:nvar] <- matrix(sample(intervalle, nvar, replace = TRUE), nvar, 1)
    for (i in (1:nvar)) A[i, i] <- 1
  }
  else if ((type == "infinite") || ((type == "aucune") && (choix == 2))){
    if (!missing(npivots)){
      if (npivots <= 0) stop("Le nombre de pivots doit etre un entier superieur a 0.")
      if (neq >= nvar){
        if (npivots >= nvar) stop("Le nombre de pivots doit etre plus petit que le nombre de variables.")
      }
      else if (neq < nvar){
        if (npivots > neq) stop("Le nombre de pivots doit etre plus petit ou egal au nombre d'equations.")
      }
    }
    else{
      if (neq >= nvar){
        npivots <- sample(1:(nvar-1), 1)
      }
      else if (neq < nvar){
        npivots <- neq
      }
    }
    # On decide la position des pivots mais on veut un pivot a la position (1,1)
    ppivots <-  sort(sample(2:nvar, npivots-1))
    ppivots <- c(1, ppivots)
    for (i in (1:npivots)){
      A[i, ppivots[i]:nvar] <- sample(intervalle, nvar-ppivots[i]+1, replace = TRUE)
      A[ , ppivots[i]] <- 0
      A[i, ppivots[i]] <- 1
    }
    B[1:npivots] <- sample(intervalle, npivots, replace = TRUE)
  }
  if (type == "aucune"){
    # npivots <- sample(1:min(neq, nvar), 1)
    # ppivots <-  sort(sample(2:min(neq, nvar), npivots-1))
    # ppivots <- c(1, ppivots)
    # for (i in (1:npivots)){
    #   A[i, ppivots[i]:nvar] <- sample(intervalle, nvar-ppivots[i]+1, replace = TRUE)
    #   A[ , ppivots[i]] <- 0
    #   A[i, ppivots[i]] <- 1
    # }
    # B[1:npivots] <- sample(intervalle, npivots, replace = TRUE)
    A[neq, ] <- 0
    B[neq] <- sample(c(-max(intervalle):-1, 1:max(intervalle)),1)
  }
  newList <- list("A" = A, "B" = B)
  return(newList)
}

inverse_rref <- function(A,
                         B,
                         intervalle = 5,
                         tol = sqrt(.Machine$double.eps)){

  A <- cbind(A, B)
  row <- nrow(A)
  col <- ncol(A)

  i <- row
  j <- col - 1

  while ((i >= 1) && (j >= 1)){
    if (sum(abs(A[, j])) == 1){
      m <- which.max(A[, j])
      if (m == 1) sequence <- 2:row
      else if (m == row) sequence <- 1:(row-1)
      else sequence <- c(1:(m-1),(m+1):row)
      for (k in sequence){
        A[k, (j:col)] <- A[k, (j:col)] + sample(c(-intervalle:-1,1:intervalle), 1)*A[m, (j:col)]
        #A[k, (j:col)] <- sample(c(-intervalle:-1,1:intervalle), 1)*A[k, (j:col)]
      }
      i <- i - 1
      j <- j - 1
    }
    else j <- j - 1
  }
  for (i in (1:row)){ #On simplifie, si possible, les lignes
    m <- numbers::mGCD(A[i, ])
    if (m != 0) A[i, ] <- A[i, ]/m
  }

  A <- A[sample(1:row), ]

  newList <- list("A" = A[, 1:(col-1)], "B" = as.matrix(A[, col]))
  return(newList)
}
