#' Affichage d'un systeme d'equations lineaires (SEL)
#'
sel <- function(A,
                B,
                sel = TRUE,
                variables = c("x","a","xi"),
                style = c("inline", "frac", "sfrac", "decimal"),
                digits = 2,
                bracket = c("crochet", "parenthese"),
                tol = sqrt(.Machine$double.eps)){

  ncol <- ncol(A)

  if (missing(variables)) variables = "xi"
  else variables <- match.arg(variables, c("a","x","xi"))

  if (variables == "x" && ncol > 4) stop("Trop grand nombre de variables pour ce cas.")
  if (variables == "a" && ncol > 26) stop("Trop grand nombre de variables pour ce cas.")

  if (missing(bracket)) bracket = "crochet"
  else bracket <- match.arg(bracket, c("parenthese", "crochet"))

  if (sel){
    cat(paste(c("\\begin{array}{",rep("c",2*ncol(A)+1),"}\n"), collapse = ""))
    for (i in (1:nrow(A))){
      for (j in (1:ncol(A))){
        if (A[i, j] > 0){
          if (j > 1) cat("+ & ")
          if (A[i, j] != 1) cat(frac(A[i, j], style = style))
        }
        else if (A[i, j] < 0){
          if (j > 1) cat("- & ")
          if (A[i, j] != -1) cat(frac(abs(A[i, j]), style = style))
          else cat("-")
        }
        else{
          cat(" & ")
        }
        if (variables == "xi") cat(c(" x_{",j,"} & "), sep = "")
        else if (variables == "x"){
          if (ncol == 3) sequence = c("x", "y", "z")
          else sequence = c("w", "x", "y", "z")
          cat(c(sequence[j]," & "), sep = "")
        }
        else if (variables == "a"){
          sequence = c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z")
          cat(c(sequence[j]," & "), sep = "")
        }
      }
      cat("= & ")
      for (j in (1:ncol(B))){
        cat(frac(B[i, j], style = style))
        if (j < ncol(B)) cat(" & ")
      }
      cat(" \\\\\n")
    }
    cat("\\end{array}\n")
  }
  else{
    mat(A)
    mat_var(ncol(A), variables = variables, bracket = bracket)
    cat("= \n")
    mat(B)
  }

}

#' Fonction permettant d'afficher une matrice colonne de variables pour l'affichage d'un SEL
mat_var <- function(ncol,
                variables = c("a","x","xi"),
                bracket = c("parenthese", "crochet")){

  if (missing(variables)) variables = "xi"
  else variables <- match.arg(variables, c("a","x","xi"))

  if (missing(bracket)) bracket = "crochet"
  else bracket <- match.arg(bracket, c("parenthese", "crochet"))

  if (variables == "x" && ncol > 4) stop("Trop grand nombre de variables pour ce cas.")
  if (variables == "a" && ncol > 26) stop("Trop grand nombre de variables pour ce cas.")

  if (bracket == "parenthese") cat('\\left(\n')
  else if (bracket == "crochet") cat('\\left[\n')

  cat("\\begin{array}{c}\n")

  for (i in (1:ncol)){
    if (variables == "xi") cat(paste(c("x_{",i,"} \\\\ \n"), collapse = ""))
    else if (variables == "x"){
      if (ncol == 3) sequence = c("x", "y", "z")
      else sequence = c("w", "x", "y", "z")
      cat(paste(sequence[i], "\\\\ \n"))
    }
    else if (variables == "a"){
      sequence = c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z")
      cat(paste(sequence[i], "\\\\ \n"))
    }
  }

  cat("\\end{array}\n")

  if (bracket == "parenthese") cat('\\right)\n')
  else if (bracket == "crochet") cat('\\right]\n')

}
