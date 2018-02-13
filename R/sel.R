#' Affichage d'un systeme d'equations lineaires (SEL)
#' @export
print_sel <- function(A,
                B,
                sel = TRUE,
                variables = c("x","a","xi"),
                style = c("inline", "frac", "sfrac", "decimal"),
                digits = 2,
                concise = FALSE,
                bracket = c("crochet", "parenthese"),
                tol = sqrt(.Machine$double.eps)){

  col <- ncol(A)

  if (missing(variables)) variables = "xi"
  else variables <- match.arg(variables, c("a","x","xi"))

  if (variables == "x" && col > 4) stop("Trop grand nombre de variables pour ce cas.")
  if (variables == "a" && col > 26) stop("Trop grand nombre de variables pour ce cas.")

  if (missing(bracket)) bracket = "crochet"
  else bracket <- match.arg(bracket, c("parenthese", "crochet"))

  if (missing(style)  &&  missing(digits)) style = "frac"
  else if (missing(style)  &&  !missing(digits)) style = "decimal"
  else style = match.arg(style, c("inline", "frac", "sfrac", "decimal"))

  if (variables == "x"){
    if (col == 3) sequence = c("x", "y", "z")
    else sequence = c("w", "x", "y", "z")
  }
  else if (variables == "a"){
    sequence = c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z")
  }

  if (sel){
    if (concise){
      cat(paste(c("\\begin{array}{",rep("c",2*strip_zeros(A)+2),"}\n"), collapse = ""))
    }
    else{
      cat(paste(c("\\begin{array}{",rep("c",2*ncol(A)+2),"}\n"), collapse = ""))
      for (i in (1:nrow(A))){
        id <- which.max(abs(A[i,])>0)
        for (j in (1:ncol(A))){
          if (id == j){
            if (A[i, j] == 0) {
              cat(" & ")
            }
            else if (A[i, j] == 1){
              if (variables == "x" || variables == "a") cat(c(" & ",sequence[j]," "))
              else cat(" & x_{",j,"} ", sep ="")
            }
            else if (A[i, j] == -1){
              if (variables == "x" || variables == "a") cat(c(" & -",sequence[j]," "))
              else cat(" & -x_{",j,"} ", sep = "")
            }
            else{
              if (variables == "x" || variables == "a") cat(c(" & ",frac(A[i, j]),sequence[j]," "), sep = "")
              else {
                cat(" & ")
                cat(frac(A[i, j], style = style, digits = digits))
                cat(c("x_{",j,"}"), sep = "")
              }
            }
          }
          else{
            if (A[i, j] < 0){
              if (A[i, j] != -1){
                if (variables == "x" || variables == "a") cat(c(" & - & ",frac(abs(A[i, j])),sequence[j]," "), sep = "")
                else{
                  cat(" & - & ")
                  cat(frac(abs(A[i, j]), style = style, digits = digits))
                  cat(c("x_{",j,"}"), sep = "")
                }
              }
              else{
                if (variables == "x" || variables == "a") cat(c(" & - & ",sequence[j]," "), sep = "")
                else cat(c(" & - & x_{",j,"} "), sep = "")
              }
            }
            else if (A[i, j] > 0){
              if (A[i, j] != 1){
                if (variables == "x" || variables == "a") cat(c(" & + & ",frac(abs(A[i, j])),sequence[j]," "), sep = "")
                else{
                  cat(" & + & ")
                  cat(frac(abs(A[i, j]), style = style, digits = digits))
                  cat(c("x_{",j,"}"), sep = "")
                }
              }
              else{
                if (variables == "x" || variables == "a") cat(c(" & + & ",sequence[j]," "), sep = "")
                else cat(c(" & + & x_{",j,"} "), sep = "")
              }
            }
            else{
              cat("& &")
            }
          }
        }
        cat("& = &")
        cat(B[i,1])
        cat("\\\\\n")
      }
    }
    cat("\\end{array}\n")
  }
  else{
    mat(A, style = style, bracket = bracket, digits = digits)
    print_mat_var(ncol(A), variables = variables, bracket = bracket)
    cat("= \n")
    mat(B, style = style, bracket = bracket, digits = digits)
  }
}

#' Fonction permettant d'afficher une matrice colonne de variables pour l'affichage d'un SEL
print_mat_var <- function(ncol,
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

# Cette fonction permet de compter le nombre minimal de colonnes pour representer le SEL de A
# Si la matrice est creuse, on peut esperer afficher la matrice en prenant moins d'espace
strip_zeros <- function(A){
  row <- nrow(A)
  out <- matrix(0, row, 1)
  for (i in (1:row)){
    out[i] <- sum(abs(A[i, ]) > 0)
  }
  return(max(out))
}


