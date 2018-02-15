#' Affichage LaTeX d'une fraction
#'
#' \code{frac} affiche le code LaTeX sous forme de fraction de tous les nombres
#' presents dans \code{x}
#'
#' Cette fonction est surtout utile pour les autres fonctions de la librairie. Elle est appelee lorsque nous faisons
#' l'affichage de matrices a l'aide la fonction \code{mat}.
#'
#' @param x Une liste de nombre decimaux
#' @param style Un choix entre quatre facons de representer la fraction
#'              \code{"inline"}; permet d'ecrire la fraction sous la forme a/b
#'              \code{"frac"}; permet d'ecrire la fraction sous la forme frac\{a\}\{b\}
#'              \code{"sfrac"}; permet d'ecrire la fraction sous la forme sfrac\{a\}\{b\}
#'              \code{"decimal"}; permet d'ecrire la fraction sous la forme decimale 0.34...
#' @param digits Permet de choisir le nombre de chiffres significatifs a afficher.
#' @return out Une liste contenant la representation sous forme de fraction LaTeX de \code{x}
#' @export
#' @examples
#' frac(4)
#' frac(3/7)
#' frac(3/7, style = "sfrac")
#' frac(-3/7, style = "inline")
#' frac(3/7, digits = 2)
frac <- function(x,
                 style = c("inline", "frac", "sfrac", "decimal"),
                 digits = 2,
                 tol = sqrt(.Machine$double.eps)){

  if (missing(style)  &&  missing(digits)) style = "frac"
  else if (missing(style)  &&  !missing(digits)) style = "decimal"
  else style = match.arg(style, c("inline", "frac", "sfrac", "decimal"))

  # Numerateur et denominateur du nombre decimal x
  n <- fractional::numerators(x)
  d <- fractional::denominators(x)

  out <- x

  for (i in (1:length(n))){
    # Denominateur est egal a 1
    if (abs(d[i] - 1) <= tol){
      if (style != "decimal") out[i] <- n[i]
      else out[i] <- specify_decimal(x[i], digits)
    }
    else{
      # Numerateur est egal a 0
      if (abs(n[i]) <= tol){
        if (style != "decimal") out[i] <- 0
        else out[i] <- specify_decimal(x[i], digits)
      }
      # Numerateur est different de 0
      else{
        # Numerateur est positif
        if (n[i] > 0){
          if (style == "inline"){
            out[i] <- paste(c(n[i],"/",d[i]), collapse = "")
          }
          else if (style == "frac"){
            out[i] <- paste(c("\\frac{",n[i],"}{",d[i],"}"), collapse = "")
          }
          else if (style == "sfrac"){
            out[i] <- paste(c("\\sfrac{",n[i],"}{",d[i],"}"), collapse = "")
          }
          else if (style == "decimal"){
            out[i] <- format(specify_decimal(x[i], digits), nsmall = digits)
          }
        }
        # Numerateur est negatif
        else{
          if (style == "inline"){
            out[i] <- paste(c(n[i],"/",d[i]), collapse = "")
          }
          else if (style == "frac"){
            out[i] <- paste(c("-\\frac{",abs(n[i]),"}{",d[i],"}"), collapse = "")
          }
          else if (style == "sfrac"){
            out[i] <- paste(c("-\\sfrac{",abs(n[i]),"}{",d[i],"}"), collapse = "")
          }
          else if (style == "decimal"){
            out[i] <- format(specify_decimal(x[i], digits), nsmall = digits)
          }
        }
      }
    }
  }
  return(out)

}

specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall=k))

#' @export
to_latex <- function(str){
  cat("$$\n")
  cat(str)
  cat("$$")
  return(NULL)
}
