#' Affichage LaTeX d'une fraction
#'
#' \code{frac} affiche le code LaTeX sous forme de fraction de tous les nombres
#' présents dans \code{x}
#'
#' Cette fonction est surtout utile pour les autres fonctions de la librairie. Elle est appelée lorsque nous faisons
#' l'affichage de matrices à l'aide la fonction \code{mat}.
#'
#' @param x Une liste de nombre décimaux
#' @param style Un choix entre quatre façons de représenter la fraction
#'              \code{"inline"}; permet d'écrire la fraction sous la forme a/b
#'              \code{"frac"}; permet d'écrire la fraction sous la forme \frac{a}{b}
#'              \code{"sfrac"}; permet d'écrire la fraction sous la forme \sfrac{a}{b}
#'              \code{"decimal"}; permet d'écrire la fraction sous la forme décimale 0.34...
#' @return out Une liste contenant la représentation sous forme de fraction LaTeX de \code{x}
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

  if (!missing(digits)) options(digits = digits)

  # Numérateur et dénominateur du nombre décimal x
  n <- fractional::numerators(x)
  d <- fractional::denominators(x)

  out <- x

  for (i in (1:length(n))){
    # Dénominateur est égal à 1
    if (abs(d[i] - 1) <= tol){
      out[i] <- n[i]
    }
    else{
      # Numérateur est égal à 0
      if (abs(n[i]) <= tol){
        out[i] <- 0
      }
      # Numérateur est différent de 0
      else{
        # Numérateur est positif
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
        }
        # Numérateur est négatif
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
        }
      }
    }
  }

  return(out)

}
