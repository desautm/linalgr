frac <- function(x,
                 style = c("inline", "frac", "sfrac", "decimal"),
                 digits = 2,
                 tol = sqrt(.Machine$double.eps)){

  if (missing(style)) style = "frac"
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
