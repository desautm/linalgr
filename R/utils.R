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
                 style = c("inline", "frac", "sfrac", "tfrac", "decimal"),
                 digits = 2,
                 tol = sqrt(.Machine$double.eps)){

  if (missing(style)  &&  missing(digits)) style = "frac"
  else if (missing(style)  &&  !missing(digits)) style = "decimal"
  else style = match.arg(style, c("inline", "frac", "sfrac", "tfrac", "decimal"))

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
          else if (style == "tfrac"){
            out[i] <- paste(c("\\tfrac{",n[i],"}{",d[i],"}"), collapse = "")
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

# Permet d'afficher un nombre precis de nombre a droite de la virgule
specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall=k))

# Nombre de chiffres apres la virgule a afficher
ndigits <- function(A, zap = getOption("digits")){
  v <- zapsmall(abs(A-floor(A)), zap)
  if (any(v > 0)){
    dec <- max(nchar(v))-2
  }
  else dec <- 0
  return(dec)
}

# Retourne le vecteur permettant d'afficher une colonne de variables
var2latex <- function(col,
                      bracket,
                      variables){

  toprint <- paste("x_{",(1:col),"} \\\\", collapse = " \n", sep = "")

  begin <- "\\begin{array}{c} \n"
  end <- "\n\\end{array}"
  toprint <- paste0(c(begin, toprint, end), collapse = "")

  if (bracket == "crochet"){
    bra <- "\\left[\n"
    ket <- "\n\\right]"
  }
  else if (bracket == "parenthese"){
    bra <- "\\left(\n"
    ket <- "\n\\right)"
  }
  else{
    bra <- "\\left|\n"
    ket <- "\n\\right|"
  }
  toprint <- paste0(c(bra, toprint, ket), collapse = "")

  toprint <- convert_var(toprint, col, variables)

  return(toprint)

}

# Convertit les variables x_{i} à a, b, c, ... ou alors w, x, y et z
convert_var <- function(string,
                        col,
                        variables){

  toprint <- string
  if (variables == "a"){
    toprint <- gsub("x_\\{1\\}","a",toprint)
    toprint <- gsub("x_\\{2\\}","b",toprint)
    toprint <- gsub("x_\\{3\\}","c",toprint)
    toprint <- gsub("x_\\{4\\}","d",toprint)
    toprint <- gsub("x_\\{5\\}","e",toprint)
    toprint <- gsub("x_\\{6\\}","f",toprint)
    toprint <- gsub("x_\\{7\\}","g",toprint)
    toprint <- gsub("x_\\{8\\}","h",toprint)
    toprint <- gsub("x_\\{9\\}","i",toprint)
    toprint <- gsub("x_\\{10\\}","j",toprint)
    toprint <- gsub("x_\\{11\\}","k",toprint)
    toprint <- gsub("x_\\{12\\}","l",toprint)
    toprint <- gsub("x_\\{13\\}","m",toprint)
    toprint <- gsub("x_\\{14\\}","n",toprint)
    toprint <- gsub("x_\\{15\\}","o",toprint)
    toprint <- gsub("x_\\{16\\}","p",toprint)
    toprint <- gsub("x_\\{17\\}","q",toprint)
    toprint <- gsub("x_\\{18\\}","r",toprint)
    toprint <- gsub("x_\\{19\\}","s",toprint)
    toprint <- gsub("x_\\{20\\}","t",toprint)
    toprint <- gsub("x_\\{21\\}","u",toprint)
    toprint <- gsub("x_\\{22\\}","v",toprint)
    toprint <- gsub("x_\\{23\\}","w",toprint)
    toprint <- gsub("x_\\{24\\}","x",toprint)
    toprint <- gsub("x_\\{25\\}","y",toprint)
    toprint <- gsub("x_\\{26\\}","z",toprint)
  }
  else if (variables == "x"){
    if (col < 4){
      toprint <- gsub("x_\\{1\\}","x",toprint)
      toprint <- gsub("x_\\{2\\}","y",toprint)
      toprint <- gsub("x_\\{3\\}","z",toprint)
    }
    else{
      toprint <- gsub("x_\\{1\\}","w",toprint)
      toprint <- gsub("x_\\{2\\}","x",toprint)
      toprint <- gsub("x_\\{3\\}","y",toprint)
      toprint <- gsub("x_\\{4\\}","z",toprint)
    }
  }
  return(toprint)
}

# Fonction permettant de nettoyer le SEL
sanitize <- function(string, concise){

  toprint <- string

  # +- devient -
  toprint <- gsub("[+]\\s&\\s[-]", "- & ", toprint)

  # +-1 x_{i} devient +-x_{i}
  toprint <- gsub("(\\s|^|[-])1\\sx_\\{(\\d+)\\}", "\\1x_{\\2}", toprint)

  # + & 0 x_{i} devient &
  toprint <- gsub("[+]\\s&\\s0\\sx_\\{(\\d+)\\}", "&", toprint)

  # 0 x_{i} au debut de la ligne devient ""
  toprint <- gsub("^0\\sx_\\{(\\d+)\\}\\s", "", toprint)

  # Enleve un + si le premier de la ligne
  toprint <- gsub("^((&\\s)+)[+]\\s", "\\1", toprint)

  # On veut enlever les - & a x_{i} pour les remplacer par -a x_{i}
  toprint <- gsub("^((&\\s)+|^)[-]\\s&\\s", "\\1 & -", toprint)

  if (concise){
    # Mettre un code pour enlever les "&" excedentaires et avoir un SEL plus petit a afficher
    toprint <- gsub("&(\\s&)*\\s", "& ", toprint) # Enleve les & excedentaires
    toprint <- gsub("^(\\s)*&(\\s)*", "", toprint) # Enleve les espaces au debut de la ligne
    toprint <- gsub("^&\\s*", "", toprint)
  }

  return(toprint)
}

# Fonction permettant de creer une matrice tridiagonale
tridiag <- function(upper, lower, main){
  out <- matrix(0,length(main),length(main))
  diag(out) <- main
  indx <- seq.int(length(upper))
  out[cbind(indx+1,indx)] <- lower
  out[cbind(indx,indx+1)] <- upper
  return(out)
}

print_sel_oper <- function(A,
                           cas = c("interchange", "combinaison", "reduit", "reduit_entier")){

  cas <- match.arg(cas, c("interchange", "combinaison", "reduit", "reduit_entier"))

  toprint <- vector("character", length = nrow(A))
  tostyle <- mat2latex(A, verbose = FALSE, envir = FALSE)

  if (cas == "interchange"){
    for (i in (1:nrow(A))){
      if (sum(abs(A[i, ])) == 0) toprint[i] <- c("\\\\ \n")
      else{
        toprint[i] <- paste0(c("L_{",tostyle[i, 2],"} \\rightarrow L_{",tostyle[i, 4],"}\\\\ \n"),collapse = "")
      }
    }
  }
  else if (cas == "combinaison"){
    for (i in (1:nrow(A))){
      if (sum(abs(A[i, ])) == 0) toprint[i] <- c("\\\\ \n")
      else{
        toprint[i] <- paste0(tostyle[i, 1]," L_{",A[i,2],"} + ",tostyle[i, 3]," L_{",A[i,4],"} \\rightarrow L_{",A[i,2],"} \\\\ \n")
        toprint[i] <- gsub("[-]1\\s","- ",toprint[i]) # -1 devient -
        toprint[i] <- gsub("(\\s|^)1\\s"," ",toprint[i]) # 1 L_{} devient L_{}
        toprint[i] <- gsub("[+]\\s[-]","- ",toprint[i]) # + - devient -
      }
    }
  }
  else if (cas == "reduit"){
    for (i in (1:nrow(A))){
      if (sum(abs(A[i, ])) == 0 || tostyle[i, 1] == 1) toprint[i] <- c("\\\\ \n")
      else{
        toprint[i] <- paste0(tostyle[i, 1]," L_{",i,"} \\rightarrow L_{",i,"}\\\\ \n",collapse = "")
        toprint[i] <- gsub("[-]1\\s","- ",toprint[i]) # -1 devient -
        toprint[i] <- gsub("(\\s|^)1\\s"," ",toprint[i]) # 1 L_{} devient L_{}
      }
    }
  }

  begin <- c("\\begin{array}{l}\n")
  end <- c("\\end{array}\\\\\n \\\\\n")

  toprint <- paste0(c(begin, toprint, end), collapse = "")

  return(toprint)

}
