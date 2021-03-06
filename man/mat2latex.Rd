% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mat2latex.R
\name{mat2latex}
\alias{mat2latex}
\title{Affichage LaTeX d'une matrice}
\usage{
mat2latex(A, B = NULL, style = c("decimal", "inline", "frac", "sfrac",
  "tfrac", "dfrac"), bracket = c("crochet", "parenthese", "determinant"),
  verbose = TRUE, copy2clip = FALSE, envir = TRUE, tolatex = TRUE,
  digits = 2)
}
\arguments{
\item{A}{une matrice}

\item{B}{une matrice (facultatif)}

\item{style}{permet de choisir la facon d'afficher les elements de la matrice. Les choix possibles sont \code{inline}, \code{frac},
\code{dfrac}, \code{sfrac} et \code{tfrac}. Par defaut, nous affichons des entiers ou des nombres decimaux.}

\item{bracket}{permet de choisir comment encadrer la matrice. Les choix sont des \code{crochet}s, des \code{parenthese}s ou un \code{determinant}.
Par defaut, nous encadrons entre \code{crochet}s.}

\item{verbose}{Si \code{TRUE} nous affichons le code LaTeX, si \code{FALSE} nous retournons le vecteur de caracteres. \code{TRUE} par defaut.}

\item{copy2clip}{Si \code{TRUE} nous copions le resultat dans le presse papier. Par defaut \code{FALSE}.}

\item{envir}{Si \code{TRUE}, nous encadrons le resultat dans un array et un bracket. Si \code{FALSE}, on ne retourne que la matrice. Utile
pour la fonction \code{sel2latex}.}

\item{tolatex}{Si \code{TRUE} nous encadrons le resultat par "$$ resultat $$".}

\item{digits}{Le nombre de chiffres a droite de la virgule a afficher. Nous affichons 2 chiffres par defaut.}
}
\value{
Le vecteur de caracteres contenant le code LaTeX de la matrice
}
\description{
\code{mat2latex} retourne un vecteur de caracteres qui contient le code LaTeX permettant d'ecrire une matrice \code{A}.
Si nous ajoutons une deuxieme matrice \code{B}, nous avons l'affichage de la matrice augmentee avec une barre verticale
separant les deux matrices. Nous pouvons choisir la facon d'afficher la matrice. Nous pouvons afficher des nombres entiers ou alors
choisir le nombre de chiffres a droite de la virgule. Nous pouvons afficher le resultat sous forme de fraction. Nous pouvons
afficher la fraction \code{inline} ou alors utiliser \code{frac}, \code{dfrac}, \code{sfrac} et \code{tfrac}. Le parametre \code{verbose}
permet d'afficher ou non la matrice. Le parametre \code{copy2clip} permet de copier ou non le resultat dans le presse-papier.
}
\examples{
A <- matrix(c(2, -4, 5,
              -3, 5, 7,
              10, 0, -1), 3, 3)
B <- matrix(c(0, -1, 5), 3, 1)
mat2latex(A)
mat2latex(A, bracket = "parenthese")
mat2latex(A,B)
mat2latex(A/3)
mat2latex(A/3, digits = 4)
mat2latex(A/3, style = "inline")
mat2latex(A/3, style = "frac")
}
