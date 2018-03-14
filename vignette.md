---
title: "Linear Algebra"
author: "Marc-AndrÃ© DÃ©sautels"
date: "15 fÃ©vrier 2018"
output:
  word_document:
    keep_md: yes
    toc: yes
  pdf_document:
    keep_tex: yes
    latex_engine: xelatex
  html_document:
    toc: yes
header-includes: \usepackage{xfrac}
---



# Utilisation de `R` pour produire du code LaTeX en algèbre linéaire

## Initialisation de la librairie

Nous devons installer la librairie. Si vous n'avez pas la librairie `devtools`, vous devez l'installer.


```r
install.packages("devtools")
```

Vous installer ensuite la librairie à l'aide de la commande suivante:

```r
devtools::install_github("desautm/linalgr")
```

Vous pouvez charger la librairie:

```r
library(linalgr)
```

## Affichage de matrices

Nous allons définir quelques matrices:

```r
m <- 5
n <- 5
A <- matrix(sample(-10:10, m*n, replace = TRUE), m, n)
B <- matrix(sample(-10:10, m, replace = TRUE), m, 1)
```

Voici l'affichage directement avec `R`:

```r
A
```

```
##      [,1] [,2] [,3] [,4] [,5]
## [1,]    1   -1    4    6    6
## [2,]   -5    3   10   -1   -3
## [3,]    5   -7    1    8    5
## [4,]   -3   -7    3   -3    0
## [5,]    1    2   -1    6    4
```

```r
B
```

```
##      [,1]
## [1,]    4
## [2,]   -2
## [3,]   -7
## [4,]    9
## [5,]    1
```

## Affichage avec la librairie `linalgr`

Pour afficher, il faut utiliser l'option `results = 'asis'` dans le bloc de code `R`.

Voici l'affichage en utilisant la librairie. 

```r
mat2latex(A)
mat2latex(B)
```

$$
\left[
\begin{array}{rrrrr}
1 & -1 & 4 & 6 & 6 \\ 
-5 & 3 & 10 & -1 & -3 \\ 
5 & -7 & 1 & 8 & 5 \\ 
-3 & -7 & 3 & -3 & 0 \\ 
1 & 2 & -1 & 6 & 4 \\ 
\end{array}
\right]
$$$$
\left[
\begin{array}{r}
4 \\ 
-2 \\ 
-7 \\ 
9 \\ 
1 \\ 
\end{array}
\right]
$$

# Affichage de SEL

## Affichage de systèmes d'équations linéaires


```r
sel2latex(A,B, variables = "xi")
```

$$
\begin{array}{rrrrrrrrrrr}
x_{1} & - & x_{2} & + & 4 x_{3} & + & 6 x_{4} & + & 6 x_{5} & = & 4 \\ 
-5 x_{1} & + & 3 x_{2} & + & 10 x_{3} & - & x_{4} & - & 3 x_{5} & = & -2 \\ 
5 x_{1} & - & 7 x_{2} & + & x_{3} & + & 8 x_{4} & + & 5 x_{5} & = & -7 \\ 
-3 x_{1} & - & 7 x_{2} & + & 3 x_{3} & - & 3 x_{4} & & & = & 9 \\ 
x_{1} & + & 2 x_{2} & - & x_{3} & + & 6 x_{4} & + & 4 x_{5} & = & 1 \\ 
\end{array}
$$

## Affichage de systèmes matriciels


```r
sel2latex(A,B, sel = FALSE, variables = "xi")
```

$$
\left[
\begin{array}{rrrrr}
1 & -1 & 4 & 6 & 6 \\ 
-5 & 3 & 10 & -1 & -3 \\ 
5 & -7 & 1 & 8 & 5 \\ 
-3 & -7 & 3 & -3 & 0 \\ 
1 & 2 & -1 & 6 & 4 \\ 
\end{array}
\right]\left[
\begin{array}{c} 
x_{1} \\ 
x_{2} \\ 
x_{3} \\ 
x_{4} \\ 
x_{5} \\
\end{array}
\right]=\left[
\begin{array}{r}
4 \\ 
-2 \\ 
-7 \\ 
9 \\ 
1 \\ 
\end{array}
\right]
$$

## Nous pouvons changer les variables utilisées


```r
sel2latex(A,B, variables = "a")
```

$$
\begin{array}{rrrrrrrrrrr}
a & - & b & + & 4 c & + & 6 d & + & 6 e & = & 4 \\ 
-5 a & + & 3 b & + & 10 c & - & d & - & 3 e & = & -2 \\ 
5 a & - & 7 b & + & c & + & 8 d & + & 5 e & = & -7 \\ 
-3 a & - & 7 b & + & 3 c & - & 3 d & & & = & 9 \\ 
a & + & 2 b & - & c & + & 6 d & + & 4 e & = & 1 \\ 
\end{array}
$$

ou alors...


```r
sel2latex(matrix(sample(-10:10,4),2,2),matrix(sample(-10:10,4),2,1), variables = "x")
```

$$
\begin{array}{rrrrr}
10 x & & & = & 5 \\ 
7 x & - & 3 y & = & -8 \\ 
\end{array}
$$

## Affichage avec des fractions décimales et contrôle du nombre de chiffres à droite de la virgule


```r
sel2latex(A/3,B, variables = "xi", digits = 3)
```

$$
\begin{array}{rrrrrrrrrrr}
0.333 x_{1} & - & 0.333 x_{2} & + & 1.333 x_{3} & + & 2.000 x_{4} & + & 2.000 x_{5} & = & 4 \\ 
-1.667 x_{1} & + & 1.000 x_{2} & + & 3.333 x_{3} & - & 0.333 x_{4} & - & 1.000 x_{5} & = & -2 \\ 
1.667 x_{1} & - & 2.333 x_{2} & + & 0.333 x_{3} & + & 2.667 x_{4} & + & 1.667 x_{5} & = & -7 \\ 
-1.000 x_{1} & - & 2.333 x_{2} & + & 1.000 x_{3} & - & 1.000 x_{4} & + & 0.000 x_{5} & = & 9 \\ 
0.333 x_{1} & + & 0.667 x_{2} & - & 0.333 x_{3} & + & 2.000 x_{4} & + & 1.333 x_{5} & = & 1 \\ 
\end{array}
$$

## Avec des fractions ordinaires

### Mode en ligne


```r
sel2latex(A/3,B, variables = "xi", style = "inline")
```

$$
\begin{array}{rrrrrrrrrrr}
1/3 x_{1} & - & 1/3 x_{2} & + & 4/3 x_{3} & + & 2 x_{4} & + & 2 x_{5} & = & 4 \\ 
-5/3 x_{1} & + & x_{2} & + & 10/3 x_{3} & - & 1/3 x_{4} & - & x_{5} & = & -2 \\ 
5/3 x_{1} & - & 7/3 x_{2} & + & 1/3 x_{3} & + & 8/3 x_{4} & + & 5/3 x_{5} & = & -7 \\ 
-x_{1} & - & 7/3 x_{2} & + & x_{3} & - & x_{4} & & & = & 9 \\ 
1/3 x_{1} & + & 2/3 x_{2} & - & 1/3 x_{3} & + & 2 x_{4} & + & 4/3 x_{5} & = & 1 \\ 
\end{array}
$$

### Mode commande frac


```r
sel2latex(A/3,B, variables = "xi", style = "frac")
```

$$
\begin{array}{rrrrrrrrrrr}
\frac{1}{3} x_{1} & - & \frac{1}{3} x_{2} & + & \frac{4}{3} x_{3} & + & 2 x_{4} & + & 2 x_{5} & = & 4 \\ 
-\frac{5}{3} x_{1} & + & x_{2} & + & \frac{10}{3} x_{3} & - & \frac{1}{3} x_{4} & - & x_{5} & = & -2 \\ 
\frac{5}{3} x_{1} & - & \frac{7}{3} x_{2} & + & \frac{1}{3} x_{3} & + & \frac{8}{3} x_{4} & + & \frac{5}{3} x_{5} & = & -7 \\ 
-x_{1} & - & \frac{7}{3} x_{2} & + & x_{3} & - & x_{4} & & & = & 9 \\ 
\frac{1}{3} x_{1} & + & \frac{2}{3} x_{2} & - & \frac{1}{3} x_{3} & + & 2 x_{4} & + & \frac{4}{3} x_{5} & = & 1 \\ 
\end{array}
$$

### Matrices creuses


```r
C <- matrix(c(-2, 2, 0, 0, 0, 0, 0, 0,
              3, 4, -5, 0, 0, 0, 0, 0,
              0, 3, 1, 0, 0, 0, 0, 0,
              0, 0, -1, -1, -1, 0, 0, 0,
              0, 0, 0, 3, 4, 7, 0, 0,
              0, 0, 0, 0, -6, 6, 1, 0,
              0, 0, 0, 0, 0, 1, 1, 1,
              0, 0, 0, 0, 0, 0, -1, 4), 8, 8, byrow = TRUE)
D <- matrix(c(1,2,3,4,5,6,7,8), 8, 1)
```

#### Option de base


```r
sel2latex(C, D, variables = "xi")
```

$$
\begin{array}{rrrrrrrrrrrrrrrrr}
-2 x_{1} & + & 2 x_{2} & & & & & & & & & & & & & = & 1 \\ 
3 x_{1} & + & 4 x_{2} & - & 5 x_{3} & & & & & & & & & & & = & 2 \\ 
& & 3 x_{2} & + & x_{3} & & & & & & & & & & & = & 3 \\ 
& & &  & -x_{3} & - & x_{4} & - & x_{5} & & & & & & & = & 4 \\ 
& & & & & & 3 x_{4} & + & 4 x_{5} & + & 7 x_{6} & & & & & = & 5 \\ 
& & & & & & &  & -6 x_{5} & + & 6 x_{6} & + & x_{7} & & & = & 6 \\ 
& & & & & & & & & & x_{6} & + & x_{7} & + & x_{8} & = & 7 \\ 
& & & & & & & & & & &  & -x_{7} & + & 4 x_{8} & = & 8 \\ 
\end{array}
$$

#### Option `concise`


```r
sel2latex(C, D, variables = "xi", concise = TRUE)
```

$$
\begin{array}{rrrrrrr}
& & -2 x_{1} & + & 2 x_{2} & = & 1 \\ 
3 x_{1} & + & 4 x_{2} & - & 5 x_{3} & = & 2 \\ 
& & 3 x_{2} & + & x_{3} & = & 3 \\ 
-x_{3} & - & x_{4} & - & x_{5} & = & 4 \\ 
3 x_{4} & + & 4 x_{5} & + & 7 x_{6} & = & 5 \\ 
-6 x_{5} & + & 6 x_{6} & + & x_{7} & = & 6 \\ 
x_{6} & + & x_{7} & + & x_{8} & = & 7 \\ 
& & -x_{7} & + & 4 x_{8} & = & 8 \\ 
\end{array}
$$

# Création de SEL

## Solution unique


```r
E <- create_sel(5,5, type = "unique")
sel2latex(E$A, E$B, variables = "xi")
```

$$
\begin{array}{rrrrrrrrrrr}
-x_{1} & - & 5 x_{2} & + & 14 x_{3} & + & 54 x_{4} & + & 1017 x_{5} & = & 2334 \\ 
-2 x_{1} & - & 14 x_{2} & + & 91 x_{3} & + & 470 x_{4} & + & 3630 x_{5} & = & 8268 \\ 
-5 x_{1} & - & 31 x_{2} & + & 190 x_{3} & + & 990 x_{4} & + & 7950 x_{5} & = & 18150 \\ 
-3 x_{1} & - & 17 x_{2} & + & 100 x_{3} & + & 527 x_{4} & + & 4339 x_{5} & = & 9926 \\ 
x_{1} & + & 6 x_{2} & - & 36 x_{3} & - & 188 x_{4} & - & 1532 x_{5} & = & -3500 \\ 
\end{array}
$$

## Aucune solution


```r
E <- create_sel(5,5, type = "aucune")
sel2latex(E$A, E$B, variables = "xi")
```

$$
\begin{array}{rrrrrrrrrrr}
x_{1} & + & 3 x_{2} & - & 5 x_{3} & + & 5 x_{4} & - & 7 x_{5} & = & -10 \\ 
-x_{1} & - & 3 x_{2} & + & 5 x_{3} & - & 5 x_{4} & + & 7 x_{5} & = & 10 \\ 
x_{1} & + & 3 x_{2} & - & 5 x_{3} & + & 5 x_{4} & - & 7 x_{5} & = & -10 \\ 
x_{1} & + & 3 x_{2} & - & 5 x_{3} & + & 5 x_{4} & - & 7 x_{5} & = & -10 \\ 
-3 x_{1} & - & 9 x_{2} & + & 15 x_{3} & - & 15 x_{4} & + & 21 x_{5} & = & 22 \\ 
\end{array}
$$

## Infinité de solutions


```r
E <- create_sel(5,5, type = "infinite")
sel2latex(E$A, E$B, variables = "xi")
```

$$
\begin{array}{rrrrrrrrrrr}
5 x_{1} & + & 43 x_{2} & + & 175 x_{3} & + & 53 x_{4} & + & 981 x_{5} & = & 430 \\ 
x_{1} & - & 7 x_{2} & - & 151 x_{3} & - & 485 x_{4} & - & 1197 x_{5} & = & -718 \\ 
x_{1} & + & 8 x_{2} & + & 44 x_{3} & + & 40 x_{4} & + & 288 x_{5} & = & 152 \\ 
x_{1} & + & 9 x_{2} & + & 54 x_{3} & + & 66 x_{4} & + & 360 x_{5} & = & 192 \\ 
2 x_{1} & + & 18 x_{2} & + & 107 x_{3} & + & 129 x_{4} & + & 711 x_{5} & = & 378 \\ 
\end{array}
$$

# Solution de SEL


```r
A <- matrix(c(1,2,3,4,5,6,7,8,9),3,3,byrow = TRUE)
B <- matrix(c(1,2,3),3,1)
```



# Essai

$$\left[
\begin{array}{rrr|r}
1 & 2 & 3 & 1 \\ 
4 & 5 & 6 & 2 \\ 
7 & 8 & 9 & 3 \\ 
\end{array}
\right]$$
