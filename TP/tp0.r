#================== Exercice 1 =====================#
r <- c(4, 5, 6)
A <- matrix(c(r, r, r), nrow = 3, ncol = 3, byrow = TRUE)
B <- rbind(A, r)
dimnames(B) <- NULL # enlever les noms des dimensions
dims <- dim(B) # [4 3]

print(A)
print(B)

#================== Exercice 2 =====================#
set.seed(10)

x <- runif(10)

x[2] # Deuxième élément de x
x[c(2, 4)] # deuxième et quatrième élément de x
x[-1] # Tous les éléments de x sauf le premier

M <- matrix(runif(16), 4, 4)

M[1, 3] # Elément dans la position 1, 3
M[,1]   # première colonne de M
M[2,]   # deuxième ligne de M

L = list(x, M, "Hello")

L[[2]] # Deuxième élément de la liste

#================== Exercice 3 =====================#
x <- -10:10

x^2   # mettre aux carré tous les éléments de x
x + 3 # rajoute 3 aux tous les éléments de x
2 * x # multiplie tous les éléments de x par 2

#================== Exercice 4 =====================#
x == 0 # Renvoie un tableau logical dont vrai veut
       # dire que l'élément correspendant est égal à 0
x > 0  # Renvoie un tableau logical dont vrai indique
       # les positions ou x_i est supérieur à 0
x[x > 0] # Extraire les éléments de x qui sont plus grands que 0
x[x >= 0] # Extraire les éléments de x qui sont plus grans où égal à 0


#================== Exercice 5 =====================#
B %*% A
# A %*% B # Non-conformable arguments, on peut pas multiplier une matrice 3 x 3 par une matrice 4 x 3...
A %*% t(B)

#================== Exercice 6 =====================#
# 1.
d_x0 <- dnorm(0)   # 0.3989423
p_xlt0 <- pnorm(0) # P(X <= 0) = 0.5

#================== Exercice 7 =====================#
library(tidyverse)

x <- seq(0, 1, 0.01)
y <- x * (1 - x)

df <- tibble(x, y)
df |> ggplot(aes(x, y)) + geom_line()

imax <- which.max(y)

x_maximizer <- x[imax] # x* = 0.5
y_max <- y[imax]       # f(x*) = 0.25

g_x <- x^2 * (1 - x)

df |>
    mutate(g = g_x) |>
    pivot_longer(
        c(y, g),
        names_to = "curve",
        values_to = "y"
    ) |>
    ggplot(aes(x, y)) +
    scale_color_hue(labels = c("g(x)", "f(x)")) +
    geom_line(aes(color = curve)) +
    labs(col = "Courbe")

#================== Exercice 8 =====================#
x <- -10:10

sum(x == 0) # 1, cela compte le nombre d'éléments de x qui sont égaux à 0
sum(x > 0) # pareil, cette fois-ci les éléments qui sont plus grand  que zero
sort(x^2) # renvoyer un tableau des éléments de x en ordre croissant
table(x^2) # renvoyer une "table" dont la première ligne est les éléments uniques
           # de x^2 et dont la deuxième ligne est le nombre de fois que la valeur du
           # même colonne aparraissent
unique(x^2) # Renvoi la première ligne de table(x^2), dans l'ordre décroissant

#================== Exercice 9 =====================#
ma_somme <- function(x) {

    acc <- 0
    n <- length(x)

    for (i in seq_len(n)) {
        acc <- acc + x[i]
    }

    acc
}

ma_variance <- function(x) {

    n <- length(x)
    my_sum <- ma_somme(x)

    x_bar <- (1 / n) * my_sum
    dev_sq <- (x - x_bar)^2

    ma_somme(dev_sq) / (n - 1)
}

x <- runif(1000)

v  <- var(x)
mv <- ma_variance(x)

abs(v - mv) < 1E-14

#================== Exercice 9 =====================#
compte <- function(char_seq, lettre) {
    ma_somme(char_seq == lettre)
}

composition <- function(char_seq) {

    comp <- vector("numeric", 4)
    lettres <- c("a", "c", "g", "t")
    i <- 1

    for (let in lettres) {
        comp[i] <- compte(char_seq, let)
        i <- i + 1
    }

    comp
}

bio_sequence <- c("a", "a", "t", "g", "a", "g", "c", "t", "a", "g", "c", "t", "g")

comp <- composition(bio_sequence)