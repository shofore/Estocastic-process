setwd("/Users/lucasrissetticabrera/Desktop/R_Estoc/Proyecto")

source("funciones.R")
#install.packages("markovchain")
library(markovchain)
#install.packages("DiagrammeR")
library(DiagrammeR)
library(igraph)

#_____________________P_3Cluster_____________________

transition_matrix1 <- matrix(c(
  0.413793, 0.586207, 0.00,
  0.226667, 0.693333, 0.08,
  0.000000, 0.500000, 0.50
), nrow=3, byrow=TRUE)

labels <- c("[-3-0.56)", "[0.56-1.94)", "[1.94-4)")

# Normalización y etiquetas
P <-transition_matrix1
rowSums(P)
P <- round(P, digits=6)
P <- t(apply(P, 1, function(row) row / sum(row)))
rownames(P) <- labels
colnames(P) <- labels
states <- labels
P
init1a <- c(1,0,0)
init2a <- c(0,1,0)
init3a <- c(0,0,1)
P1a <- init1a%*%transition_matrix1
P2a <- init2a%*%transition_matrix1
P3a <- init3a%*%transition_matrix1
Exp1a <- (-3+0.56)*P1a[1]/2+(0.56+1.94)*P1a[2]/2+(1.94+4)*P1a[3]/2
Exp2a <- (-3+0.56)*P2a[1]/2+(0.56+1.94)*P2a[2]/2+(1.94+4)*P2a[3]/2
Exp3a <- (-3+0.56)*P3a[1]/2+(0.56+1.94)*P3a[2]/2+(1.94+4)*P3a[3]/2

#_____________________P_4Cluster_____________________

transition_matrix2 <- matrix(c(
  0.416667, 0.375000, 0.208333, 0.000000,
  0.192308, 0.557692, 0.211538, 0.038462,
  0.133333, 0.500000, 0.266667, 0.100000,
  0.000000, 0.000000, 0.500000, 0.500000
), nrow=4, byrow=TRUE)

labels <- c("[-3-0.39)", "[0.39-1.16)", "[1.16-2.22)", "[2.22-4)")
init1b <- c(1,0,0,0)
init2b <- c(0,1,0,0)
init3b <- c(0,0,1,0)
init4b <- c(0,0,0,1)
P1b <- init1b%*%transition_matrix2
P2b <- init2b%*%transition_matrix2
P3b <- init3b%*%transition_matrix2
P4b <- init4b%*%transition_matrix2
Exp1b <- (-3+0.39)*P1b[1]/2+(0.39+1.16)*P1b[2]/2+(1.16+2.22)*P1c[3]/2+(2.22+4)*P1b[4]/2
Exp2b <- (-3+0.39)*P2b[1]/2+(0.39+1.16)*P2b[2]/2+(1.16+2.22)*P2c[3]/2+(2.22+4)*P2b[4]/2
Exp3b <- (-3+0.39)*P3b[1]/2+(0.39+1.16)*P3b[2]/2+(1.16+2.22)*P3c[3]/2+(2.22+4)*P3b[4]/2
Exp4b <- (-3+0.39)*P4b[1]/2+(0.39+1.16)*P4b[2]/2+(1.16+2.22)*P4c[3]/2+(2.22+4)*P4b[4]/2

# Normalización y etiquetas
P <-transition_matrix2
rowSums(P)
P <- round(P, digits=6)
P <- t(apply(P, 1, function(row) row / sum(row)))
rownames(P) <- labels
colnames(P) <- labels
states <- labels
P
init <- c(0,0,1,0)


#_____________________P_5Cluster_____________________

transition_matrix3 <- matrix(c(
  0.000000, 1.00, 0.00, 0.000000, 0.00,
  0.000000, 0.36, 0.44, 0.200000, 0.00,
  0.000000, 0.24, 0.50, 0.220000, 0.04,
  0.033333, 0.10, 0.50, 0.266667, 0.10,
  0.000000, 0.00, 0.00, 0.500000, 0.50
), nrow=5, byrow=TRUE)

labels <- c("[-3-1.18)", "[-1.18-0.46)", "[0.46-1.17)", "[1.17-2.22)", "[2.22-4)")
init1c <- c(1,0,0,0,0)
init2c <- c(0,1,0,0,0)
init3c <- c(0,0,1,0,0)
init4c <- c(0,0,0,1,0)
init5c <- c(0,0,0,0,1)
P1c <- init1c%*%transition_matrix3
P2c <- init2c%*%transition_matrix3
P3c <- init3c%*%transition_matrix3
P4c <- init4c%*%transition_matrix3
P5c <- init5c%*%transition_matrix3
Exp1c <- (-3+(-1.18))*P1c[1]/2+(-1.18+0.46)*P1c[2]/2+(0.46+1.17)*P1c[3]/2+(1.17+2.22)*P1c[4]/2+(2.22+4)*P1c[5]/2
Exp2c <- (-3+(-1.18))*P2c[1]/2+(-1.18+0.46)*P2c[2]/2+(0.46+1.17)*P2c[3]/2+(1.17+2.22)*P2c[4]/2+(2.22+4)*P2c[5]/2
Exp3c <- (-3+(-1.18))*P3c[1]/2+(-1.18+0.46)*P3c[2]/2+(0.46+1.17)*P3c[3]/2+(1.17+2.22)*P3c[4]/2+(2.22+4)*P3c[5]/2
Exp4c <- (-3+(-1.18))*P4c[1]/2+(-1.18+0.46)*P4c[2]/2+(0.46+1.17)*P4c[3]/2+(1.17+2.22)*P4c[4]/2+(2.22+4)*P4c[5]/2
Exp5c <- (-3+(-1.18))*P5c[1]/2+(-1.18+0.46)*P5c[2]/2+(0.46+1.17)*P5c[3]/2+(1.17+2.22)*P5c[4]/2+(2.22+4)*P5c[5]/2
Fmatrix <- diag(4)-matrix(c(
  0.000000, 1.00, 0.000000, 0.00,
  0.000000, 0.36, 0.200000, 0.00,
  0.033333, 0.10,  0.266667, 0.10,
  0.000000, 0.00,  0.500000, 0.50
), nrow=4, byrow=TRUE)
InvF <- solve(Fmatrix)
expected_steps <- rowSums(InvF)
est <- matrixpower(transition_matrix3,1000)
expected_inflation_lt <- (-3+(-1.18))*est[1,1]/2+(-1.18+0.46)*est[1,2]/2+(0.46+1.17)*est[1,3]/2+(1.17+2.22)*est[1,4]/2+(2.22+4)*est[1,5]/2
exp_anual_inf <- expected_inflation_lt*4
# Normalización y etiquetas
P <-transition_matrix3
rowSums(P)
P <- round(P, digits=6)
P <- t(apply(P, 1, function(row) row / sum(row)))
rownames(P) <- labels
colnames(P) <- labels
states <- labels
P
init <- c(0,0,1,0,0)

#_____________________P_6Cluster_____________________

transition_matrix4 <- matrix(c(
  0.000000, 1.000000, 0.000000, 0.000000, 0.000000, 0.000000,
  0.000000, 0.360000, 0.400000, 0.240000, 0.000000, 0.000000,
  0.000000, 0.244898, 0.489796, 0.224490, 0.040816, 0.000000,
  0.034483, 0.103448, 0.482759, 0.241379, 0.103448, 0.034483,
  0.000000, 0.000000, 0.250000, 0.375000, 0.125000, 0.250000,
  0.000000, 0.000000, 0.000000, 0.250000, 0.500000, 0.250000
), nrow=6, byrow=TRUE)

labels <- c("[-3-1.18)", "[-1.18-0.46)", "[0.46-1.15)", "[1.15-1.95)", "[1.95-2.98)", "[2.98-4)")
init1d <- c(1,0,0,0,0,0)
init2d <- c(0,1,0,0,0,0)
init3d <- c(0,0,1,0,0,0)
init4d <- c(0,0,0,1,0,0)
init5d <- c(0,0,0,0,1,0)
init6d <- c(0,0,0,0,0,1)
P1d <- init1d%*%transition_matrix4
P2d <- init2d%*%transition_matrix4
P3d <- init3d%*%transition_matrix4
P4d <- init4d%*%transition_matrix4
P5d <- init5d%*%transition_matrix4
P6d <- init6d%*%transition_matrix4
Exp1d <- (-3+(-1.18))*P1d[1]/2+(-1.18+0.46)*P1d[2]/2+(0.46+1.15)*P1d[3]/2+(1.15+1.95)*P1d[4]/2+(1.95+2.98)*P1d[5]/2+(2.98+4)*P1d[6]/2
Exp2d <- (-3+(-1.18))*P2d[1]/2+(-1.18+0.46)*P2d[2]/2+(0.46+1.15)*P2d[3]/2+(1.15+1.95)*P2d[4]/2+(1.95+2.98)*P2d[5]/2+(2.98+4)*P2d[6]/2
Exp3d <- (-3+(-1.18))*P3d[1]/2+(-1.18+0.46)*P3d[2]/2+(0.46+1.15)*P3d[3]/2+(1.15+1.95)*P3d[4]/2+(1.95+2.98)*P3d[5]/2+(2.98+4)*P3d[6]/2
Exp4d <- (-3+(-1.18))*P4d[1]/2+(-1.18+0.46)*P4d[2]/2+(0.46+1.15)*P4d[3]/2+(1.15+1.95)*P4d[4]/2+(1.95+2.98)*P4d[5]/2+(2.98+4)*P4d[6]/2
Exp5d <- (-3+(-1.18))*P5d[1]/2+(-1.18+0.46)*P5d[2]/2+(0.46+1.15)*P5d[3]/2+(1.15+1.95)*P5d[4]/2+(1.95+2.98)*P5d[5]/2+(2.98+4)*P6d[6]/2
Exp6d <- (-3+(-1.18))*P6d[1]/2+(-1.18+0.46)*P6d[2]/2+(0.46+1.15)*P6d[3]/2+(1.15+1.95)*P6d[4]/2+(1.95+2.98)*P6d[5]/2+(2.98+4)*P6d[6]/2
# Normalización y etiquetas
P <-transition_matrix4
rowSums(P)
P <- round(P, digits=6)
P <- t(apply(P, 1, function(row) row / sum(row)))
rownames(P) <- labels
colnames(P) <- labels
states <- labels
P
init <- c(0,0,1,0,0,0)

#_____________________Estacionariedad para P_n Cluster_____________________

matrixpower(transition_matrix3,1000) #puedo cambiar este numero y lo eleva mas veces 
# Simulación de la cadena para 100 pasos

simlist <- markov(init,P,100,states)
simlist
table(simlist)/100
steps <- 1000000
simlist <- markov(init,P,steps,states)
table(simlist)/steps

mc = new("markovchain", transitionMatrix=P, states=labels, name="Cadena 1")
mc
summary(mc)

plot(mc, main="Cadena de Markov")

plot(mc, 
     edge.arrow.size = 0.6,    # Tamaño de las flechas
     vertex.label.cex = 1.5,   # Tamaño de las etiquetas de los vértices
     vertex.size = 25,         # Tamaño de los vértices
     layout = layout_in_circle, # Disposición circular
     main = "Cadena de Markov") # Título del gráfico

DistEst = steadyStates(mc)
DistEst


#_____________ MONTE CARLO _____________

# Simulación de Monte Carlo
n_simulations <- 100
results <- matrix(0, n_simulations, length(labels))

for (i in 1:n_simulations) {
  sim <- markov(init, P, steps, labels)
  results[i, ] <- table(factor(sim, levels=labels)) / steps
}

# Calcular las medias de las simulaciones
mean_results <- colMeans(results)
mean_results

# Visualización de los resultados de la simulación de Monte Carlo
barplot(mean_results, main="Resultados de la Simulación de Monte Carlo", 
        names.arg=labels, las=2, col="lightblue")
library(markovchain)
