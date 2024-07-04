setwd("/Users/lucasrissetticabrera/Desktop/R_Estoc/Proyecto")

source("funciones.R")
#install.packages("markovchain")
library(markovchain)
#install.packages("DiagrammeR")
library(DiagrammeR)
library(igraph)

#_____________________P_3Cluster_____________________

transition_matrix1 <- matrix(c(
  0.890625, 0.109375, 0.000000,
  0.162162, 0.648649, 0.189189,
  0.000000, 0.500000, 0.500000
), nrow=3, byrow=TRUE)

labels <- c("[5-7.9)", "[7.9-9.9)", "[9.9-13)")

# Normalización y etiquetas
P <-transition_matrix1
rowSums(P)
P <- round(P, digits=6)
P <- t(apply(P, 1, function(row) row / sum(row)))
rownames(P) <- labels
colnames(P) <- labels
states <- labels
P
init <- c(0,1,0)

#_____________________P_4Cluster_____________________

transition_matrix2 <- matrix(c(
  0.852941, 0.147059, 0.000000, 0.000000,
  0.113636, 0.659091, 0.204545, 0.022727,
  0.000000, 0.307692, 0.538462, 0.153846,
  0.000000, 0.090909, 0.363636, 0.545455
), nrow=4, byrow=TRUE)

labels <- c("[5-7.1)", "[7.1-8.48)", "[8.48-10.17)", "[10.17-13]")

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
  0.444444, 0.555556, 0.000000, 0.000000, 0.000000,
  0.121212, 0.696970, 0.181818, 0.000000, 0.000000,
  0.027778, 0.111111, 0.583333, 0.250000, 0.027778,
  0.000000, 0.000000, 0.307692, 0.538462, 0.153846,
  0.000000, 0.000000, 0.090909, 0.363636, 0.545455
), nrow=5, byrow=TRUE)

labels <- c("[5-6.35)", "[6.35-7.34)", "[7.34-8.53)", "[8.53-10.17)", "[10.17-13)")

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
  0.444444, 0.555556, 0.000000, 0.000000, 0.000000, 0.000000,
  0.121212, 0.696970, 0.181818, 0.000000, 0.000000, 0.000000,
  0.027778, 0.111111, 0.583333, 0.250000, 0.027778, 0.000000,
  0.000000, 0.000000, 0.304348, 0.434783, 0.217391, 0.043478,
  0.000000, 0.000000, 0.181818, 0.363636, 0.363636, 0.090909,
  0.000000, 0.000000, 0.000000, 0.333333, 0.333333, 0.333333
), nrow=6, byrow=TRUE)

labels <- c("[5-6.35)", "[6.35-7.34)", "[7.34-8.49)", "[8.49-9.83)", "[9.83-11.33)", "[11.33-13)")

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

matrixpower(P,100) #puedo cambiar este numero y lo eleva mas veces 
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
period(mc)
plot(mc, main="Cadena de Markov")

plot(mc, 
     edge.arrow.size = 0.6,    # Tamaño de las flechas
     vertex.label.cex = 1.5,   # Tamaño de las etiquetas de los vértices
     vertex.size = 25,         # Tamaño de los vértices
     layout = layout_in_circle, # Disposición circular
     main = "Cadena de Markov") # Título del gráfico

DistEst = steadyStates(mc)
DistEst

rowSums(DistEst)



#________________________________NEW________________________________

simulaCadena <- function (n, X0 ,P){
  dim <- length(P[1,]) # length = dimenstón del vecto
  Xn <- numeric((n+1)) # declaro un vector en ceros
  Xn[1] <- X0
  for (i in 2: (n+1)){
    aux <- Xn[i-1]
    Xn[i] <- sample(1:dim, 1, T ,P[aux, ])
  }
  plot((0:n) , Xn, type = "l",
       pch = 16,col = "red", lwd=0.5)
  abline (h = 0, v=0)
  grid(10)
  Xn
}
simulaCadena (1000,1,P)
simulaCadena (1000,2,P)
simulaCadena (1000,3,P)
simulaCadena (1000,4,P)
simulaCadena (1000,5,P)
simulaCadena (1000,6,P)

# Buscar paquete MCMC
# Comparar con simulación monte carlo
# diferencias con la simumlación
# Metropoli hasting MCMC ¿hapsting?
#   Estima de cierta forma muchas veces la estacionaria y luego saca el promedio
#   No elevarlo a la 1000
#   Givesammple
# mejor agrupamiento

# con R2 lineal y ver cual sea el menor entre el numero de clasificación


#____________________________MCMC____________________________


num_steps <- 100
num_simulations <- 1000

simulate_markov <- function(P, labels, num_steps, num_simulations) {
  # Crear objeto markovchain con la matriz de transición y etiquetas de estados
  mc <- new("markovchain", states = labels, transitionMatrix = P, name = "Cadena de Markov")
  
  # Matriz para registrar los estados finales de cada simulación
  final_states <- matrix(0, nrow = num_simulations, ncol = length(labels))
  
  # Simulación de Monte Carlo
  for (i in 1:num_simulations) {
    # Realizar una secuencia de Markov
    simulation <- markovchainSequence(n = num_steps, markovchain = mc)
    # Registrar el último estado de la simulación
    final_states[i, which(labels == tail(simulation, 1))] <- 1
  }
  
  # Calcular la distribución estacionaria empírica como el promedio de los estados finales
  empirical_stationary_distribution <- colSums(final_states) / num_simulations
  
  # Visualización de la cadena de Markov
  plot(mc, main = "Visualización de la Cadena de Markov", edge.arrow.size = 0.6, 
       vertex.label.cex = 1.5, vertex.size = 25, layout = layout_in_circle)
  
  # Verificación de la convergencia observando la variabilidad de las distribuciones empíricas
  convergence_test <- apply(final_states, 2, var)
  
  # Análisis de sensibilidad (opcional): Cambiar num_steps y num_simulations para ver su efecto
  # Puede ser implementado aquí o como un análisis adicional fuera de esta función
  
  return(list(
    MarkovChain = mc,
    EmpiricalStationaryDistribution = empirical_stationary_distribution,
    ConvergenceTest = convergence_test
  ))
}
simulate_markov(P, labels, num_steps, num_simulations)

