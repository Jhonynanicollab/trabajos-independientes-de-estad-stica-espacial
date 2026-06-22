library(spdep)

# Datos grilla 3x3
z <- c(2.1, 2.0, 1.6,
       1.9, 1.8, 1.5,
       1.7, 1.4, 1.3)


# MÉTODO TORRE (rook) - menos vecinos

nb_rook <- cell2nb(3, 3, type = "rook")
lw_rook <- nb2listw(nb_rook, style = "B")  # B = binaria

# Calcular I
I_rook <- moran(z, lw_rook, n = length(z), S0 = Szero(lw_rook))$I
print(paste("I torre (rook):", round(I_rook, 4)))

# Prueba analítica
moran.test(z, lw_rook)

# MÉTODO REINA (queen) - más vecinos (incluye diagonales)
nb_queen <- cell2nb(3, 3, type = "queen")
lw_queen <- nb2listw(nb_queen, style = "B")

I_queen <- moran(z, lw_queen, n = length(z), S0 = Szero(lw_queen))$I
print(paste("I reina (queen):", round(I_queen, 4)))

moran.test(z, lw_queen)

# PRUEBA MONTE CARLO (más robusta con n pequeño)
set.seed(123)  # para reproducibilidad
moran.mc(z, lw_rook, nsim = 999)
moran.mc(z, lw_queen, nsim = 999)