# Non-Linear Dynamical Systems: The Lorenz Strange Attractor
# Tier 4 (Mind-Blowing) - Dynamics

make_plot <- function(data_dir = "data") {
  library(ggplot2)
  if (file.exists("theme_journal.R")) source("theme_journal.R", local = TRUE)
  
  # Numerical integration of the classic Lorenz 1963 chaotic differential equations
  # dx/dt = sigma*(y - x), dy/dt = x*(rho - z) - y, dz/dt = x*y - beta*z
  sigma <- 10
  rho <- 28
  beta <- 8/3
  dt <- 0.01
  n_steps <- 4500
  
  xs <- numeric(n_steps)
  ys <- numeric(n_steps)
  zs <- numeric(n_steps)
  
  # Initial perturbation
  xs[1] <- 0.1; ys[1] <- 1.0; zs[1] <- 1.05
  
  for (i in 1:(n_steps - 1)) {
    dx <- sigma * (ys[i] - xs[i]) * dt
    dy <- (xs[i] * (rho - zs[i]) - ys[i]) * dt
    dz <- (xs[i] * ys[i] - beta * zs[i]) * dt
    
    xs[i + 1] <- xs[i] + dx
    ys[i + 1] <- ys[i] + dy
    zs[i + 1] <- zs[i] + dz
  }
  
  df_lorenz <- data.frame(
    step = 1:n_steps,
    x = xs,
    y = ys,
    z = zs
  )
  
  p <- ggplot(df_lorenz, aes(x = x, y = z, color = step)) +
    geom_path(linewidth = 0.55, alpha = 0.85) +
    scale_color_viridis_c(option = "inferno", name = "Evolutionary Time Step (t):") +
    scale_x_continuous(limits = c(-22, 22), breaks = seq(-20, 20, by = 10)) +
    scale_y_continuous(limits = c(0, 52), breaks = seq(0, 50, by = 10)) +
    annotate("text", x = 0, y = 48, label = "Two-Winged Butterfly Attractor Lobe", 
             fontface = "bold", color = "#F59E0B", size = 3.4) +
    labs(
      title = "Non-Linear Phase Portrait: The Lorenz Strange Attractor",
      subtitle = "Chaotic deterministic trajectory in state space (X vs Z projection); exhibits non-repeating fractal orbits",
      x = "State Space Coordinate X (Convective Flow Rate)",
      y = "State Space Coordinate Z (Vertical Temperature Distortion)",
      caption = "Source: Simulated Lorenz 1963 non-linear system (sigma = 10, rho = 28, beta = 8/3)"
    )
  
  if (exists("theme_topjournal")) {
    p <- p + theme_topjournal(grid = "xy") +
      theme(
        legend.position = "top",
        legend.direction = "horizontal",
        legend.title = element_text(face = "bold", size = 8.5),
        legend.key.width = unit(1.8, "cm"),
        legend.key.height = unit(0.35, "cm")
      )
  } else {
    p <- p + theme_classic()
  }
  p
}

explain <- function() {
  list(
    title = "Non-Linear Dynamical Phase Portrait (Lorenz Strange Attractor)",
    how_to_read = "A continuous phase-space trajectory orbiting through time. The orbit never intersects itself and never repeats, looping indefinitely between two symmetrical butterfly wings around unstable fixed points. Color tracks temporal progress from dark purple to blazing yellow.",
    when_to_use = "The foundational icon of chaos theory, meteorology, cardiac electrophysiology, and non-linear dynamics (Edward Lorenz 1963) demonstrating sensitive dependence on initial conditions (the butterfly effect).",
    pitfalls = "Chaotic dynamics are strictly deterministic (not random noise); numerical integration must use sufficiently small time steps (dt <= 0.01) to prevent numerical instability.",
    journal_archetype = "Physical Review Letters, Nature Physics, and Chaos non-linear dynamics landmark papers.",
    data_recipe = "Requires time-series differential equation integration coordinates across state variables."
  )
}
