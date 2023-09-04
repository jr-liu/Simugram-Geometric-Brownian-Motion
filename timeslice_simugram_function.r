# Simugram plot function
simugram.plot <- function(s0, mu, sigma, n, run, year, xlim, ylim, angle, is_from_front_to_back, is_colorful,
                          is_shade, is_showxaxis, is_showmarkpoint, markpoint) {
  
  
  ## Simulates terminal values of GBM
  sim.gbm <- function(s0, t, mu, sigma, n) {
    s0*exp((mu - .5*sigma^2)*t + sigma*sqrt(t)*rnorm(n)) ## mu represents drift; sigma represents volatility
  }
  
  ## The mean rate of return for s(t) is (mu - .5*sigma^2) 
  
  simugram <- function(tv, run) {
    m = length(tv)
    x <- seq(min(pretty(tv)), max(pretty(tv)), length = run)
    ecdf.jad <- NULL
    for (i in 1:length(x)) {
      ecdf.jad <- c(ecdf.jad, length(tv[tv <= x[i]])/m)
    }
    return(list(x = x, y = ecdf.jad))
  }
  
  if (is_from_front_to_back == TRUE) {
    year <- rev(year)
  }
  
  ## Initiate basic parameters
  x_input = c(0, 10000) ## x_input contains all the simulated terminal prices
  cat_input = c(0, 1) ## cat_input contains all the corresponding probabilities
  ncat_input <- length(year) ## Number of categories to iterate
  
  ## Set the colors
  line_color <- rep(1, ncat_input)
  shade_color <- rep("#d3d3d3", ncat_input)
  if (is_colorful == TRUE) {
    line_color <- c(1:ncat_input)
    shade_color <- c("#d3d3d3", "#ffc0cb", "#dbead5", "#a7d3f5", "#ceffff", "#ecd8e9", "#ffe596", "#e9e9e9")
  }
  
  ## View angle parameters
  slope <- 0.4/1000*angle
  y_angle <- 0.4
  x_angle <- y_angle/slope
  by_index <- 0.1
  
  if (angle == 0) {
    y_angle <- 0
    x_angle <- 0
  }
  
  if (angle < 0) {
    opar <- par(no.readonly = TRUE)
    par(mar = c(5.1, 1.1, 4.1, 5.1))
    by_index <- -0.1
  }
  
  x_lab = "$1000"
  y_lab = "Probability"
  xlim_show = c(0, 10000, 2000) ## xlim is displayed as from 0 to 10000 by 2000
  ylim_show = c(0, 1, 0.2) ## ylim (probability) is displayed as from 0 to 1 by 0.2
  
  plot(x_input[1], 0, type = "n",
       main = paste0("Portfolio Simugram (", "mu = ", as.character(mu), ", sigma = ", as.character(sigma), ")"),
       xlab = "", ylab = "", xlim = xlim, ylim = ylim, bty = "l", yaxs = "i", xaxs = "i", axes = FALSE)
  
  x_adj <- (ncat_input)*x_angle
  y_adj <- (ncat_input)*y_angle
  
  b <- x_input[1]*slope
  z_x <- seq(x_input[1], (x_input[1] + x_adj), by = by_index)
  z_y <- (z_x*slope) - b
  x_axis_range <- xlim_show[2] - x_input[1]
  z_x2 <- z_x + x_axis_range
  
  lines(z_x, z_y)
  lines(z_x2, z_y)
  segments((x_input[1] + ncat_input*x_angle), (ncat_input*y_angle), (z_x2[length(z_x2)]),(ncat_input*y_angle))
  segments(x_input[1], 0, (z_x2[1]), 0)
  
  ## Iterate through year categories
  for (i in 1:ncat_input) {
    x_adj <- x_adj-x_angle
    y_adj <- y_adj-y_angle
    
    h <- year[i]
    tv <- sim.gbm(s0, t = h, mu, sigma, n)
    plot_index <- which(simugram(tv, run)$x/1000 <= 10000) ## Do not over plot
    x_input <- simugram(tv, run)$x[plot_index]/1000
    cat_input <- simugram(tv, run)$y[plot_index]
    
    if (angle > 0) {
      segments(xlim_show[2] + x_adj, y_adj, xlim_show[2] + x_adj + 50, y_adj)
      text(xlim_show[2] + x_adj + 20, y_adj + 0.06, paste0(year[i], " Year"), pos = 4)
    }

    if (angle < 0) {
      segments(x_input[1] + x_adj, y_adj, x_input[1] + x_adj-50, y_adj)
      text(x_input[1] + x_adj - 20, y_adj + 0.06, paste0(year[i], " Year"), pos = 2)
    }
    
    if (is_shade == TRUE) {
      x_start <- 1
      polygon(c(x_input[x_start] + x_adj, x_input[x_start:length(x_input)] + x_adj, x_input[length(x_input)] + x_adj),
              c(y_adj, cat_input[x_start:length(x_input)] + y_adj, y_adj), col = shade_color[i], border = FALSE)
    }
    lines(x_input + x_adj, cat_input + y_adj, col = line_color[i], lty = 1, lwd = 2)
    
    if (is_showxaxis == TRUE){
      segments((x_input[1] + x_adj), y_adj, (z_x2[length(z_x2)] - x_angle*i), y_adj)
    }
    
    if (is_showmarkpoint == TRUE) {
      i <- 1
      for (i in 1:length(markpoint)) {
        points(markpoint[i] + x_adj, y_adj, pch = 3)
        text(markpoint[i] + x_adj, y_adj + 0.2, paste0(markpoint[i]/1000, "M"), cex = 0.7)
      }
    }
    i = i + 1
  }
  
  segments(x_input[1], 0, (z_x2[1]), 0)
  axis(1, at = seq(xlim_show[1], xlim_show[2], xlim_show[3]))
  
  if (angle >= 0) {
    axis(2, at = seq(ylim_show[1], ylim_show[2], ylim_show[3]), las = 1)
    mtext(x_lab, side = 1, line = 2.2, adj = 0.32)
    mtext(y_lab, side = 2, line = 3.2, adj = 0)
  }
  
  else {
    axis(4, at = seq(ylim_show[1], ylim_show[2], ylim_show[3]), las = 1)
    mtext(x_lab, side = 1, line = 2.2, adj = 0.68)
    mtext(y_lab, side = 4, line = 3, adj = 0)
    par(opar)
  }
}

# Call the function
simugram.plot(s0 = 100000, mu = 0.1, sigma = 0.2, n = 1000, run = 1000, year = c(1, 5, 10, 15, 20, 25, 30), 
              xlim = c(-12000, 12000), ylim = c(0, 3), angle = -0.8, is_from_front_to_back = FALSE, is_colorful = TRUE,
              is_shade = FALSE, is_showxaxis = TRUE, is_showmarkpoint = TRUE, markpoint = c(1000, 5000))