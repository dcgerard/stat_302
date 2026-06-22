## Creates ch7_fig/cond_dist.gif: animated conditional distributions for case0701
## Run from repo root or the 09_ch7 directory.

library(tidyverse)
library(Sleuth3)
library(magick)

data("case0701")

lmout <- lm(Distance ~ Velocity, data = case0701)
b0    <- coef(lmout)[["(Intercept)"]]
b1    <- coef(lmout)[["Velocity"]]
s     <- sigma(lmout)

## bell-curve half-width at peak ≈ 120 km/s
bell_scale <- 120 / (1 / (s * sqrt(2 * pi)))

## x values for frames
x_frames <- seq(-150, 1050, length.out = 14)

## constant plot limits
xlim_plot <- c(-350, 1250)
ylim_plot <- c(-0.6, 2.5)

base_plot <- ggplot(case0701, aes(x = Velocity, y = Distance)) +
  geom_point(color = "gray40", size = 1.8) +
  geom_abline(intercept = b0, slope = b1,
              color = "#0066cc", linewidth = 1.2) +
  coord_cartesian(xlim = xlim_plot, ylim = ylim_plot) +
  labs(x = "Recession velocity (km/s)",
       y = "Distance (megaparsecs)",
       title = "Conditional distribution of Distance | Velocity") +
  theme_bw(base_size = 12) +
  theme(plot.title    = element_text(size = 11, face = "bold"),
        plot.subtitle = element_text(size = 9, color = "gray30"))

tmpdir      <- tempdir()
frame_files <- character(length(x_frames))

for (i in seq_along(x_frames)) {
  xv     <- x_frames[i]
  y_mean <- b0 + b1 * xv

  y_seq <- seq(y_mean - 3.5 * s, y_mean + 3.5 * s, length.out = 300)
  dens  <- dnorm(y_seq, mean = y_mean, sd = s)

  bell_df <- data.frame(
    bx = c(xv + dens * bell_scale, rev(xv - dens * bell_scale)),
    by = c(y_seq, rev(y_seq))
  )
  ref_df <- data.frame(rx = xv, ry_lo = ylim_plot[1], ry_hi = y_mean)

  p <- base_plot +
    geom_segment(data = ref_df,
                 aes(x = rx, xend = rx, y = 0, yend = ry_hi),
                 inherit.aes = FALSE,
                 color = "#cc3300", linetype = "dashed", linewidth = 0.7) +
    geom_polygon(data = bell_df,
                 aes(x = bx, y = by),
                 inherit.aes = FALSE,
                 fill = "#cc3300", alpha = 0.35,
                 color = "#cc3300", linewidth = 0.8) +
    geom_point(data = data.frame(px = xv, py = y_mean),
               aes(x = px, y = py),
               inherit.aes = FALSE,
               color = "#cc3300", size = 3.5) +
    labs(subtitle = sprintf(
      "x₀ = %d km/s  •  μ(x₀) = %.3f Mpc  •  σ = %.3f Mpc",
      round(xv), y_mean, s))

  fname <- file.path(tmpdir, sprintf("frame_%02d.png", i))
  ggsave(fname, plot = p, width = 5.5, height = 3.5, dpi = 120)
  frame_files[i] <- fname
}

frames  <- image_read(frame_files)
gif_out <- image_animate(frames, fps = 2, loop = 0)

out_dir <- file.path(getwd(), "ch7_fig")
dir.create(out_dir, showWarnings = FALSE)
image_write(gif_out, file.path(out_dir, "cond_dist.gif"))
cat("Wrote", file.path(out_dir, "cond_dist.gif"), "\n")
