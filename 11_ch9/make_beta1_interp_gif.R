## Creates ch9_fig/beta1_interp.gif
## Animated demonstration of parallel slopes in multiple regression.
## Data: case0902 from Sleuth3 (96 mammal species: brain weight, body weight, gestation)
##
## Shows three frames:
##   1. logBrain vs logGestation with a single (marginal) fit line
##   2. Points colored by body-weight tertile; marginal line still shown
##   3. Per-group parallel fit lines from the additive model (same slope,
##      different heights), illustrating the interpretation of beta_1 when
##      body weight is controlled for
##
## Run from the 11_ch9 directory (or any directory — paths are relative to
## this script's location via setwd() below if needed, or just run from root).

library(tidyverse)
library(Sleuth3)
library(magick)

data("case0902")

df <- case0902 |>
  mutate(
    logBrain     = log(Brain),
    logGestation = log(Gestation),
    logBody      = log(Body),
    BodyGroup    = cut(
      log(Body),
      breaks         = quantile(log(Body), probs = c(0, 1/3, 2/3, 1)),
      labels         = c("Small body weight", "Medium body weight", "Large body weight"),
      include.lowest = TRUE
    )
  )

## ── Models ───────────────────────────────────────────────────────────────────

## Marginal model (one line for everyone)
lm_marg <- lm(logBrain ~ logGestation, data = df)

## Additive model: same slope for all groups, different intercepts
lm_add  <- lm(logBrain ~ logGestation + BodyGroup, data = df)

b1_marg <- round(coef(lm_marg)["logGestation"], 2)
b1_part <- round(coef(lm_add)["logGestation"],  2)

## ── Prediction lines (smooth, ordered by x) ──────────────────────────────────

x_seq <- seq(min(df$logGestation), max(df$logGestation), length.out = 200)

## Marginal line
marg_line <- tibble(
  logGestation = x_seq,
  logBrain_hat = predict(lm_marg, newdata = data.frame(logGestation = x_seq))
)

## Parallel lines per group
group_lines <- expand.grid(
  logGestation = x_seq,
  BodyGroup    = levels(df$BodyGroup)
) |>
  as_tibble() |>
  mutate(
    logBrain_hat = predict(
      lm_add,
      newdata = data.frame(logGestation = logGestation,
                           BodyGroup    = BodyGroup)
    )
  )

## ── Plot settings ─────────────────────────────────────────────────────────────

xlim_plot <- range(df$logGestation) + c(-0.15, 0.15)
ylim_plot <- range(df$logBrain)     + c(-0.4,  0.4)

cb_colors <- c(
  "Small body weight"  = "#E69F00",
  "Medium body weight" = "#56B4E9",
  "Large body weight"  = "#009E73"
)

base_theme <- theme_bw(base_size = 13) +
  theme(legend.position = "right",
        plot.title    = element_text(face = "bold", size = 12),
        plot.subtitle = element_text(size = 10, color = "gray30"))

## ── Frame 1: Scatter + marginal line ─────────────────────────────────────────
p1 <- ggplot(df, aes(x = logGestation, y = logBrain)) +
  geom_point(color = "gray40", size = 2) +
  geom_line(data = marg_line,
            aes(x = logGestation, y = logBrain_hat),
            inherit.aes = FALSE,
            color = "#0066cc", linewidth = 1.4) +
  coord_cartesian(xlim = xlim_plot, ylim = ylim_plot) +
  labs(
    x        = "log(Gestation period, days)",
    y        = "log(Brain weight, g)",
    title    = expression(mu(logBrain ~ "|" ~ logGestation) == beta[0] + beta[1] * logGestation),
    subtitle = bquote(hat(beta)[1] == .(b1_marg) ~
                        "  (marginal slope across all species)")
  ) +
  base_theme

## ── Frame 2: Color by body group + marginal line ─────────────────────────────
p2 <- ggplot(df, aes(x = logGestation, y = logBrain)) +
  geom_point(aes(color = BodyGroup), size = 2) +
  geom_line(data = marg_line,
            aes(x = logGestation, y = logBrain_hat),
            inherit.aes = FALSE,
            color = "#0066cc", linewidth = 1.4, linetype = "dashed") +
  scale_color_manual(values = cb_colors, name = "Body weight group") +
  coord_cartesian(xlim = xlim_plot, ylim = ylim_plot) +
  labs(
    x        = "log(Gestation period, days)",
    y        = "log(Brain weight, g)",
    title    = "Body weight confounds the gestation–brain relationship",
    subtitle = "Large animals have both longer gestation AND bigger brains"
  ) +
  base_theme

## ── Frame 3: Parallel lines from additive model ───────────────────────────────
p3 <- ggplot(df, aes(x = logGestation, y = logBrain)) +
  geom_point(aes(color = BodyGroup), size = 2) +
  geom_line(data    = group_lines,
            mapping = aes(x = logGestation, y = logBrain_hat, color = BodyGroup),
            inherit.aes = FALSE,
            linewidth   = 1.4) +
  scale_color_manual(values = cb_colors, name = "Body weight group") +
  coord_cartesian(xlim = xlim_plot, ylim = ylim_plot) +
  labs(
    x = "log(Gestation period, days)",
    y = "log(Brain weight, g)",
    title    = expression(mu(logBrain ~ "|" ~ logGestation * ", " * BodyGroup) ==
                            beta[0] + beta[1] * logGestation + beta[2] * BodyGroup),
    subtitle = bquote(hat(beta)[1] == .(b1_part) ~
                        "  within each group — same slope, different heights")
  ) +
  base_theme

## ── Assemble and save ─────────────────────────────────────────────────────────
tmpdir <- tempdir()

ggsave(file.path(tmpdir, "frame_01.png"), p1, width = 6.5, height = 4.2, dpi = 120)
ggsave(file.path(tmpdir, "frame_02.png"), p2, width = 6.5, height = 4.2, dpi = 120)
ggsave(file.path(tmpdir, "frame_03.png"), p3, width = 6.5, height = 4.2, dpi = 120)

## Duplicate frames so each holds long enough for the viewer to read
frame_files <- c(
  rep(file.path(tmpdir, "frame_01.png"), 4),
  rep(file.path(tmpdir, "frame_02.png"), 4),
  rep(file.path(tmpdir, "frame_03.png"), 6)
)

frames  <- image_read(frame_files)
gif_out <- image_animate(frames, fps = 2, loop = 0)

out_dir <- file.path(getwd(), "ch9_fig")
dir.create(out_dir, showWarnings = FALSE)
image_write(gif_out, file.path(out_dir, "beta1_interp.gif"))
cat("Wrote", file.path(out_dir, "beta1_interp.gif"), "\n")
