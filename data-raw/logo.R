# Hex logo for lopensemble.
#
# Run with `Rscript data-raw/logo.R` to regenerate man/figures/logo.png and
# man/figures/logo.svg. Needs only grid (base) and ragg.

library(grid)

## --- palette ---------------------------------------------------------------
col_bg_top <- "#0C1A2B"
col_bg_bot <- "#1D3E63"
col_border <- "#63A6D6"
col_hist <- "#DCE6F0"
col_member <- "#7FC0EA"
col_ribbon <- "#63A6D6"
col_pool <- "#F2A25C"
col_text <- "#F7FAFD"

## --- hexagon ---------------------------------------------------------------
hex_angle <- seq(30, 330, by = 60) * pi / 180
hex_x <- 0.5 + cos(hex_angle) / sqrt(3)
hex_y <- 0.5 + sin(hex_angle) / 2

## --- forecast geometry -----------------------------------------------------
# Everything below lives in a [0, 1] x [0, 1] panel that is placed in the
# middle of the hexagon further down. The series runs up to `horizon`; the
# strip to the right of it holds the mixture density.
split <- 0.30 # last observation
horizon <- 0.76 # end of the forecast, and baseline of the density

history <- function(x) 0.50 + 0.13 * sin(6.0 * x + 0.4)

# Ensemble members leave the last observation with different drifts, plus a
# wiggle that grows with the forecast horizon.
drift <- c(0.34, 0.17, 0.00, -0.15, -0.31)
wiggle <- c(0.035, -0.05, 0.045, -0.03, 0.04)
freq <- c(9.0, 11.5, 7.5, 13.0, 8.5)
phase <- c(0.0, 1.3, 2.4, 0.7, 3.0)

member <- function(x, i) {
  h <- (x - split) / (horizon - split)
  history(split) + drift[i] * h^1.3 + wiggle[i] * sin(freq[i] * h + phase[i]) * h
}

x_hist <- seq(0, split, length.out = 200)
x_fc <- seq(split, horizon, length.out = 200)
members <- lapply(seq_along(drift), function(i) member(x_fc, i))

# The pool: a weighted mixture of the members, with the kind of weights
# `crps_weights()` returns, so the pooled path leans on the better models.
weights <- c(0.12, 0.28, 0.32, 0.19, 0.09)
pooled <- Reduce(`+`, Map(`*`, members, weights))

lo <- do.call(pmin, members)
hi <- do.call(pmax, members)

# Mixture density at the horizon: one Gaussian bump per member, weighted.
ends <- vapply(members, function(m) m[length(m)], numeric(1))
y_grid <- seq(min(ends) - 0.14, max(ends) + 0.14, length.out = 400)
dens <- rowSums(vapply(seq_along(drift), function(i) {
  weights[i] * dnorm(y_grid, mean = ends[i], sd = 0.07)
}, numeric(length(y_grid))))
dens <- dens / max(dens) * (1 - horizon) * 0.95

## --- drawing ---------------------------------------------------------------
draw_logo <- function() {
  grid.newpage()

  hex <- polygonGrob(hex_x, hex_y)
  grid.polygon(
    hex_x, hex_y,
    gp = gpar(
      fill = linearGradient(
        c(col_bg_bot, col_bg_top),
        x1 = 0.1, y1 = 0, x2 = 0.9, y2 = 1
      ),
      col = NA
    )
  )

  # Plot panel; "inherit" keeps the hexagonal clipping path in force.
  pushViewport(viewport(clip = hex))
  pushViewport(viewport(
    x = 0.49, y = 0.595, width = 0.76, height = 0.42, clip = "inherit"
  ))

  # Spread of the members.
  grid.polygon(
    c(x_fc, rev(x_fc)), c(hi, rev(lo)),
    gp = gpar(fill = adjustcolor(col_ribbon, alpha.f = 0.22), col = NA)
  )

  # Individual model predictions.
  for (m in members) {
    grid.lines(x_fc, m, gp = gpar(col = adjustcolor(col_member, alpha.f = 0.8),
                                  lwd = 3, lineend = "round"))
  }

  # Mixture density, standing on the forecast horizon.
  grid.lines(c(horizon, horizon), range(y_grid),
             gp = gpar(col = adjustcolor(col_hist, alpha.f = 0.35), lwd = 3))
  grid.lines(horizon + dens, y_grid,
             gp = gpar(col = col_pool, lwd = 6, lineend = "round"))

  # Observed series and the pooled prediction.
  grid.lines(x_hist, history(x_hist),
             gp = gpar(col = col_hist, lwd = 6, lineend = "round"))
  grid.lines(x_fc, pooled,
             gp = gpar(col = col_pool, lwd = 7, lineend = "round"))

  popViewport(1)

  # Wordmark, scaled to a fixed share of the hexagon width.
  fs <- 20
  gp <- gpar(col = col_text, fontfamily = "Inter", fontface = "bold",
             fontsize = fs)
  w_txt <- convertWidth(grobWidth(textGrob("lopensemble", gp = gp)), "npc",
                        valueOnly = TRUE)
  gp$fontsize <- fs * 0.60 / w_txt
  grid.text("lopensemble", x = 0.5, y = 0.245, gp = gp)

  popViewport(1)

  # Border last so it sits on top of everything.
  grid.polygon(hex_x, hex_y,
               gp = gpar(fill = NA, col = col_border, lwd = 14))
}

## --- output ----------------------------------------------------------------
w <- 1200
h <- w * 2 / sqrt(3)
dir.create("man/figures", showWarnings = FALSE, recursive = TRUE)

ragg::agg_png("man/figures/logo.png", width = w, height = h, units = "px",
              res = 300, background = "transparent")
draw_logo()
invisible(dev.off())

svg("man/figures/logo.svg", width = w / 300, height = h / 300, bg = "transparent")
draw_logo()
invisible(dev.off())

message("wrote man/figures/logo.png and man/figures/logo.svg")
