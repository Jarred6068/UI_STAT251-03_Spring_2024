#Checks the numbers on the Week 9 Lecture 19 deck (10/19/2026): finding areas under the
#standard normal curve, then the central limit theorem and confidence intervals carried
#over from Lecture 18 (which stopped before the CLT), plus a first interval for a
#proportion. It draws:
#
#  l19_three_areas.png  lower tail, upper tail and area between, side by side
#  l19_lower.png        P(Z < 1.25) and P(Z < -0.84)
#  l19_upper.png        P(Z > 1.80) and P(Z > -1.30)
#  l19_between.png      P(-1.30 < Z < 1.80) and P(-1.96 < Z < 1.96)
#  l19_figure_sizes.csv the size, in points, each figure is drawn at
#
#The carried-over slides reuse the Lecture 18 figures (make_lecture18_figures.R checks
#those numbers). The z-table values here are the four-decimal values of the course
#z-table, i.e. round(pnorm(z), 4), which is how students will read them.
#
#TYPE SIZE. Same rule as the other make_lectureN_figures.R scripts: nothing on a slide
#is under 16pt, so each figure is drawn at exactly its on-slide size (points / 72
#inches) and every text size goes through tsz() or el(), which refuse anything smaller.
#
#SOURCES
#  Teen cell phones: Pew Research Center survey of 743 U.S. teens aged 13-17, "Are you
#    losing focus in class by checking your cell phone?" Never 289, Rarely 216,
#    Sometimes 178, Often 60 (the published counts behind docs/Data/
#    teens_cell_phone_focus_simulated.csv; see assets/make_teens_cell_phone_data.R).
#  World Cup: 2.69 goals per match, as Lecture 18.

near <- function(a, b, tol = 5e-4) abs(a - b) < tol
zt <- function(z) round(pnorm(z), 4)          # a z-table lookup

#lower tail
stopifnot(zt(1.25) == 0.8944, zt(-0.84) == 0.2005)
#upper tail: 1 - table value
stopifnot(zt(1.80) == 0.9641, near(1 - zt(1.80), 0.0359, 1e-9),
          zt(-1.30) == 0.0968, near(1 - zt(-1.30), 0.9032, 1e-9))
#between: larger table value minus smaller
stopifnot(near(zt(1.80) - zt(-1.30), 0.8673, 1e-9),
          zt(1.96) == 0.9750, zt(-1.96) == 0.0250, near(zt(1.96) - zt(-1.96), 0.95, 1e-9))
#practice, and the empirical rule it should recall (68% and 95%)
stopifnot(zt(2) == 0.9772, near(1 - zt(0.5), 0.3085, 1e-9),
          near(zt(1) - zt(-1), 0.6826, 1e-9), near(zt(2) - zt(-2), 0.9544, 1e-9))

#World Cup tournament average: P(x-bar >= 3), the CLT used with the z-table
z3 <- round((3 - 2.69) / 0.205, 2)
stopifnot(z3 == 1.51, near(1 - zt(z3), 0.0655, 1e-9))

#teens losing focus in class: "sometimes" or "often"
x_t <- 178 + 60; n_t <- 289 + 216 + 178 + 60
ph_t <- round(x_t / n_t, 3); se_t <- round(sqrt(ph_t * (1 - ph_t) / n_t), 3)
stopifnot(x_t == 238, n_t == 743, ph_t == 0.320, n_t - x_t == 505, se_t == 0.017,
          near(2 * se_t, 0.034, 1e-9), near(ph_t - 2 * se_t, 0.286, 1e-9),
          near(ph_t + 2 * se_t, 0.354, 1e-9))

#----------------------------------------------------------------------------
suppressPackageStartupMessages(library(ggplot2))

this_file <- tryCatch(normalizePath(sys.frame(1)$ofile, winslash = "/"),
                      error = function(e) NA_character_)
if (is.na(this_file)) {
  args <- commandArgs(trailingOnly = FALSE)
  m <- grep("^--file=", args, value = TRUE)
  this_file <- if (length(m)) normalizePath(sub("^--file=", "", m[[1]]),
                                            winslash = "/") else NA_character_
}
if (is.na(this_file)) stop("Could not determine the script location; run it with Rscript.")
assets_dir <- dirname(this_file)

NAVY <- "#1F3864"; PALE <- "#D9E5F7"; SHADE <- "#8FAADC"; RED <- "#C00000"
MIN_PT <- 16
tsz <- function(pt) { stopifnot(pt >= MIN_PT); pt / ggplot2::.pt }
el  <- function(pt, ...) { stopifnot(pt >= MIN_PT); element_text(size = pt, ...) }

sizes <- data.frame(file = character(), w_pt = numeric(), h_pt = numeric(),
                    stringsAsFactors = FALSE)
save_fig <- function(p, file, w_pt, h_pt) {
  ggsave(file.path(assets_dir, file), p, width = w_pt / 72, height = h_pt / 72,
         dpi = 300, bg = "white")
  sizes[nrow(sizes) + 1, ] <<- list(file, w_pt, h_pt)
}
bare_x <- function() theme_classic() +
  theme(axis.title.x = element_blank(), axis.text.x = el(16), axis.title.y = element_blank(),
        axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        strip.text = el(18, face = "bold", colour = NAVY), strip.background = element_blank(),
        panel.spacing = unit(28, "pt"), plot.margin = margin(4, 12, 2, 12))

MINUS <- "−"
fmtz <- function(z) sub("-", MINUS, formatC(z, format = "f", digits = 2), fixed = TRUE)

#one panel per row of `areas`: a standard normal curve shaded from lo to hi, with a red
#line at each finite cut-off
curves <- function(areas) {
  grid <- seq(-3.5, 3.5, length.out = 401)
  areas$panel <- factor(areas$panel, levels = areas$panel)
  line <- do.call(rbind, lapply(seq_len(nrow(areas)), function(i)
    data.frame(panel = areas$panel[i], x = grid, y = dnorm(grid))))
  fill <- do.call(rbind, lapply(seq_len(nrow(areas)), function(i) {
    g <- grid[grid >= max(areas$lo[i], -3.5) & grid <= min(areas$hi[i], 3.5)]
    g <- sort(unique(c(max(areas$lo[i], -3.5), g, min(areas$hi[i], 3.5))))
    data.frame(panel = areas$panel[i], x = g, y = dnorm(g)) }))
  cuts <- do.call(rbind, lapply(seq_len(nrow(areas)), function(i) {
    z <- c(areas$lo[i], areas$hi[i]); z <- z[is.finite(z)]
    data.frame(panel = areas$panel[i], z = z) }))
  ggplot() +
    geom_area(data = fill, aes(x, y), fill = SHADE, alpha = 0.8) +
    geom_line(data = line, aes(x, y), colour = NAVY, linewidth = 0.9) +
    geom_segment(data = cuts, aes(x = z, xend = z, y = 0, yend = dnorm(z) + 0.04),
                 colour = RED, linewidth = 1) +
    facet_wrap(~panel, nrow = 1) +
    scale_x_continuous(breaks = -3:3, labels = sub("-", MINUS, -3:3, fixed = TRUE)) +
    coord_cartesian(xlim = c(-3.5, 3.5), ylim = c(0, 0.45), expand = FALSE) +
    bare_x()
}
pz <- function(lo, hi) {
  if (!is.finite(lo)) return(paste0("P(Z < ", fmtz(hi), ")"))
  if (!is.finite(hi)) return(paste0("P(Z > ", fmtz(lo), ")"))
  paste0("P(", fmtz(lo), " < Z < ", fmtz(hi), ")")
}
two <- function(lo, hi) data.frame(panel = mapply(pz, lo, hi), lo = lo, hi = hi)

#----------------------------------------------------------------------------
save_fig(curves(data.frame(panel = c("lower tail", "upper tail", "area between"),
                           lo = c(-Inf, 1.80, -1.30), hi = c(-1.30, Inf, 1.80))),
         "l19_three_areas.png", 840, 200)
save_fig(curves(two(c(-Inf, -Inf), c(1.25, -0.84))), "l19_lower.png", 680, 200)
save_fig(curves(two(c(1.80, -1.30), c(Inf, Inf))), "l19_upper.png", 680, 200)
save_fig(curves(two(c(-1.30, -1.96), c(1.80, 1.96))), "l19_between.png", 680, 200)

write.csv(sizes, file.path(assets_dir, "l19_figure_sizes.csv"), row.names = FALSE)
cat("Lecture 19 numbers check out; wrote", nrow(sizes), "figures to", assets_dir, "\n")
