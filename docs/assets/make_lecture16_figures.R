#Builds the figures for the Week 7 Lecture 16 deck (10/7/2026): the rest of the
#Lecture 15 discrete material (carried over, its figures come from
#make_lecture14_figures.R and make_lecture15_figures.R), then a short block on
#continuous distributions.
#
#  l16_uniform_wait.png  the shuttle wait, X ~ Uniform(0, 20): a flat density at 1/20
#  l16_triangle.png      the triangle density of W, peak 0.4 at W = 2, zero at 0 and 5
#
#  l16_figure_sizes.csv  the size, in points, each figure is drawn at
#
#It also asserts every number the new slides print, by the hand method the course
#teaches (rectangle and triangle areas, the z-table), so the slides cannot drift from
#the arithmetic. The examples are the ones in the Week 7 notes (lecturemenu7.Rmd,
#"Continuous probability distributions"); the heights model is the Week 6 one.
#
#TYPE SIZE. Same rule as the other make_lectureN_figures.R scripts: nothing on a slide
#is under 16pt, so each figure is drawn at exactly its on-slide size (points / 72
#inches) and every text size goes through tsz() or el(), which refuse anything smaller.

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

NAVY <- "#1F3864"; PALE <- "#D9E5F7"
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
near <- function(a, b, tol = 5e-4) abs(a - b) < tol

#----------------------------------------------------------------------------
#numbers printed on the slides

#counting vs. the formula (slide 9): X = heads in 3 tosses, from the 8 outcomes and Binom(3, 0.5)
binom_pmf <- function(k, n, p) factorial(n) / (factorial(k) * factorial(n - k)) * p^k * (1 - p)^(n - k)
S <- c("HHH", "HHT", "HTH", "THH", "HTT", "THT", "TTH", "TTT")
heads <- nchar(gsub("T", "", S))
stopifnot(mean(heads == 3) == 1 / 8, mean(heads == 2) == 3 / 8,
          binom_pmf(3, 3, 0.5) == 1 / 8, binom_pmf(2, 3, 0.5) == 3 / 8, choose(3, 2) == 3)

#shuttle wait: Uniform(0, 20), every probability is base x 1/20
f_u <- 1 / 20
stopifnot(20 * f_u == 1, 5 * f_u == 0.25, (15 - 4) * f_u == 0.55, 0 * f_u == 0,
          near(punif(15, 0, 20) - punif(4, 0, 20), 0.55, 1e-12))

#triangle: rises 0 -> 0.4 on [0, 2], falls 0.4 -> 0 on [2, 5]
tri_f <- function(w) ifelse(w <= 2, 0.2 * w, 0.4 * (5 - w) / 3)
stopifnot(0.5 * 5 * 0.4 == 1, near(integrate(tri_f, 0, 5)$value, 1, 1e-9),
          tri_f(1) == 0.2, near(0.5 * 1 * 0.2, 0.1, 1e-12), near(integrate(tri_f, 0, 1)$value, 0.1, 1e-9),
          near(0.5 * 3 * 0.4, 0.6, 1e-12), near(integrate(tri_f, 2, 5)$value, 0.6, 1e-9))

#women's heights, N(65.4, 3.38) from Week 6, by the z-table (4 decimals)
HM <- 65.4; HS <- 3.38
z70 <- round((70 - HM) / HS, 2); z62 <- round((62 - HM) / HS, 2)
t70 <- round(pnorm(z70), 4); t62 <- round(pnorm(z62), 4)
stopifnot(z70 == 1.36, z62 == -1.01, t70 == 0.9131, t62 == 0.1562,
          near(1 - t70, 0.0869, 1e-9), near(t70 - t62, 0.7569, 1e-9),
          near(pnorm(70, HM, HS) - pnorm(62, HM, HS), 0.76, 5e-3))

#----------------------------------------------------------------------------
#l16_uniform_wait.png: the flat density, unshaded (the areas are drawn on the board)
pu <- ggplot() +
  annotate("rect", xmin = 0, xmax = 20, ymin = 0, ymax = f_u, fill = PALE) +
  annotate("segment", x = c(0, 0, 20), xend = c(20, 0, 20), y = c(f_u, 0, 0), yend = c(f_u, f_u, f_u),
           linewidth = 1, colour = NAVY) +
  annotate("text", x = 10, y = f_u / 2, label = "area = 20 × 1/20 = 1", size = tsz(18),
           colour = NAVY, fontface = "bold") +
  scale_x_continuous(breaks = seq(0, 20, 5)) +
  scale_y_continuous(breaks = c(0, f_u), labels = c("0", "1/20")) +
  coord_cartesian(xlim = c(-1, 22), ylim = c(0, 0.07), expand = FALSE) +
  labs(x = "x = wait (minutes)", y = "f(x)") +
  theme_classic() +
  theme(axis.title = el(16), axis.text = el(16), plot.margin = margin(6, 10, 2, 6))
save_fig(pu, "l16_uniform_wait.png", 400, 175)

#----------------------------------------------------------------------------
#l16_triangle.png: the triangle density of W, unshaded
tri <- data.frame(w = c(0, 2, 5), f = c(0, 0.4, 0))
pt <- ggplot(tri, aes(w, f)) +
  geom_polygon(fill = PALE) +
  geom_line(linewidth = 1, colour = NAVY) +
  annotate("segment", x = 0, xend = 5, y = 0, yend = 0, linewidth = 0.6) +
  scale_x_continuous(breaks = 0:5) +
  scale_y_continuous(breaks = seq(0, 0.4, 0.1), labels = c("0", ".1", ".2", ".3", ".4")) +
  coord_cartesian(xlim = c(-0.2, 5.4), ylim = c(0, 0.45), expand = FALSE) +
  labs(x = "w", y = "f(w)") +
  theme_classic() +
  theme(axis.title = el(16), axis.text = el(16), panel.grid.major = element_line(colour = "grey88"),
        plot.margin = margin(6, 10, 2, 6))
save_fig(pt, "l16_triangle.png", 400, 200)

write.csv(sizes, file.path(assets_dir, "l16_figure_sizes.csv"), row.names = FALSE)
cat("wrote", nrow(sizes), "figures to", assets_dir, "\n")
