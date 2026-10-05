#Builds the figures for the Week 7 Lecture 15 deck (10/5/2026): the rest of the
#Lecture 14 probability material (carried over, its figures come from
#make_lecture13_figures.R and make_lecture14_figures.R), then the Bernoulli, binomial
#and Poisson distributions.
#
#  l15_binomial_shapes.png  Binom(10, p) for p = 0.2, 0.5, 0.8 side by side: what p
#                           does to the shape
#  l15_poisson_goals.png    Poisson(2.69): goals per match at the 2022 World Cup
#
#  l15_figure_sizes.csv     the size, in points, each figure is drawn at
#
#It also asserts every number the new slides print, by the hand formula the course
#teaches, so the slides cannot drift from the arithmetic.
#
#SOURCES (checked Oct 5 2026)
#  2022 FIFA World Cup: 172 goals in 64 matches (2.69 per match, a record for the
#    32-team format) - Wikipedia "2022 FIFA World Cup", tournament statistics. Seven
#    matches ended 0-0 (Denmark-Tunisia, Mexico-Poland, Morocco-Croatia, Uruguay-South
#    Korea, England-USA, Croatia-Belgium, Morocco-Spain after extra time), tying the
#    record - Eurosport / BetMGM "how many World Cup matches finished 0-0".
#  Stephen Curry career free throws: 4,236 of 4,645 = .912 - Basketball-Reference,
#    curryst01, career totals.
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

#the hand formulas, exactly as written on the slides
binom_pmf <- function(k, n, p) factorial(n) / (factorial(k) * factorial(n - k)) * p^k * (1 - p)^(n - k)
pois_pmf  <- function(k, lambda) lambda^k * exp(-lambda) / factorial(k)
near <- function(a, b, tol = 5e-4) abs(a - b) < tol

#----------------------------------------------------------------------------
#numbers printed on the slides

#guessing one 4-option question: Bernoulli(0.25)
p1 <- 0.25
stopifnot(1 - p1 == 0.75, p1 * (1 - p1) == 0.1875)

#three tosses again: Binom(3, 0.5), P(X = 2) = 3/8 and 3!/(2!1!) = 3
stopifnot(choose(3, 2) == 3, binom_pmf(2, 3, 0.5) == 3 / 8)

#guessing a 10-question quiz: Binom(10, 0.25)
stopifnot(near(binom_pmf(3, 10, 0.25), 0.250), near(dbinom(3, 10, 0.25), binom_pmf(3, 10, 0.25), 1e-12),
          near(binom_pmf(0, 10, 0.25), 0.056), 10 * 0.25 == 2.5,
          near(sqrt(10 * 0.25 * 0.75), 1.37, 5e-3))

#Curry free throws: Binom(10, 0.912)
p_ft <- 4236 / 4645
stopifnot(near(p_ft, 0.912))
stopifnot(near(0.912^10, 0.398), near(1 - 0.912^10, 0.602), near(10 * 0.912, 9.12, 1e-9))

#World Cup goals: Poisson(172 / 64)
lam <- 172 / 64
stopifnot(near(lam, 2.69, 5e-3), near(pois_pmf(0, lam), 0.068), near(dpois(0, lam), pois_pmf(0, lam), 1e-12),
          near(64 * pois_pmf(0, lam), 4.4, 0.05),
          near(lam / 2, 1.34, 5e-3), near(exp(-lam / 2), 0.26, 5e-3))
pk <- pois_pmf(0:8, lam)
stopifnot(which.max(pk) == 3, near(pk[3], 0.25, 5e-3), near(pk[4], 0.22, 5e-3))

#----------------------------------------------------------------------------
#l15_binomial_shapes.png: Binom(10, p) for three values of p, same y axis
ps <- c(0.2, 0.5, 0.8)
d <- do.call(rbind, lapply(ps, function(p) data.frame(
  k = 0:10, prob = binom_pmf(0:10, 10, p), panel = sprintf("p = %.1f", p))))
stopifnot(all(abs(tapply(d$prob, d$panel, sum) - 1) < 1e-12))
pb <- ggplot(d, aes(k, prob)) +
  geom_col(width = 0.75, fill = PALE, colour = "grey35", linewidth = 0.4) +
  facet_wrap(~panel, nrow = 1) +
  scale_x_continuous(breaks = c(0, 5, 10)) +
  scale_y_continuous(breaks = c(0, 0.1, 0.2, 0.3), labels = c("0", ".1", ".2", ".3")) +
  coord_cartesian(ylim = c(0, 0.32), expand = FALSE) +
  labs(x = "k = number of successes out of n = 10", y = "P(k)") +
  theme_classic() +
  theme(axis.title = el(16), axis.text = el(16),
        strip.text = el(18, face = "bold", colour = NAVY), strip.background = element_blank(),
        panel.spacing = unit(18, "pt"), plot.margin = margin(4, 10, 2, 6))
save_fig(pb, "l15_binomial_shapes.png", 840, 205)

#----------------------------------------------------------------------------
#l15_poisson_goals.png: Poisson(2.69), k = 0..8, bars labelled with their probability
dp <- data.frame(k = 0:8, prob = pk, lab = ifelse(pk < 0.005, "<.01", sprintf("%.2f", pk)))
pp <- ggplot(dp, aes(factor(k), prob)) +
  geom_col(width = 0.7, fill = PALE, colour = "grey35", linewidth = 0.4) +
  geom_text(aes(label = lab), vjust = -0.4, size = tsz(16), colour = NAVY, fontface = "bold") +
  scale_y_continuous(breaks = c(0, 0.1, 0.2), labels = c("0", ".1", ".2")) +
  coord_cartesian(ylim = c(0, 0.3), expand = FALSE) +
  labs(x = "k = goals in a match", y = "P(k)") +
  theme_classic() +
  theme(axis.title = el(16), axis.text = el(16), plot.margin = margin(8, 10, 2, 6))
save_fig(pp, "l15_poisson_goals.png", 640, 225)

write.csv(sizes, file.path(assets_dir, "l15_figure_sizes.csv"), row.names = FALSE)
cat("wrote", nrow(sizes), "figures to", assets_dir, "\n")
