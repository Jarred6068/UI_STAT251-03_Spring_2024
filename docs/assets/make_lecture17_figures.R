#Checks the numbers on the Week 7 Lecture 17 deck (10/9/2026): the rest of Lecture 16
#(Poisson numbers are asserted in make_lecture15_figures.R), two new binomial examples,
#and the normal distribution as a continuous distribution. The World Cup Poisson figure
#is l15_poisson_goals.png. Two "add up the bars" figures are drawn here:
#
#  l17_binom_quiz.png    Binom(6, 0.25): guessing on a 6-question, 4-option quiz
#  l17_pois_texts.png    Poisson(2): text messages in a 10-minute stretch of class
#  l17_mass_density.png  probability mass (packets) vs. probability density (area)
#  l17_figure_sizes.csv  the size, in points, each figure is drawn at
#
#Each bar is labelled with its probability to 3 decimals, and the slide answers are
#sums of those LABELS (what students add), asserted below against the exact values.
#Both scenarios are hypothetical. They deliberately avoid the help desk / pothole
#(Exam 2 bonus) and coffee stand (Homework 2) Poisson rates.
#
#TYPE SIZE. Same rule as the other make_lectureN_figures.R scripts: nothing on a slide
#is under 16pt, so each figure is drawn at exactly its on-slide size (points / 72
#inches) and every text size goes through tsz() or el(), which refuse anything smaller.
#
#SOURCES (checked Oct 7 2026)
#  Stephen Curry career free throws: 4,236 of 4,645 = .912 - Basketball-Reference,
#    curryst01, career totals (same as Lecture 15).
#  US lymphoma incidence, age-adjusted, 2018-2022 cases (SEER Cancer Stat Facts):
#    non-Hodgkin lymphoma 18.7 per 100,000 a year (seer.cancer.gov/statfacts/html/nhl.html)
#    Hodgkin lymphoma      2.5 per 100,000 a year (seer.cancer.gov/statfacts/html/hodg.html)
#    so about 21 per 100,000, p = 0.00021 (the slide rounds 21.2 to 21).
#  The doctor and his 71-of-893 patients are hypothetical (Jarred's scenario, kept at
#    8% on purpose: the point is that P(X >= 71) needs software, not hand work).

binom_pmf <- function(k, n, p) factorial(n) / (factorial(k) * factorial(n - k)) * p^k * (1 - p)^(n - k)
near <- function(a, b, tol = 5e-4) abs(a - b) < tol

#Curry: Binom(10, 0.912), P(X >= 2) = 1 - [P(0) + P(1)]
p_ft <- 4236 / 4645
stopifnot(near(p_ft, 0.912), near(0.912^10, 0.40, 5e-3))
low <- 0.088^10 + 10 * 0.912 * 0.088^9
stopifnot(near(low, 3e-9, 1e-10), near(binom_pmf(0, 10, p_ft) + binom_pmf(1, 10, p_ft), 3e-9, 1e-10),
          near(10 * 0.912, 9.1, 0.05))

#lymphoma: Binom(893, 0.00021), 71 cases (about 8%)
q <- 0.00021
stopifnot(near(71 / 893, 0.08, 0.005), near((18.7 + 2.5) / 1e5, q, 5e-6), near(893 * q, 0.19, 5e-3))
tail71 <- pbinom(70, 893, q, lower.tail = FALSE)          # 1 - pbinom(70, ...) underflows to 0
stopifnot(tail71 > 1.3e-155, tail71 < 1.5e-155,          # the slide: about 1 x 10^-155
          pbinom(70, 893, (18.7 + 2.5) / 1e5, lower.tail = FALSE) < 3e-155)

#World Cup goals, Poisson(172/64 = 2.69): (a) P(X = 0), (b) P(X > 1) = 1 - [P(0) + P(1)]
lam_wc <- 172 / 64
stopifnot(near(exp(-2.69), 0.068), near(2.69 * 0.068, 0.183),
          near(1 - (0.068 + 2.69 * 0.068), 0.749),                 # what the slide shows
          near(1 - ppois(1, lam_wc), 0.75, 1e-3), near(1 - ppois(1, 2.69), 0.75, 1e-3))

#women's heights, N(65.4, 3.38) from Week 6, by the z-table (4 decimals)
HM <- 65.4; HS <- 3.38
z70 <- round((70 - HM) / HS, 2); z62 <- round((62 - HM) / HS, 2)
t70 <- round(pnorm(z70), 4); t62 <- round(pnorm(z62), 4)
stopifnot(z70 == 1.36, z62 == -1.01, t70 == 0.9131, t62 == 0.1562,
          near(1 - t70, 0.0869, 1e-9), near(t70 - t62, 0.7569, 1e-9),
          near(pnorm(70, HM, HS) - pnorm(62, HM, HS), 0.76, 5e-3))
#----------------------------------------------------------------------------
#"add up the bars" figures
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
pois_pmf <- function(k, lambda) lambda^k * exp(-lambda) / factorial(k)

bar_fig <- function(k, prob, xlab, ymax) {
  d <- data.frame(k = k, prob = prob,
                  lab = ifelse(round(prob, 3) == 0, "<.001", sprintf("%.3f", prob)))
  ggplot(d, aes(factor(k), prob)) +
    geom_col(width = 0.7, fill = PALE, colour = "grey35", linewidth = 0.4) +
    geom_text(aes(label = lab), vjust = -0.4, size = tsz(16), colour = NAVY, fontface = "bold") +
    scale_y_continuous(breaks = c(0, 0.1, 0.2, 0.3), labels = c("0", ".1", ".2", ".3"),
                       expand = c(0, 0)) +
    scale_x_discrete(expand = expansion(add = 0.6)) +
    coord_cartesian(ylim = c(0, ymax)) +
    labs(x = xlab, y = "P(k)") +
    theme_classic() +
    theme(axis.title = el(16), axis.text = el(16), plot.margin = margin(8, 10, 2, 6))
}
W <- 640; H <- 215   # on-slide size in points

#quiz: Binom(6, 0.25), k = 0..6
bq <- round(binom_pmf(0:6, 6, 0.25), 3)
stopifnot(identical(bq, c(0.178, 0.356, 0.297, 0.132, 0.033, 0.004, 0)),
          near(bq[1] + bq[2], 0.534, 1e-9),                       # (a) P(X <= 1)
          near(1 - (bq[1] + bq[2] + bq[3]), 0.169, 1e-9),         # (b) P(X >= 3)
          near(sum(bq[4:7]), 0.169, 1e-9),                        #     same by adding bars
          near(bq[2] + bq[3] + bq[4], 0.785, 1e-9),               # (c) P(1 <= X <= 3)
          near(pbinom(1, 6, 0.25), 0.534, 1e-3),                  # labels agree with exact
          near(1 - pbinom(2, 6, 0.25), 0.169, 1e-3),
          near(pbinom(3, 6, 0.25) - pbinom(0, 6, 0.25), 0.785, 1.5e-3))
save_fig(bar_fig(0:6, binom_pmf(0:6, 6, 0.25), "k = questions right by guessing", 0.42),
         "l17_binom_quiz.png", W, H)

#texts: Poisson(2), k = 0..7
pt2 <- round(pois_pmf(0:7, 2), 3)
stopifnot(identical(pt2, c(0.135, 0.271, 0.271, 0.180, 0.090, 0.036, 0.012, 0.003)),
          near(pt2[1] + pt2[2], 0.406, 1e-9),                     # (a) P(X <= 1)
          near(1 - (pt2[1] + pt2[2] + pt2[3]), 0.323, 1e-9),      # (b) P(X >= 3)
          near(pt2[3] + pt2[4] + pt2[5], 0.541, 1e-9),            # (c) P(2 <= X <= 4)
          near(ppois(1, 2), 0.406, 1e-3),
          near(1 - ppois(2, 2), 0.323, 1e-3),
          near(ppois(4, 2) - ppois(1, 2), 0.541, 1e-3))
save_fig(bar_fig(0:7, pois_pmf(0:7, 2), "k = texts in 10 minutes", 0.32),
         "l17_pois_texts.png", W, H)

#----------------------------------------------------------------------------
#l17_mass_density.png: probability mass (packets at single values, the Poisson(2)
#texts example) next to probability density (the women's-heights normal curve, where
#only an AREA is a probability). Continuous distributions are taught through the
#normal only, so the density panel is the normal.
LAB <- "#C00000"
dm <- data.frame(k = 0:7, prob = pois_pmf(0:7, 2))
pm <- ggplot(dm, aes(factor(k), prob)) +
  geom_segment(aes(xend = factor(k), y = 0, yend = prob), colour = "grey45", linewidth = 0.6) +
  geom_point(size = 5, colour = NAVY) +
  annotate("text", x = 3, y = pois_pmf(2, 2) + 0.045, hjust = 0.3, size = tsz(16),
           colour = LAB, fontface = "bold", label = "P(X = 2) = 0.271") +
  scale_y_continuous(breaks = NULL, expand = c(0, 0)) +
  scale_x_discrete(expand = expansion(add = 0.6)) +
  coord_cartesian(ylim = c(0, 0.36)) +
  labs(title = "Discrete: mass", x = "k = texts in 10 minutes", y = NULL) +
  theme_classic() +
  theme(axis.title = el(16), axis.text = el(16), axis.line.y = element_blank(),
        plot.title = el(18, face = "bold", colour = NAVY), plot.margin = margin(4, 14, 2, 6))

lo <- 64; hi <- 67
stopifnot(near(pnorm(hi, HM, HS) - pnorm(lo, HM, HS), 0.343, 1e-3))
xs <- seq(HM - 3.5 * HS, HM + 3.5 * HS, length.out = 400)
dd <- data.frame(x = xs, y = dnorm(xs, HM, HS))
pd <- ggplot(dd, aes(x, y)) +
  geom_area(data = subset(dd, x >= lo & x <= hi), fill = PALE, colour = NA) +
  geom_line(linewidth = 0.9, colour = NAVY) +
  annotate("segment", x = 59.5, y = 0.062, xend = 64.9, yend = 0.04, colour = LAB,
           linewidth = 0.7, arrow = arrow(length = unit(6, "pt"))) +
  annotate("text", x = 57, y = 0.088, size = tsz(16), colour = LAB,
           fontface = "bold", label = "area =\nP(64 < X < 67)", hjust = 0.5, lineheight = 0.9) +
  scale_y_continuous(breaks = NULL, expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(56, 74, 6)) +
  coord_cartesian(ylim = c(0, max(dd$y) * 1.08)) +
  labs(title = "Continuous: density", x = "x = height (inches)", y = NULL) +
  theme_classic() +
  theme(axis.title = el(16), axis.text = el(16), axis.line.y = element_blank(),
        plot.title = el(18, face = "bold", colour = NAVY), plot.margin = margin(4, 6, 2, 14))

save_fig(patchwork::wrap_plots(pm, pd, nrow = 1), "l17_mass_density.png", 840, 215)

write.csv(sizes, file.path(assets_dir, "l17_figure_sizes.csv"), row.names = FALSE)
cat("Lecture 17 numbers check out; wrote", nrow(sizes), "figures to", assets_dir, "\n")
