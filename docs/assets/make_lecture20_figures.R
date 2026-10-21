#Checks the numbers on the Week 9 Lecture 20 deck (10/21/2026): confidence levels other
#than 95%, and the interval for a mean when sigma is unknown (the t distribution). It draws:
#
#  l20_crit_values.png  90%, 95% and 99% of a standard normal shaded, with the critical values
#  l20_width.png        one sample's interval at 80/90/95/99%, and at n = 126, 504, 2016
#  l20_s_coverage.png   100 intervals x-bar +/- 1.96 s / sqrt(5): about 12 miss, not 5
#  l20_t_coverage.png   the same 100 samples with 1.96 and with t = 2.776 (notes only)
#  l20_t_vs_z.png       t densities with 2, 5 and 30 degrees of freedom against N(0, 1)
#  l20_figure_sizes.csv the size, in points, each figure is drawn at
#
#Critical values are the hand-method values: z from the course z-table (1.645 sits halfway
#between the table's 0.9495 and 0.9505), t from the course t-table (misc/t_table.Rmd, three
#decimals). The CYP2C19 and watermelon files are written by hand from the sources below and
#read back here, so the slide numbers can never drift from the data.
#
#TYPE SIZE. Same rule as the other make_lectureN_figures.R scripts: nothing on a slide is
#under 16pt, so each figure is drawn at exactly its on-slide size (points / 72 inches) and
#every text size goes through tsz() or el(), which refuse anything smaller.
#
#SOURCES
#  CYP2C19: 1000 Genomes Project phase 3, rs4244285 (the CYP2C19*2 no-function allele), the
#    504 East Asian (EAS) individuals; genotypes from the Ensembl REST API
#    (rest.ensembl.org/variation/human/rs4244285?genotypes=1), population labels from the
#    phase 3 sample panel, checked October 20, 2026 -> cyp2c19_rs4244285_eas_1000g.csv.
#    Clinical meaning: Lee et al. (2022), CPIC guideline for CYP2C19 genotype and clopidogrel
#    therapy, Clin Pharmacol Ther 112:959-967 (avoid clopidogrel for ACS/PCI in carriers).
#  Seal whiskers: Ginter, DeWitt, Fish & Marshall (2012), PLoS ONE 7:e34481, Tables 1 and 2
#    (one whisker per seal; total length in mm, mean +/- SD). NOT whiskerMaster.csv, whose
#    rows are simulated on a log scale.
#  Watermelon: Assefa et al. (2020), Plants 9:1054, Table 3 (citrulline by HPLC, mg/g dry
#    weight; mean of triplicates) -> watermelon_citrulline_assefa2020.csv.

near <- function(a, b, tol = 5e-4) abs(a - b) < tol
zt <- function(z) round(pnorm(z), 4)            # a z-table lookup
tt <- function(C, df) round(qt(1 - (1 - C) / 2, df), 3)   # a t-table lookup

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

#critical values: area 1 - alpha/2 to the left
stopifnot(zt(1.64) == 0.9495, zt(1.65) == 0.9505,             # 0.95 halfway: 1.645
          zt(1.96) == 0.9750,
          zt(2.57) == 0.9949, zt(2.58) == 0.9951,             # 0.995 halfway: 2.576
          zt(1.28) == 0.8997,                                 # 0.90: 1.28
          round(qnorm(0.95), 3) == 1.645, round(qnorm(0.995), 3) == 2.576)
zc <- c(`80%` = 1.28, `90%` = 1.645, `95%` = 1.96, `99%` = 2.576)
#the 2024 "try it out": alpha = 0.25, 0.08, 0.03 -> 75%, 92%, 97%
stopifnot(round(qnorm(1 - 0.25 / 2), 2) == 1.15, round(qnorm(1 - 0.08 / 2), 2) == 1.75,
          round(qnorm(1 - 0.03 / 2), 2) == 2.17)

#t-table values used on the slides
stopifnot(tt(0.95, 9) == 2.262, tt(0.90, 24) == 1.711, tt(0.99, 9) == 3.250,
          tt(0.95, 4) == 2.776, tt(0.95, 8) == 2.306, tt(0.90, 11) == 1.796,
          tt(0.99, 11) == 3.106, tt(0.95, 29) == 2.045, tt(0.95, 100) == 1.984)

#Example 1: CYP2C19*2 carriers among 504 East Asian 1000 Genomes participants
cyp <- read.csv(file.path(assets_dir, "cyp2c19_rs4244285_eas_1000g.csv"))
x_c <- sum(cyp$carrier); n_c <- nrow(cyp)
ph_c <- round(x_c / n_c, 3); se_c <- round(sqrt(ph_c * (1 - ph_c) / n_c), 3)
me_c <- round(zc[c("90%", "95%", "99%")] * se_c, 3)
stopifnot(x_c == 277, n_c == 504, n_c - x_c == 227, ph_c == 0.550, se_c == 0.022,
          all(table(cyp$copies) == c(227, 239, 38)),
          identical(cyp$sample[1:5], c("HG00403", "HG00404", "HG00406", "HG00407", "HG00409")),
          identical(cyp$genotype[1:5], c("G/G", "A/A", "G/A", "G/G", "G/G")),
          all(near(me_c, c(0.036, 0.043, 0.057), 1e-9)),
          all(near(ph_c - me_c, c(0.514, 0.507, 0.493), 1e-9)),
          all(near(ph_c + me_c, c(0.586, 0.593, 0.607), 1e-9)))

#Example 2: spotted seal whiskers, n = 9 seals (Ginter et al. 2012)
n_w <- 9; xb_w <- 91.2; s_w <- 7.9
se_w <- round(s_w / sqrt(n_w), 2); me_w <- round(tt(0.95, n_w - 1) * se_w, 2)
stopifnot(se_w == 2.63, me_w == 6.06, near(xb_w - me_w, 85.14, 1e-9), near(xb_w + me_w, 97.26, 1e-9))

#Example 3: watermelon citrulline, a random sample of 12 of the 100 gene-bank accessions
wm <- read.csv(file.path(assets_dir, "watermelon_citrulline_assefa2020.csv"))
gb <- wm[wm$source == "gene bank", ]
set.seed(2026)
wsamp <- gb[sort(sample(nrow(gb), 12)), ]
xb_m <- round(mean(wsamp$citrulline), 2); s_m <- round(sd(wsamp$citrulline), 2)
se_m <- round(s_m / sqrt(12), 2)
me_m <- round(c(tt(0.90, 11), tt(0.99, 11)) * se_m, 2)
stopifnot(nrow(gb) == 100, near(mean(wm$citrulline), 27.3, 0.05),   # the paper's 27.3 average
          identical(wsamp$accession[1:5], c("IT120008", "IT199814", "IT203017", "IT203029", "IT203627")),
          xb_m == 27.07, s_m == 5.27, se_m == 1.52, all(near(me_m, c(2.73, 4.72), 1e-9)),
          near(xb_m - me_m[1], 24.34, 1e-9), near(xb_m + me_m[1], 29.80, 1e-9),
          near(xb_m - me_m[2], 22.35, 1e-9), near(xb_m + me_m[2], 31.79, 1e-9))

#----------------------------------------------------------------------------
suppressPackageStartupMessages(library(ggplot2))

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
  theme(axis.title.x = el(16), axis.text.x = el(16), axis.title.y = element_blank(),
        axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        strip.text = el(18, face = "bold", colour = NAVY), strip.background = element_blank(),
        panel.spacing = unit(28, "pt"), plot.margin = margin(4, 12, 2, 12))
MINUS <- "−"
neg <- function(s) sub("-", MINUS, s, fixed = TRUE)

#l20_crit_values.png: middle C shaded, alpha/2 in each tail
grid <- seq(-4.4, 4.4, length.out = 401)
lv <- data.frame(panel = c("90%", "95%", "99%"), z = zc[c("90%", "95%", "99%")],
                 tail = c("0.05", "0.025", "0.005"))
lv$panel <- factor(lv$panel, levels = lv$panel)
line <- do.call(rbind, lapply(seq_len(nrow(lv)), function(i)
  data.frame(panel = lv$panel[i], x = grid, y = dnorm(grid))))
fill <- do.call(rbind, lapply(seq_len(nrow(lv)), function(i) {
  g <- c(-lv$z[i], grid[abs(grid) <= lv$z[i]], lv$z[i])
  data.frame(panel = lv$panel[i], x = g, y = dnorm(g)) }))
p_crit <- ggplot() +
  geom_area(data = fill, aes(x, y), fill = SHADE, alpha = 0.8) +
  geom_line(data = line, aes(x, y), colour = NAVY, linewidth = 0.9) +
  geom_segment(data = rbind(transform(lv, x = -z), transform(lv, x = z)),
               aes(x = x, xend = x, y = 0, yend = dnorm(x) + 0.05), colour = RED, linewidth = 1) +
  geom_text(data = lv, aes(x = z + 0.05, y = dnorm(z) + 0.09, label = formatC(z, format = "f", digits = 3)),
            hjust = 0, size = tsz(16), colour = RED, fontface = "bold") +
  geom_text(data = lv, aes(x = -z - 0.05, y = dnorm(z) + 0.09,
                           label = neg(formatC(-z, format = "f", digits = 3))),
            hjust = 1, size = tsz(16), colour = RED, fontface = "bold") +
  geom_text(data = lv, aes(x = 0, y = 0.15, label = panel), size = tsz(18), colour = NAVY,
            fontface = "bold") +
  facet_wrap(~panel, nrow = 1, labeller = as_labeller(function(v)
    paste0(v, ":  ", c(`90%` = "0.05", `95%` = "0.025", `99%` = "0.005")[v], " in each tail"))) +
  scale_x_continuous(breaks = c(-3, 0, 3), labels = neg(c(-3, 0, 3))) +
  coord_cartesian(xlim = c(-4.4, 4.4), ylim = c(0, 0.47), expand = FALSE) +
  labs(x = NULL) + bare_x() + theme(axis.title.x = element_blank())
save_fig(p_crit, "l20_crit_values.png", 840, 210)

#l20_width.png: the CYP2C19 interval at four levels (n = 504), and at four n (95%)
wl <- data.frame(panel = "confidence level  (n = 504)", lab = names(zc), me = zc * se_c)
nn <- c(126, 504, 2016)
wn <- data.frame(panel = "sample size  (95%)", lab = paste0("n = ", nn),
                 me = 1.96 * sqrt(ph_c * (1 - ph_c) / nn))
stopifnot(all(near(head(wn$me, -1) / wn$me[-1], 2, 1e-9)))   # 4 times the n, half the width
w <- rbind(wl, wn); w$lab <- factor(w$lab, levels = rev(unique(c(names(zc), paste0("n = ", nn)))))
w$panel <- factor(w$panel, levels = unique(w$panel))
p_w <- ggplot(w, aes(y = lab)) +
  geom_vline(xintercept = ph_c, colour = "grey60", linetype = "dashed") +
  geom_segment(aes(x = ph_c - me, xend = ph_c + me, yend = lab), colour = NAVY, linewidth = 1.6) +
  geom_point(aes(x = ph_c), colour = NAVY, size = 2.4) +
  facet_wrap(~panel, nrow = 1, scales = "free_y") +
  scale_x_continuous(breaks = c(0.4, 0.5, 0.6, 0.7)) +
  coord_cartesian(xlim = c(0.38, 0.72)) +
  labs(x = "proportion of carriers") + bare_x() +
  theme(axis.text.y = el(16, colour = "black"))
save_fig(p_w, "l20_width.png", 840, 215)

#l20_s_coverage.png: N(91, 8) whisker lengths, 100 samples of n = 5, x-bar +/- 1.96 s / sqrt(5)
MU_w <- 91; SIG_w <- 8; n5 <- 5
set.seed(11)
sm <- replicate(100, { v <- rnorm(n5, MU_w, SIG_w); c(mean(v), sd(v)) })
cov <- data.frame(i = 1:100, m = sm[1, ], s = sm[2, ])
cov$lo_z <- cov$m - 1.96 * cov$s / sqrt(n5); cov$hi_z <- cov$m + 1.96 * cov$s / sqrt(n5)
cov$lo_t <- cov$m - tt(0.95, 4) * cov$s / sqrt(n5); cov$hi_t <- cov$m + tt(0.95, 4) * cov$s / sqrt(n5)
cov$hit_z <- cov$lo_z <= MU_w & MU_w <= cov$hi_z
cov$hit_t <- cov$lo_t <= MU_w & MU_w <= cov$hi_t
miss_z <- sum(!cov$hit_z); miss_t <- sum(!cov$hit_t)
stopifnot(miss_z == 12, miss_t == 5,
          near(2 * pt(1.96, 4) - 1, 0.878, 5e-4))   # the long-run capture rate with 1.96: 88%
cover_plot <- function(d, ylab) ggplot(d, aes(x = i)) +
  geom_hline(yintercept = MU_w, colour = NAVY, linewidth = 1) +
  geom_segment(aes(y = lo, yend = hi, xend = i, colour = hit, linewidth = hit)) +
  scale_colour_manual(values = c(`TRUE` = "grey60", `FALSE` = RED), guide = "none") +
  scale_linewidth_manual(values = c(`TRUE` = 0.5, `FALSE` = 1.1), guide = "none") +
  coord_cartesian(xlim = c(0, 101), ylim = c(66, 116), expand = FALSE) +
  scale_y_continuous(breaks = c(70, 91, 110)) +
  labs(x = "sample number", y = ylab) + theme_classic() +
  theme(axis.title = el(16), axis.text = el(16), strip.text = el(18, face = "bold", colour = NAVY),
        strip.background = element_blank(), plot.margin = margin(4, 12, 2, 4))
dz <- transform(cov, lo = lo_z, hi = hi_z, hit = hit_z)
save_fig(cover_plot(dz, "mm") +
           annotate("text", x = 101, y = 113, hjust = 1, label = "red: misses μ = 91",
                    colour = RED, size = tsz(16), fontface = "bold"),
         "l20_s_coverage.png", 760, 215)
dzt <- rbind(transform(dz, panel = paste0("x̄ ± 1.96 s/√5:  ", miss_z, " of 100 miss")),
             transform(cov, lo = lo_t, hi = hi_t, hit = hit_t,
                       panel = paste0("x̄ ± 2.776 s/√5:  ", miss_t, " of 100 miss")))
dzt$panel <- factor(dzt$panel, levels = unique(dzt$panel))
save_fig(cover_plot(dzt, "mm") + facet_wrap(~panel, ncol = 1), "l20_t_coverage.png", 760, 420)

#l20_t_vs_z.png
gx <- seq(-4, 4, length.out = 401)
dens <- rbind(data.frame(curve = "N(0, 1)", x = gx, y = dnorm(gx)),
              do.call(rbind, lapply(c(2, 5, 30), function(d)
                data.frame(curve = paste0("t, df = ", d), x = gx, y = dt(gx, d)))))
dens$curve <- factor(dens$curve, levels = c("t, df = 2", "t, df = 5", "t, df = 30", "N(0, 1)"))
cols <- c(`t, df = 2` = RED, `t, df = 5` = "#E69F00", `t, df = 30` = "#009E73", `N(0, 1)` = NAVY)
labs_at <- data.frame(curve = factor(levels(dens$curve), levels = levels(dens$curve)),
                      x = c(2.55, 2.55, 2.55, 2.55), y = c(0.37, 0.31, 0.25, 0.19))
p_t <- ggplot(dens, aes(x, y, colour = curve)) +
  geom_line(aes(linewidth = curve == "N(0, 1)")) +
  geom_text(data = labs_at, aes(x, y, label = curve), hjust = 0, size = tsz(16), fontface = "bold") +
  scale_colour_manual(values = cols, guide = "none") +
  scale_linewidth_manual(values = c(`TRUE` = 1.4, `FALSE` = 0.9), guide = "none") +
  scale_x_continuous(breaks = -4:4, labels = neg(-4:4)) +
  coord_cartesian(xlim = c(-4, 4.6), ylim = c(0, 0.42), expand = FALSE) +
  labs(x = NULL) + bare_x() + theme(axis.title.x = element_blank())
stopifnot(dt(0, 2) < dt(0, 5), dt(0, 5) < dt(0, 30), dt(0, 30) < dnorm(0),
          dt(3, 2) > dt(3, 5), dt(3, 5) > dt(3, 30), dt(3, 30) > dnorm(3))
save_fig(p_t, "l20_t_vs_z.png", 640, 215)

write.csv(sizes, file.path(assets_dir, "l20_figure_sizes.csv"), row.names = FALSE)
cat("Lecture 20 numbers check out; wrote", nrow(sizes), "figures to", assets_dir, "\n")
