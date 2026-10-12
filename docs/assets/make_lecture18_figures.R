#Checks the numbers on the Week 8 Lecture 18 deck (10/12/2026): sampling distributions,
#the central limit theorem and an introduction to confidence intervals. It draws:
#
#  l18_spinner.png      one spin of a 1-2-3 spinner vs. the mean of two spins
#  l18_clt_goals.png    10,000 sample means of minutes until the next goal, n = 1, 2, 5, 30
#  l18_phat_curry.png   exact sampling distributions of Curry's p-hat, n = 10, 30, 100
#  l18_ci_cover.png     25 intervals x-bar +/- 2 SE from samples of 20 songs
#  l18_figure_sizes.csv the size, in points, each figure is drawn at
#
#The CLT goal-wait and World Cup numbers match the Week 7 notes (lecturemenu7.Rmd,
#"Sampling distributions and the central limit theorem"). The song-length numbers (mu = 3.71,
#sigma = 1.51, SE = 0.34, sample 1 x-bar = 3.64) come from Spotify_Youtube.csv; they
#replaced the overused heights data on Oct 12 2026.
#
#TYPE SIZE. Same rule as the other make_lectureN_figures.R scripts: nothing on a slide
#is under 16pt, so each figure is drawn at exactly its on-slide size (points / 72
#inches) and every text size goes through tsz() or el(), which refuse anything smaller.
#
#SOURCES (checked Oct 12 2026)
#  2022 FIFA World Cup: 172 goals in 64 matches (as Lecture 17).
#  2018 FIFA World Cup: 169 goals in 64 matches, 2.64 per match - Wikipedia, "2018 FIFA
#    World Cup statistics".
#  Stephen Curry career free throws: 4,236 of 4,645 = .912 (as Lecture 17).
#  Spotify songs: Spotify_Youtube.csv (Kaggle "Spotify and Youtube", collected Feb 2023).
#  The spinner and the two polls are hypothetical; the goal waits are simulated.

near <- function(a, b, tol = 5e-4) abs(a - b) < tol

#spinner: X = 1, 2, 3 each 1/3; x-bar of n = 2 spins, 9 equally likely ordered samples
sp <- expand.grid(a = 1:3, b = 1:3)
sp$m <- (sp$a + sp$b) / 2
spd <- as.data.frame(table(sp$m)); names(spd) <- c("m", "count")
spd$m <- as.numeric(as.character(spd$m)); spd$p <- spd$count / 9
s_mu <- 2; s_sd <- sqrt(mean(((1:3) - 2)^2))
sb_mu <- sum(spd$m * spd$p); sb_sd <- sqrt(sum((spd$m - sb_mu)^2 * spd$p))
stopifnot(nrow(sp) == 9, identical(spd$count, c(1L, 2L, 3L, 2L, 1L)),
          near(sb_mu, 2, 1e-12), near(s_sd, 0.82, 5e-3), near(sb_sd, 0.58, 5e-3),
          near(sb_sd, s_sd / sqrt(2), 1e-12))


#Curry p-hat, n = 100
p_ft <- 0.912
se_ft <- sqrt(p_ft * (1 - p_ft) / 100)
stopifnot(near(4236 / 4645, p_ft), near(se_ft, 0.028, 5e-4),
          near(p_ft - 2 * 0.028, 0.856, 1e-9), near(p_ft + 2 * 0.028, 0.968, 1e-9),
          near(c(10, 30, 100) * (1 - p_ft), c(0.88, 2.64, 8.8), 1e-9))

#World Cup: Poisson(2.69) goals per match, tournament average of 64 matches
lam <- 2.69
se_wc <- sqrt(lam) / sqrt(64)
stopifnot(near(172 / 64, lam, 5e-3), near(sqrt(lam), 1.64, 5e-3), near(se_wc, 0.205, 5e-4),
          near(lam - 2 * 0.205, 2.28, 1e-9), near(lam + 2 * 0.205, 3.10, 1e-9),
          near(169 / 64, 2.64, 5e-3))

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
data_dir <- file.path(dirname(assets_dir), "Data")

NAVY <- "#1F3864"; PALE <- "#D9E5F7"; RED <- "#C00000"
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
        axis.line.y = element_blank(), plot.title = el(16, hjust = 0.5, colour = NAVY, face = "bold"),
        strip.text = el(16, face = "bold", colour = NAVY), strip.background = element_blank(),
        plot.margin = margin(4, 12, 2, 12))

#songs: the population of 18,860 Spotify songs (Spotify_Youtube.csv, Kaggle, collected
#February 2023: the top 10 Spotify tracks of each of 2,079 artists). A song credited to
#several artists appears once per artist, so duplicates by Spotify URI are dropped.
#Replaced the Lecture 13 heights data on Oct 12 2026 - heights had been used too often.
sg <- read.csv(file.path(data_dir, "Spotify_Youtube.csv"), stringsAsFactors = FALSE)
stopifnot(nrow(sg) == 20718, length(unique(sg$Artist)) == 2079)   # before de-duplicating
sg <- sg[!duplicated(sg$Uri) & !is.na(sg$Duration_ms), ]
x <- sg$Duration_ms / 60000                     # song length in minutes
MU <- mean(x); SIG <- sd(x); SE <- SIG / sqrt(20)
stopifnot(length(x) == 18860,
          round(MU, 2) == 3.71, round(SIG, 2) == 1.51,
          near(1.51 / sqrt(20), 0.34, 5e-3), near(1.51 / sqrt(80), 0.17, 5e-3),
          near(sqrt(80 / 20), 2, 1e-12), round(SE, 2) == 0.34)
#the first five rows, as printed on the "Data: Spotify songs" slide
stopifnot(identical(sg$Track[1:2], c("Feel Good Inc.", "Rhinestone Eyes")),
          all(sg$Artist[1:5] == "Gorillaz"),
          identical(round(x[1:5], 2), c(3.71, 3.34, 3.59, 3.90, 5.68)))

#----------------------------------------------------------------------------
#l18_spinner.png
sp_pop <- data.frame(m = 1:3, p = 1 / 3, lab = "1/3", panel = "one spin:  X")
sp_bar <- data.frame(m = spd$m, p = spd$p, lab = paste0(spd$count, "/9"),
                     panel = "mean of two spins:  x̄")
spall <- rbind(sp_pop, sp_bar)
spall$panel <- factor(spall$panel, levels = unique(spall$panel))
spall$w <- ifelse(spall$panel == levels(spall$panel)[1], 0.6, 0.3)
p_sp <- ggplot(spall, aes(m, p)) +
  geom_col(aes(width = w), fill = PALE, colour = "grey35", linewidth = 0.4) +
  geom_text(aes(label = lab), vjust = -0.3, size = tsz(16), colour = NAVY, fontface = "bold") +
  facet_wrap(~panel, nrow = 1) +
  scale_x_continuous(breaks = c(1, 1.5, 2, 2.5, 3), labels = c("1", "1.5", "2", "2.5", "3")) +
  coord_cartesian(xlim = c(0.6, 3.4), ylim = c(0, 0.45), expand = FALSE) +
  labs(x = NULL) + bare_x()
save_fig(p_sp, "l18_spinner.png", 640, 215)

#l18_clt_goals.png: same simulation as the Week 7 notes (seed 7)
gap <- 90 / lam
set.seed(7)
clt_n <- c(1, 2, 5, 30)
clt <- do.call(rbind, lapply(clt_n, function(n)
  data.frame(n = paste0("n = ", n), m = replicate(10000, mean(rexp(n, 1 / gap))))))
stopifnot(identical(round(clt$m[1:5], 1), c(1.6, 54.3, 57.3, 27.2, 18.3)))  # "Data: waiting for a goal" table
clt$n <- factor(clt$n, levels = paste0("n = ", clt_n))
chk <- tapply(clt$m, clt$n, sd) / (gap / sqrt(clt_n))
stopifnot(near(gap, 33.5, 0.05), all(abs(tapply(clt$m, clt$n, mean) - gap) < 0.6),
          all(abs(chk - 1) < 0.03))
p_clt <- ggplot(clt, aes(m)) +
  geom_histogram(bins = 30, fill = PALE, colour = "grey35", linewidth = 0.2) +
  geom_vline(xintercept = gap, colour = RED, linewidth = 0.8) +
  facet_wrap(~n, nrow = 1, scales = "free") +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 3)) +
  labs(x = "x̄ = mean minutes until the next goal") + bare_x()
save_fig(p_clt, "l18_clt_goals.png", 840, 215)

#l18_phat_curry.png: exact Binom(n, 0.912) / n
ph_n <- c(10, 30, 100)
ph <- do.call(rbind, lapply(ph_n, function(n)
  data.frame(n = paste0("n = ", n), ph = (0:n) / n, p = dbinom(0:n, n, p_ft), w = 0.7 / n)))
ph$n <- factor(ph$n, levels = paste0("n = ", ph_n))
p_ph <- ggplot(ph[ph$p > 1e-3, ], aes(ph, p)) +
  geom_col(aes(width = w), fill = PALE, colour = "grey35", linewidth = 0.2) +
  geom_vline(xintercept = p_ft, colour = RED, linewidth = 0.8) +
  facet_wrap(~n, nrow = 1, scales = "free") +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 3)) +
  labs(x = "p̂ = proportion of free throws made") + bare_x()
save_fig(p_ph, "l18_phat_curry.png", 840, 215)

#l18_ci_cover.png: 25 samples of 20 songs, each x-bar +/- 2(0.34). Sample 1 is the one
#used on the "interval for mean song length" slide.
set.seed(10)
cm <- replicate(25, mean(sample(x, 20, replace = TRUE)))
ci <- data.frame(i = 1:25, m = cm, lo = cm - 2 * 0.34, hi = cm + 2 * 0.34)
ci$hit <- ci$lo <= MU & MU <= ci$hi
stopifnot(sum(ci$hit) == 24, round(cm[1], 2) == 3.64, ci$hit[1],
          near(3.64 - 2 * 0.34, 2.96, 1e-9), near(3.64 + 2 * 0.34, 4.32, 1e-9),
          3.64 - 0.68 <= 3.71, 3.71 <= 3.64 + 0.68)
p_ci <- ggplot(ci, aes(y = i)) +
  geom_vline(xintercept = MU, colour = NAVY, linewidth = 1) +
  geom_segment(aes(x = lo, xend = hi, yend = i, colour = hit, linewidth = hit)) +
  geom_point(aes(x = m, colour = hit), size = 1.8) +
  geom_text(data = ci[!ci$hit, ], aes(x = ifelse(m > MU, hi + 0.05, lo - 0.05),
            hjust = ifelse(m > MU, 0, 1)), label = "misses", size = tsz(16),
            colour = RED, fontface = "bold") +
  annotate("text", x = MU + 0.04, y = 27.4, label = "mu == 3.71", parse = TRUE, size = tsz(16), hjust = 0,
           colour = NAVY) +
  scale_colour_manual(values = c(`TRUE` = "grey35", `FALSE` = RED), guide = "none") +
  scale_linewidth_manual(values = c(`TRUE` = 0.7, `FALSE` = 1.4), guide = "none") +
  coord_cartesian(xlim = c(MU - 1.3, MU + 2.1), ylim = c(0, 29), expand = FALSE) +
  labs(x = "song length (minutes)") + bare_x()
save_fig(p_ci, "l18_ci_cover.png", 640, 215)

write.csv(sizes, file.path(assets_dir, "l18_figure_sizes.csv"), row.names = FALSE)
cat("Lecture 18 numbers check out; wrote", nrow(sizes), "figures to", assets_dir, "\n")
