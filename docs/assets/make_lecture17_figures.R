#Checks the numbers on the Week 7 Lecture 17 deck (10/9/2026): the rest of Lecture 16
#(Poisson numbers are asserted in make_lecture15_figures.R), two new binomial examples,
#and the normal distribution as a continuous distribution. No new figures; the Poisson
#figure is l15_poisson_goals.png.
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

#women's heights, N(65.4, 3.38) from Week 6, by the z-table (4 decimals)
HM <- 65.4; HS <- 3.38
z70 <- round((70 - HM) / HS, 2); z62 <- round((62 - HM) / HS, 2)
t70 <- round(pnorm(z70), 4); t62 <- round(pnorm(z62), 4)
stopifnot(z70 == 1.36, z62 == -1.01, t70 == 0.9131, t62 == 0.1562,
          near(1 - t70, 0.0869, 1e-9), near(t70 - t62, 0.7569, 1e-9),
          near(pnorm(70, HM, HS) - pnorm(62, HM, HS), 0.76, 5e-3))
cat("Lecture 17 numbers check out\n")
