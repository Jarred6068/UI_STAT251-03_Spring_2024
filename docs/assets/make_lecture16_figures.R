#Checks the numbers on the Week 7 Lecture 16 deck (10/7/2026). Lecture 16 is the
#expected value, Bernoulli and binomial slides carried from Lecture 15 (their numbers
#are asserted in make_lecture15_figures.R) plus one new slide, so this script draws
#no figures; it only asserts the new slide's arithmetic.
#
#Lecture 16 stopped at its slide 11 on Oct 7 2026. Its continuous-distribution slides
#(the uniform shuttle wait and the triangle density, with figures l16_uniform_wait.png
#and l16_triangle.png) were dropped when the rest of the deck moved into Lecture 17,
#which covers only the normal distribution; see make_lecture17_figures.R. Both
#examples are still written up in the Week 7 notes.

binom_pmf <- function(k, n, p) factorial(n) / (factorial(k) * factorial(n - k)) * p^k * (1 - p)^(n - k)

#counting vs. the formula (slide 9): X = heads in 3 tosses, from the 8 outcomes and Binom(3, 0.5)
S <- c("HHH", "HHT", "HTH", "THH", "HTT", "THT", "TTH", "TTT")
heads <- nchar(gsub("T", "", S))
stopifnot(mean(heads == 3) == 1 / 8, mean(heads == 2) == 3 / 8,
          binom_pmf(3, 3, 0.5) == 1 / 8, binom_pmf(2, 3, 0.5) == 3 / 8, choose(3, 2) == 3)
cat("Lecture 16 numbers check out\n")
