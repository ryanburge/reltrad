library(tidyverse)
library(haven)

# Run R_Coding_GSS first to produce the `gss` object with reltrad coded,
# then run this script to generate verification counts.
#
# Usage:
#   source("R_Coding_GSS")
#   source("generate_verification_counts.R")

years <- sort(unique(gss$year))

counts <- gss %>%
  filter(!is.na(year), !is.na(reltrad)) %>%
  group_by(year, reltrad) %>%
  summarise(n = n(), .groups = "drop") %>%
  pivot_wider(names_from = reltrad, values_from = n, values_fill = 0) %>%
  arrange(year)

# Print a clean table to console — paste this output into verification.txt
print(counts, n = Inf)
