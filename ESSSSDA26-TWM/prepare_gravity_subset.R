# Builds a balanced bilateral-trade panel for the Two-Way Mundlak applied
# example from the raw CEPII Gravity dataset (Gravity.csv).
#
# Why balanced: the textbook Mundlak transform (regress on X plus simple
# pair-mean and year-mean of X) is only algebraically identical to two-way
# fixed effects when the panel is balanced. On the raw (unbalanced) trade
# data the two estimators diverge (~0.056 vs ~0.154 in testing); on a
# balanced sub-panel they match to numerical precision (~1e-05). Since the
# whole point of this section is to demonstrate exact equivalence, we
# restrict to pairs observed in every year of the window.

suppressPackageStartupMessages(library(data.table))

path_in  <- "/Users/rwalker/MBA-Survey/Gravity.csv"
path_out <- "/Users/rwalker/MBA-Survey/gravity_twm.csv"

cols_needed <- c("year", "iso3_o", "iso3_d", "dist", "contig",
                  "comlang_off", "comcol", "fta_wto", "tradeflow_baci",
                  "gdp_o", "gdp_d")

dt <- fread(path_in, select = cols_needed, na.strings = c("", "NA"))

sub <- dt[iso3_o != iso3_d &
          !is.na(tradeflow_baci) & tradeflow_baci > 0 &
          !is.na(comlang_off) & !is.na(comcol) &
          !is.na(gdp_o) & gdp_o > 0 & !is.na(gdp_d) & gdp_d > 0 &
          year >= 2010 & year <= 2019]

sub[, pair := paste(iso3_o, iso3_d, sep = "_")]

# Keep only pairs observed in all 10 years of the window (balanced panel)
counts <- sub[, .N, by = pair]
balanced_pairs <- counts[N == 10, pair]
bal <- sub[pair %in% balanced_pairs]

bal[, `:=`(
  log_trade       = log(tradeflow_baci),
  fta             = fta_wto,
  log_distance    = log(dist),
  common_language = comlang_off,
  contiguity      = contig,
  colony          = comcol,
  log_gdp_o       = log(gdp_o),
  log_gdp_d       = log(gdp_d)
)]

out <- bal[, .(pair, iso3_o, iso3_d, year, log_trade, fta,
               log_distance, common_language, contiguity, colony,
               log_gdp_o, log_gdp_d)]

fwrite(out, path_out)

cat("Wrote", nrow(out), "rows,", length(unique(out$pair)), "pairs,",
    "years", min(out$year), "-", max(out$year), "to", path_out, "\n")
cat("File size:", file.size(path_out) / 1e6, "MB\n")
