# Run once if needed:
install.packages(c("dplyr", "lubridate", "ggplot2", "car"))

library(dplyr)
library(lubridate)
library(ggplot2)
library(car)

# ------------------------------------------------------------
# 0. Map your imported data frames to clean names
#    Change the names on the RIGHT to match your Environment.
#    (Run ls() in R to see the exact object names.)
# ------------------------------------------------------------
i44_raw    <- I44_Monthly_adjusted   # I44 data frame
vix_raw    <- VIX_Monthly_Average    # VIX data frame
brent_raw <- Brent_Monthly            # Brent data frame

# --- 1. Clean I44 --------------------------------------------
i44 <- i44_raw %>%
  rename(Date = 1, I44 = 2) %>%
  mutate(
    Date  = as.character(Date),
    Month = floor_date(as.Date(paste0(substr(Date, 1, 7), "-01")), "month")
  ) %>%
  select(Month, I44)

# --- 2. Clean Brent ------------------------------------------
# Fixed: Convert Date to Date format before applying floor_date()
brent <- brent_raw %>%
  rename(Date = 1, Brent = 2) %>%
  mutate(
    Date  = as.Date(Date),
    Month = floor_date(Date, "month")
  ) %>%
  select(Month, Brent)

# ------------------------------------------------------------
# 3. Clean VIX
# ------------------------------------------------------------
vix <- vix_raw %>%
  rename(Date = 1, VIX = 2) %>%
  mutate(
    Date  = as.Date(Date),
    VIX   = as.numeric(as.character(VIX)),
    Month = floor_date(Date, "month")
  ) %>%
  filter(!is.na(Date), !is.na(VIX)) %>%
  select(Month, VIX)

cat("I44 rows:  ", nrow(i44),  "\n")
cat("Brent rows:", nrow(brent), "\n")
cat("VIX rows:  ", nrow(vix),  "\n")

# ------------------------------------------------------------
# 4. Merge and construct variables
# ------------------------------------------------------------
df <- i44 %>%
  inner_join(brent, by = "Month") %>%
  inner_join(vix,   by = "Month") %>%
  arrange(Month) %>%
  mutate(
    r_NOK   = -100 * (log(I44)   - lag(log(I44))),
    r_Oil   =  100 * (log(Brent) - lag(log(Brent))),
    VIX_lag = lag(VIX, 1)          # regime variable
  ) %>%
  filter(!is.na(r_NOK), !is.na(r_Oil), !is.na(VIX_lag))

cat("Usable monthly observations:", nrow(df), "\n")
cat("VIX range (lagged):", round(range(df$VIX_lag), 2), "\n")

# ============================================================
# 5. Estimate threshold c by minimizing SSR
# ============================================================

lo <- quantile(df$VIX_lag, 0.15)
hi <- quantile(df$VIX_lag, 0.85)
candidates <- seq(lo, hi, length.out = 100)

ssr_at <- function(c) {
  d <- df
  d$regime <- ifelse(d$VIX_lag <= c, "Low", "High")
  m <- lm(r_NOK ~ r_Oil * regime, data = d)
  sum(resid(m)^2)
}

ssr_vals <- sapply(candidates, ssr_at)
c_hat    <- candidates[which.min(ssr_vals)]

cat("Estimated VIX threshold c:", round(c_hat, 2), "\n")

# SSR curve
plot(candidates, ssr_vals, type = "l",
     xlab = "Candidate threshold c (VIX)",
     ylab = "Residual sum of squares",
     main = "Threshold estimation: SSR(c)")
abline(v = c_hat, col = "red", lty = 2)

# ============================================================
# 6. Estimate threshold model
# ============================================================

df <- df %>%
  mutate(regime = factor(ifelse(VIX_lag <= c_hat, "Low", "High"),
                         levels = c("Low", "High")))

cat("\nRegime counts:\n")
print(table(df$regime))

model2 <- lm(r_NOK ~ r_Oil * regime, data = df)
summary(model2)

beta_L <- coef(model2)["r_Oil"]
beta_H <- coef(model2)["r_Oil"] + coef(model2)["r_Oil:regimeHigh"]

cat("\nbeta_L (low-risk regime):  ", round(beta_L, 4), "\n")
cat("beta_H (high-risk regime): ", round(beta_H, 4), "\n")
cat("Difference beta_H - beta_L:", round(beta_H - beta_L, 4), "\n")

# ============================================================
# 7. Test H2: beta_L != beta_H
# ============================================================

summary(model2)$coefficients["r_Oil:regimeHigh", ]
linearHypothesis(model2, "r_Oil:regimeHigh = 0")

# ============================================================
# 8. Robustness: median split
# ============================================================

median_vix <- median(df$VIX_lag)

df <- df %>%
  mutate(regime_med = factor(ifelse(VIX_lag <= median_vix, "Low", "High"),
                             levels = c("Low", "High")))

model2_median <- lm(r_NOK ~ r_Oil * regime_med, data = df)
summary(model2_median)

beta_L_med <- coef(model2_median)["r_Oil"]
beta_H_med <- coef(model2_median)["r_Oil"] +
  coef(model2_median)["r_Oil:regime_medHigh"]

cat("\n--- Median split robustness ---\n")
cat("Median VIX:                ", round(median_vix, 2), "\n")
cat("beta_L (low-risk regime):  ", round(beta_L_med, 4), "\n")
cat("beta_H (high-risk regime): ", round(beta_H_med, 4), "\n")

# ============================================================
# 9. Plots
# ============================================================

ggplot(df, aes(x = VIX_lag, y = r_NOK, color = regime)) +
  geom_point(alpha = 0.6) +
  geom_vline(xintercept = c_hat, linetype = "dashed") +
  labs(
    title = "NOK return vs lagged VIX, by regime",
    subtitle = paste0("Estimated threshold c = ", round(c_hat, 2)),
    x = "Lagged VIX",
    y = "NOK return (%)",
    color = "Regime"
  ) +
  theme_minimal()

ggplot(df, aes(x = r_Oil, y = r_NOK, color = regime)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "NOK return vs Brent return, by VIX regime",
    subtitle = paste0("Threshold c = ", round(c_hat, 2)),
    x = "Brent return (%)",
    y = "NOK return (%)",
    color = "Regime"
  ) +
  theme_minimal()