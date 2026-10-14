# Run once if needed:
# install.packages(c("dplyr", "lubridate", "ggplot2", "car"))

library(dplyr)
library(lubridate)
library(ggplot2)
library(car)

# ------------------------------------------------------------
# 0. Map your imported data frames to clean names
#    (Run ls() to see the exact object names.)
# ------------------------------------------------------------
i44_raw    <- I44_Monthly_adjusted       # I44 data frame
vix_raw    <- VIX_Monthly_Average        # VIX data frame
brent_raw <- Brent_Monthly              # Brent data frame
usd_raw    <- Broad_US_Monthly_Average   # Broad USD data frame

# ------------------------------------------------------------
# 1. Clean I44 (Fixed Date parsing)
# ------------------------------------------------------------
i44 <- i44_raw %>%
  rename(Date = 1, I44 = 2) %>%
  mutate(
    Date  = as.character(Date),
    Month = floor_date(as.Date(paste0(substr(Date, 1, 7), "-01")), "month")
  ) %>%
  select(Month, I44)

# ------------------------------------------------------------
# 2. Clean Brent
# ------------------------------------------------------------
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

# ------------------------------------------------------------
# 4. Clean Broad USD
# ------------------------------------------------------------
usd <- usd_raw %>%
  rename(Date = 1, USD = 2) %>%
  mutate(
    Date  = as.Date(Date),
    USD   = as.numeric(as.character(USD)),
    Month = floor_date(Date, "month")
  ) %>%
  filter(!is.na(Date), !is.na(USD)) %>%
  select(Month, USD)

cat("I44 rows:  ", nrow(i44),  "\n")
cat("Brent rows:", nrow(brent), "\n")
cat("VIX rows:  ", nrow(vix),  "\n")
cat("USD rows:  ", nrow(usd),  "\n")

# ------------------------------------------------------------
# 5. Merge and construct variables
# ------------------------------------------------------------
df <- i44 %>%
  inner_join(brent, by = "Month") %>%
  inner_join(vix,   by = "Month") %>%
  inner_join(usd,   by = "Month") %>%
  arrange(Month) %>%
  mutate(
    r_NOK   = -100 * (log(I44)   - lag(log(I44))),
    r_Oil   =  100 * (log(Brent) - lag(log(Brent))),
    r_USD   =  100 * (log(USD)   - lag(log(USD))),
    VIX_lag = lag(VIX, 1)
  ) %>%
  filter(!is.na(r_NOK), !is.na(r_Oil),
         !is.na(r_USD), !is.na(VIX_lag))

cat("Usable monthly observations:", nrow(df), "\n")

# ============================================================
# 6. Linear model with USD control
# ============================================================

# Baseline (no USD) for comparison
model_lin_base <- lm(r_NOK ~ r_Oil, data = df)

# With USD control
model_lin_usd <- lm(r_NOK ~ r_Oil + r_USD, data = df)

cat("\n--- Linear model WITHOUT USD ---\n")
print(summary(model_lin_base)$coefficients)

cat("\n--- Linear model WITH USD ---\n")
print(summary(model_lin_usd)$coefficients)

cat("\nBrent coefficient (no USD):   ",
    round(coef(model_lin_base)["r_Oil"], 4), "\n")
cat("Brent coefficient (with USD):",
    round(coef(model_lin_usd)["r_Oil"], 4), "\n")
cat("USD coefficient:             ",
    round(coef(model_lin_usd)["r_USD"], 4), "\n")

# ============================================================
# 7. Re-estimate threshold c (same as Part 2)
# ============================================================

lo <- quantile(df$VIX_lag, 0.15)
hi <- quantile(df$VIX_lag, 0.85)
candidates <- seq(lo, hi, length.out = 100)

ssr_at <- function(c) {
  d <- df
  d$regime <- ifelse(d$VIX_lag <= c, "Low", "High")
  m <- lm(r_NOK ~ r_Oil * regime + r_USD, data = d)
  sum(resid(m)^2)
}

ssr_vals <- sapply(candidates, ssr_at)
c_hat    <- candidates[which.min(ssr_vals)]
cat("\nEstimated VIX threshold c (with USD):", round(c_hat, 2), "\n")

df <- df %>%
  mutate(regime = factor(ifelse(VIX_lag <= c_hat, "Low", "High"),
                         levels = c("Low", "High")))

cat("Regime counts:\n")
print(table(df$regime))

# ============================================================
# 8. Threshold model with USD control (common gamma)
# ============================================================

model3 <- lm(r_NOK ~ r_Oil * regime + r_USD, data = df)
summary(model3)

beta_L <- coef(model3)["r_Oil"]
beta_H <- coef(model3)["r_Oil"] + coef(model3)["r_Oil:regimeHigh"]
gamma  <- coef(model3)["r_USD"]

cat("\nbeta_L (low-risk regime):   ", round(beta_L, 4), "\n")
cat("beta_H (high-risk regime): ", round(beta_H, 4), "\n")
cat("gamma (USD coefficient):    ", round(gamma,  4), "\n")
cat("Difference beta_H - beta_L:", round(beta_H - beta_L, 4), "\n")

# ============================================================
# 9. Test H3: regime-dependent Brent effect remains
# ============================================================

# (a) Is the interaction term still significant after USD control?
summary(model3)$coefficients["r_Oil:regimeHigh", ]
linearHypothesis(model3, "r_Oil:regimeHigh = 0")

# (b) Compare Brent coefficients across models
cat("\n--- Comparison of Brent slopes across models ---\n")
cat("Linear, no USD:        ", round(coef(model_lin_base)["r_Oil"], 4), "\n")
cat("Linear, with USD:      ", round(coef(model_lin_usd)["r_Oil"],  4), "\n")
cat("Threshold, low risk:   ", round(beta_L, 4), "\n")
cat("Threshold, high risk:  ", round(beta_H, 4), "\n")

# ============================================================
# 10. Robustness: allow USD coefficient to vary by regime
# ============================================================

model3_flex <- lm(r_NOK ~ r_Oil * regime + r_USD * regime, data = df)
summary(model3_flex)

# Print all coefficient names to verify exact syntax in your R session
print(names(coef(model3_flex)))

# Extract coefficients safely using a foolproof search for r_USD and colon ":"
all_coefs <- coef(model3_flex)
gamma_L   <- unname(all_coefs["r_USD"])

# Find the interaction term containing both r_USD and a colon
interaction_name <- names(all_coefs)[grepl("r_USD", names(all_coefs)) & grepl(":", names(all_coefs))]
gamma_H   <- unname(gamma_L + all_coefs[interaction_name])

cat("\n--- Flexible model: USD also regime-dependent ---\n")
cat("USD coefficient (low risk):  ", round(gamma_L, 4), "\n")
cat("USD coefficient (high risk): ", round(gamma_H, 4), "\n")

# ============================================================
# 11. Plots
# ============================================================

ggplot(df, aes(x = r_Oil, y = r_NOK, color = regime)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "NOK return vs Brent return, by VIX regime",
    subtitle = paste0("Threshold c = ", round(c_hat, 2),
                      " (controlling for USD in the model)"),
    x = "Brent return (%)",
    y = "NOK return (%)",
    color = "Regime"
  ) +
  theme_minimal()

ggplot(df, aes(x = r_USD, y = r_NOK)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, color = "darkred") +
  labs(
    title = "NOK return vs broad USD return",
    x = "USD return (%)",
    y = "NOK return (%)"
  ) +
  theme_minimal()