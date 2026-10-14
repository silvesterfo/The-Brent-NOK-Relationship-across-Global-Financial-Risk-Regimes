# Run once if not installed:
install.packages(c("dplyr", "lubridate", "ggplot2"))

library(dplyr)
library(lubridate)
library(ggplot2)

# --- 0. Rename your imported data frames here -----------------
# Change these to match the names shown in your Environment pane.
i44_raw   <- I44_Monthly_adjusted
brent_raw <- Brent_Monthly

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

# --- 3. Merge and compute returns ----------------------------
df <- i44 %>%
  inner_join(brent, by = "Month") %>%
  arrange(Month) %>%
  mutate(
    r_NOK = -100 * (log(I44)   - lag(log(I44))),
    r_Oil =  100 * (log(Brent) - lag(log(Brent)))
  ) %>%
  filter(!is.na(r_NOK), !is.na(r_Oil))

cat("Number of usable monthly observations:", nrow(df), "\n")
print(head(df))

# --- 4. Baseline regression ----------------------------------
model1 <- lm(r_NOK ~ r_Oil, data = df)
summary(model1)

# One-sided test for H1: beta_Oil > 0
tstat    <- coef(summary(model1))["r_Oil", "t value"]
df_resid <- model1$df.residual
p_one_sided <- pt(tstat, df_resid, lower.tail = FALSE)
cat("One-sided p-value for H1 (beta_Oil > 0):", p_one_sided, "\n")

# --- 5. Plot -------------------------------------------------
ggplot(df, aes(x = r_Oil, y = r_NOK)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(
    title = "NOK return vs Brent oil return (Monthly)",
    x = "Brent return (%)",
    y = "NOK return (%)"
  ) +
  theme_minimal()
