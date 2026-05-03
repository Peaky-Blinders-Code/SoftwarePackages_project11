# ==============================================================================
# PROJECT 11: DRIVING DATA ANALYSIS (CLEAN VERSION)
# ==============================================================================

# --- 1. Load Data and Libraries ---
library(wooldridge)
data("driving")
df <- driving

# ==============================================================================
# PART 1: DESCRIPTIVE STATISTICS
# ==============================================================================

# --- Central Tendency ---
avg_accidents <- mean(df$totfatrte, na.rm = TRUE)
med_accidents <- median(df$totfatrte, na.rm = TRUE)

cat("--- Central Tendency ---\n")
cat("Mean Accident Rate: ", avg_accidents, "\n")
cat("Median Accident Rate: ", med_accidents, "\n\n")

# --- Variability ---
sd_accidents <- sd(df$totfatrte, na.rm = TRUE)
min_val <- min(df$totfatrte, na.rm = TRUE)
max_val <- max(df$totfatrte, na.rm = TRUE)

cat("--- Variability ---\n")
cat("Standard Deviation: ", sd_accidents, "\n")
cat("Range: ", min_val, "to", max_val, "\n\n")

# --- Distribution Shape (approximation) ---
shape <- ifelse(avg_accidents > med_accidents,
                "Positively Skewed (Right-Skewed)",
                "Negatively Skewed (Left-Skewed)")

cat("--- Distribution Shape ---\n")
cat("The distribution is:", shape, "\n\n")

# --- Outlier Detection (IQR method) ---
Q1 <- quantile(df$totfatrte, 0.25, na.rm = TRUE)
Q3 <- quantile(df$totfatrte, 0.75, na.rm = TRUE)
IQR_val <- Q3 - Q1

lower_bound <- Q1 - 1.5 * IQR_val
upper_bound <- Q3 + 1.5 * IQR_val

outliers <- df$totfatrte[
  df$totfatrte < lower_bound | df$totfatrte > upper_bound
]

cat("--- Outliers ---\n")
print(outliers)
cat("\n")

# --- Seatbelt Grouping ---
med_seatbelt <- median(df$seatbelt, na.rm = TRUE)

df$seatbelt_group <- ifelse(df$seatbelt > med_seatbelt,
                            "Higher Usage",
                            "Lower Usage")

# --- Group Comparison ---
comparison_mean <- aggregate(totfatrte ~ seatbelt_group, data = df, mean)
comparison_sd <- aggregate(totfatrte ~ seatbelt_group, data = df, sd)

cat("--- Group Comparison ---\n")
print(comparison_mean)
print(comparison_sd)

# --- Visualizations ---
par(mfrow = c(1, 2))

hist(df$totfatrte,
     col = "steelblue",
     main = "Fatality Rate Distribution",
     xlab = "Fatality Rate")

abline(v = avg_accidents, col = "red", lwd = 2)
abline(v = med_accidents, col = "yellow", lwd = 2, lty = 2)

legend("topright",
       legend = c("Mean", "Median"),
       col = c("red", "yellow"),
       lty = c(1, 2),
       lwd = 2)

boxplot(df$totfatrte,
        col = "lightgreen",
        main = "Boxplot of Fatality Rates",
        ylab = "Fatality Rate")

par(mfrow = c(1, 1))

# ==============================================================================
# PART 2: PROBABILITY ANALYSIS
# ==============================================================================

prob_high <- mean(df$totfatrte > 25, na.rm = TRUE)
prob_safe <- mean(df$totfatrte < 12, na.rm = TRUE)

low_group <- df$totfatrte[df$seatbelt_group == "Lower Usage"]
high_group <- df$totfatrte[df$seatbelt_group == "Higher Usage"]

prob_high_given_low_sb <- mean(low_group > 25, na.rm = TRUE)
prob_high_given_high_sb <- mean(high_group > 25, na.rm = TRUE)

cat("--- Probability Results ---\n")
cat("P(Accident > 25):", prob_high, "\n")
cat("P(Accident < 12):", prob_safe, "\n")
cat("P(High | Low Seatbelt):", prob_high_given_low_sb, "\n")
cat("P(High | High Seatbelt):", prob_high_given_high_sb, "\n")

# ==============================================================================
# PART 3: RELATIONSHIP ANALYSIS
# ==============================================================================

cor_sb <- cor(df$seatbelt, df$totfatrte, use = "complete.obs")
cor_unem <- cor(df$unem, df$totfatrte, use = "complete.obs")

cat("--- Correlation ---\n")
cat("Seatbelt vs Fatality:", cor_sb, "\n")
cat("Unemployment vs Fatality:", cor_unem, "\n")

plot(df$seatbelt, df$totfatrte,
     main = "Seatbelt vs Fatality Rate",
     xlab = "Seatbelt Usage",
     ylab = "Fatality Rate",
     pch = 19,
     col = rgb(0.2, 0.5, 0.8, 0.3))

abline(lm(totfatrte ~ seatbelt, data = df), col = "red", lwd = 2)

# ==============================================================================
# PART 4: ECONOMIC ACTIVITY ANALYSIS
# ==============================================================================

breaks <- unique(quantile(df$vehicmilespc,
                          probs = c(0, 0.33, 0.66, 1),
                          na.rm = TRUE))

df$activity_level <- cut(df$vehicmilespc,
                         breaks = breaks,
                         labels = c("Low", "Medium", "High"),
                         include.lowest = TRUE)

econ_table <- table(df$activity_level, df$seatbelt_group)
income_mean <- aggregate(totfatrte ~ activity_level, data = df, mean)

cat("--- Economic Analysis ---\n")
print(econ_table)
print(income_mean)

boxplot(totfatrte ~ activity_level,
        data = df,
        col = c("tomato", "orange", "lightblue"),
        main = "Fatality Rate by Activity Level",
        xlab = "Activity Level",
        ylab = "Fatality Rate")

# ==============================================================================
# PART 5: INFERENCE & REGRESSION
# ==============================================================================

t_test <- t.test(totfatrte ~ seatbelt_group, data = df)

model_simple <- lm(totfatrte ~ seatbelt, data = df)
model_multi <- lm(totfatrte ~ seatbelt + unem, data = df)

cat("--- T-Test ---\n")
print(t_test)

cat("--- Simple Regression ---\n")
print(summary(model_simple)$coefficients)

cat("--- Multiple Regression ---\n")
print(summary(model_multi)$coefficients)

par(mfrow = c(1, 2))
plot(model_multi, which = 1:2)
par(mfrow = c(1, 1))