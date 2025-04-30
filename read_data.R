library(haven)
library(dplyr)
library(tidyr)
library(ggplot2)

# ==== 1. Read datasets ====

# --- 1.1 Identifying our sample population: adults (40-65) with Diabetes, Cancer, or CHD ----
demographic_data <- read_xpt("data/DEMO_L.xpt")
med_diagnosis_data <- read_xpt("data/MCQ_L.xpt")
diabetes_data <- read_xpt("data/DIQ_L.xpt")

# ---- 1.2 Exposure: Medication, Healthy Lifestyle Decisions, or Both ----
medication_data <- read_xpt("data/RXQ_RX_L.xpt")
diet_data <- read_xpt("data/DR1TOT_L.xpt")
physical_fit_data <- read_xpt("data/PAQ_L.xpt")
weight_data <- read_xpt("data/WHQ_L.xpt")

# ---- 1.3 Potential Confounding: height, gender, age (demo) ----
body_measurement_data <- read_xpt("data/BMX_L.xpt")

# ---- 1.4 Outcome Variables ----
blood_press_data <- read_xpt("data/BPXO_L.xpt")
fast_gluc_data <- read_xpt("data/GLU_L.xpt")
HDL_data <- read_xpt("data/HDL_L.xpt")
tot_chol_data <- read_xpt("data/TCHOL_L.xpt")
HSCRP_data <- read_xpt("data/HSCRP_L.xpt")
health_status_data <- read_xpt("data/HUQ_L.xpt")
GHB_data <- read_xpt("data/GHB_L.xpt")

# ==== 2. Prepare dataset ====

# ---- 2.1 Isolate sample population ----
# identify population who are age 40-65; kept age, gender columns
demographic_data <- demographic_data %>%
  select(SEQN,RIAGENDR,RIDAGEYR) %>%
  filter(RIDAGEYR >= 19 & RIDAGEYR <= 65)

# combine chronic disease data
med_diagnosis_data <- med_diagnosis_data %>%
  select(SEQN,MCQ160C,MCQ220)
diabetes_data <- diabetes_data %>%
  select(SEQN,DIQ010)
med_diagnosis_data <- med_diagnosis_data %>%
  full_join(diabetes_data,by="SEQN")

# isolate population who have been diagnosed by at least 1 of 3 major chronic diseases
sample_data <- left_join(demographic_data,med_diagnosis_data,by="SEQN") %>%
  filter(MCQ160C == 1 | MCQ220 == 1 | DIQ010 == 1) %>%
  mutate(
    CHD = case_when(
      MCQ160C == 1 ~ "Yes",
      MCQ160C == 2 ~ "No",
      TRUE ~ NA_character_
    ),
    Cancer = case_when(
      MCQ220 == 1 ~ "Yes",
      MCQ220 == 2 ~ "No",
      TRUE ~ NA_character_
    ),
    Diabetes = case_when(
      DIQ010 == 1 ~ "Yes",
      DIQ010 == 2 ~ "No",
      TRUE ~ NA_character_),
    Gender = case_when(
      RIAGENDR == 1 ~ "M",
      RIAGENDR == 2 ~ "F"),
    Age = RIDAGEYR) %>%
  select(-MCQ160C,-MCQ220,-DIQ010,-RIAGENDR,-RIDAGEYR)

# ---- 2.2 Add column for exposure ----

medication_data <- medication_data %>%
  select(SEQN,RXQ033) %>%
  mutate(Medicine = case_when(
    RXQ033 == 1 ~ "Yes",
    RXQ033 == 2 ~ "No")) %>%
  filter(RXQ033 != 7 & RXQ033 != 9)

diet_data <- diet_data %>%
  select(SEQN,DRQSDIET) %>%
  mutate(Diet = case_when(
    DRQSDIET == 1 ~ "Yes",
    DRQSDIET == 2 ~ "No")) %>%
  filter(DRQSDIET != 9)

weight_data <- weight_data %>%
  select(SEQN,WHQ070) %>%
  mutate(Lose_weight = case_when(
    WHQ070 == 1 ~ "Yes",
    WHQ070 == 2 ~ "No")) %>%
  filter(WHQ070 != 7 & WHQ070 != 9)

physical_fit_data <- physical_fit_data %>%
  select(SEQN,PAD790Q,PAD790U,PAD800) %>%
  mutate(
    PAD790U = if_else(PAD790U == "D" & PAD790Q > 4, "W", PAD790U),
    PAD790U = if_else(PAD790U == "W" & PAD790Q > 28, "M", PAD790U),
    PAD790U = if_else(PAD790U == "M" & PAD790Q > 31, "W", PAD790U)) %>%
  filter(PAD790Q!=7777,PAD790Q!=9999,PAD800!=9999)
    
physical_fit_data <- physical_fit_data %>%
  mutate(
    PAD790Q = if_else(PAD790U == "D", PAD790Q * 7, PAD790Q),
    PAD790Q = if_else(PAD790U == "M", PAD790Q / 4, PAD790Q),
    PAD790Q = if_else(PAD790U == "Y", PAD790Q / 52, PAD790Q),
    PAD800 = if_else(is.na(PAD800), 0, PAD800),
    Week_fit_total_min = PAD790Q * PAD800,
    Meets_req_fit_150 = case_when(
      Week_fit_total_min < 150 ~ "No",
      Week_fit_total_min >= 150 ~ "Yes"))

sample_data <- sample_data %>%
  left_join(medication_data,by="SEQN") %>%
  left_join(diet_data,by="SEQN") %>%
  left_join(physical_fit_data,by="SEQN") %>%
  left_join(weight_data,by="SEQN") %>%
  select(SEQN,Gender,Age,Medicine,Diet,Meets_req_fit_150,Lose_weight) %>%
  mutate(
    Medicine = replace_na(Medicine, "0"),
    Meets_req_fit_150 = replace_na(Meets_req_fit_150, "0"),
    Diet = replace_na(Diet, "0"),
    Lose_weight = replace_na(Lose_weight, "No"),
    Exposure = case_when(
      Medicine %in% c("Yes") &
        Meets_req_fit_150 %in% c("No", "0") &
        Diet %in% c("No", "0") &
        Lose_weight %in% c("No", "0") ~ "Meds Only",
      
      Medicine %in% c("Yes") &
        (Meets_req_fit_150 == "Yes" | Diet == "Yes" | Lose_weight == "Yes") ~ "Both",
      
      Medicine %in% c("No", "0") &
        (Meets_req_fit_150 == "Yes" | Diet == "Yes" | Lose_weight == "Yes") ~ "Lifestyle only",
      
      Medicine %in% c("No", "0") &
        Meets_req_fit_150 %in% c("No", "0") &
        Diet %in% c("No", "0") &
        Lose_weight %in% c("No", "0") ~ "None"))

sample_data %>%
  count(Exposure)

# ---- 2.3 Isolate outcome variables, mutate to binary ----
blood_press_data <- blood_press_data %>%
  select(SEQN,BPXOSY2,BPXODI2,BPXOSY3,BPXODI3) %>%
  mutate(BP_systolic = case_when(
    !is.na(BPXOSY2) & !is.na(BPXOSY3) ~ (BPXOSY2 + BPXOSY3) / 2,
    !is.na(BPXOSY2) & is.na(BPXOSY3) ~ BPXOSY2,
    is.na(BPXOSY2) & !is.na(BPXOSY3) ~ BPXOSY3,
    TRUE ~ NA_real_
  ),
  BP_diastolic = case_when(
    !is.na(BPXODI2) & !is.na(BPXODI3) ~ (BPXODI2 + BPXODI3) / 2,
    !is.na(BPXODI2) & is.na(BPXODI3) ~ BPXODI2,
    is.na(BPXODI2) & !is.na(BPXODI3) ~ BPXODI3,
    TRUE ~ NA_real_
  ),
  Healthy_BP = case_when(
    is.na(BP_systolic) | is.na(BP_diastolic) ~ NA_character_,
    BP_systolic >= 130 ~ "No",
    BP_diastolic >= 80 ~ "No",
    TRUE ~ "Yes")) %>%
  select(SEQN,BP_systolic,BP_diastolic,Healthy_BP)
         
fast_gluc_data <- fast_gluc_data %>%
  select(SEQN,LBXGLU) %>%
  drop_na(LBXGLU) %>%
  mutate(Healthy_fasting_glucose = case_when(
    LBXGLU >= 100 ~ "No",
    TRUE ~ "Yes"))

GHB_data <- GHB_data %>%
  select(SEQN,LBXGH) %>%
  drop_na(LBXGH) %>%
  mutate(Healthy_HA1C = case_when(
    LBXGH >= 5.7 ~ "No",
    TRUE ~ "Yes"))

# varries for gender
HDL_data <- HDL_data %>%
  select(SEQN,LBDHDD) %>%
  drop_na(LBDHDD)

tot_chol_data <- tot_chol_data %>%
  select(SEQN,LBXTC) %>%
  drop_na(LBXTC) %>%
  mutate(Healthy_tot_chol = case_when(
    LBXTC >= 200 ~ "No",
    TRUE ~ "Yes"))

HSCRP_data <- HSCRP_data %>%
  select(SEQN,LBXHSCRP) %>%
  drop_na(LBXHSCRP) %>%
  mutate(Healthy_HSCRP = case_when(
    LBXHSCRP >= 5 ~ "No",
    TRUE ~ "Yes"))
  
health_status_data <- health_status_data %>%
  select(SEQN,HUQ010) %>%
  drop_na(HUQ010) %>%
  filter(HUQ010 != 7 & HUQ010 != 9) %>%
  mutate(Health_status = HUQ010)

# ---- 2.4 Combine and mutate for variables that differ by gender ----
exposure_outcome_data <- sample_data %>%
  left_join(blood_press_data,by="SEQN") %>%
  left_join(fast_gluc_data,by="SEQN") %>%
  left_join(GHB_data,by="SEQN") %>%
  left_join(HDL_data,by="SEQN") %>%
  left_join(tot_chol_data,by="SEQN") %>%
  left_join(HSCRP_data,by="SEQN") %>%
  left_join(CBC_data,by="SEQN") %>%
  left_join(health_status_data,by="SEQN") %>%
  mutate(
    Healthy_HDL = case_when(
      is.na(LBDHDD) ~ NA_character_,
      Gender == "M" & LBDHDD < 40 ~ "No",
      Gender == "F" & LBDHDD < 50 ~ "No",
      LBDHDD >= 60 ~ "No",
      TRUE ~ "Yes")) %>%
  select(SEQN,Age,Gender,Exposure,Health_status,Healthy_BP,Healthy_HSCRP,Healthy_tot_chol,Healthy_HDL,Healthy_fasting_glucose,Healthy_HA1C)

# List of outcome variables to convert
outcome_vars <- c("Healthy_BP", "Healthy_HSCRP", "Healthy_tot_chol", 
                  "Healthy_HDL", "Healthy_fasting_glucose", "Healthy_HA1C")

# Ensure exposure_outcome_data is a data frame (if not, convert it)
exposure_outcome_data <- as.data.frame(exposure_outcome_data)

# Loop over each outcome variable and convert "Yes" to 1 and "No" to 0, leaving NA as is
for (var in outcome_vars) {
  if(var %in% names(exposure_outcome_data)) {
    exposure_outcome_data[[var]] <- ifelse(exposure_outcome_data[[var]] == "Yes", 1, 
                                           ifelse(exposure_outcome_data[[var]] == "No", 0, exposure_outcome_data[[var]]))
  } else {
    warning(paste("Column", var, "does not exist in the dataset"))
  }
}

# ==== 3. Contingency tables and Analysis ====
# ---- 3.1 Pairwise Fisher Exact Test and Odds ratios for lab results ----
# ---- 3.11 Fisher Analysis ----
fisher_results <- data.frame(
  Outcome = numeric(),
  Exposure_Comparison = character(),
  P_Value = numeric(),
  Odds_Ratio = numeric(),
  CI_Lower = numeric(),
  CI_Upper = numeric(),
  stringsAsFactors = FALSE
)

# ---- 3.12 Odds ratio plots ----
outcome_vars <- names(exposure_outcome_data)[grepl("^Healthy_", names(exposure_outcome_data))]
for (var in outcome_vars) {
  cat("\n======================\n")
  cat("Outcome:", var, "\n")
  cat("======================\n")
  
  tab <- table(exposure_outcome_data$Exposure, exposure_outcome_data[[var]])
  print(tab)
  
  exposure_levels <- unique(exposure_outcome_data$Exposure)
  
  for (i in 1:(length(exposure_levels) - 1)) {
    for (j in (i + 1):length(exposure_levels)) {
      pairwise_tab <- tab[c(i, j), ]
      comparison_label <- paste(exposure_levels[i], "vs", exposure_levels[j])
      cat("\nPairwise comparison:", comparison_label, "\n")
      
      fisher_result <- fisher.test(pairwise_tab)
      
      # Extract and store results
      fisher_results <- rbind(fisher_results, data.frame(
        Outcome = var,
        Exposure_Comparison = comparison_label,
        P_Value = fisher_result$p.value,
        Odds_Ratio = fisher_result$estimate,
        CI_Lower = fisher_result$conf.int[1],
        CI_Upper = fisher_result$conf.int[2],
        stringsAsFactors = FALSE
      ))
    }
  }
}

print(fisher_results)
# ----  3.13 Plot OR ----
outcomes <- unique(fisher_results$Outcome)

for (outcome in outcomes) {
  outcome_data <- fisher_results[fisher_results$Outcome == outcome, ]
  
  p <- ggplot(outcome_data, aes(x = Odds_Ratio, y = Exposure_Comparison)) +
    geom_point(color = "blue", size = 3) +
    geom_errorbarh(aes(xmin = CI_Lower, xmax = CI_Upper), height = 0.2, color = "black") +
    geom_vline(xintercept = 1, linetype = "solid", color = "black", alpha = 0.5) +
    geom_text(aes(label = paste0("p=", signif(P_Value, 2))), 
              hjust = -0.7, vjust = -0.7, size = 3, color = "darkred") +
    scale_x_log10() +
    labs(title = paste("Odds Ratios for Outcome:", outcome),
         x = "Odds Ratio",
         y = "Exposure Comparison") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(p)
  
  filename <- paste0("odds_ratio_", gsub("[^A-Za-z0-9]", "_", outcome), ".png")
  ggsave(filename, plot = p, width = 8, height = 6, dpi = 300)
}
# ---- 3.14 Log regression ----
logistic_results <- data.frame(
  Outcome = character(),
  Predictor = character(),
  OR = numeric(),
  CI_Lower = numeric(),
  CI_Upper = numeric(),
  P_Value = numeric(),
  stringsAsFactors = FALSE
)


outcome_vars <- names(exposure_outcome_data)[grepl("^Healthy_", names(exposure_outcome_data))]

for (var in outcome_vars) {
  cat("Fitting logistic model for:", var, "\n")
  
  binary_outcome <- exposure_outcome_data[[var]]

  temp_data <- data.frame(
    Outcome = as.numeric(binary_outcome),
    Exposure = relevel(factor(exposure_outcome_data$Exposure), ref = "Meds Only"),
    Age = exposure_outcome_data$Age,
    Gender = relevel(factor(exposure_outcome_data$Gender), ref = "M")
  )
  str(temp_data)
  table(temp_data$Outcome)
  
  # Drop rows with missing outcome
  temp_data <- temp_data[!is.na(temp_data$Outcome), ]
  
  # Fit logistic regression model
  model <- glm(Outcome ~ Exposure + Age + Gender, data = temp_data, family = binomial)
  # Extract odds ratios and confidence intervals
  coef_summary <- summary(model)$coefficients
  conf_int <- suppressMessages(confint(model))
  
  for (predictor in rownames(coef_summary)) {
    OR <- exp(coef(model)[predictor])
    CI <- exp(conf_int[predictor, ])
    P <- coef_summary[predictor, "Pr(>|z|)"]
    
    logistic_results <- rbind(logistic_results, data.frame(
      Outcome = var,
      Predictor = predictor,
      OR = OR,
      CI_Lower = CI[1],
      CI_Upper = CI[2],
      P_Value = P,
      stringsAsFactors = FALSE
    ))
  }
}

print(logistic_results)
# ---- 3.2 Wilcoxin rank sum test for Subjective Health Status ----
# ---- 3.21 Analysis ----
# Create an empty dataframe to store the results
wilcox_results <- data.frame(
  Exposure_Comparison = character(),
  W_Statistic = numeric(),
  P_Value = numeric(),
  stringsAsFactors = FALSE
)

# Get the levels of the Exposure factor
exposure_levels <- levels(exposure_outcome_data$Exposure)

# Loop through each pair of exposure levels
for (i in 1:(length(exposure_levels) - 1)) {
  for (j in (i + 1):length(exposure_levels)) {
    
    # Subset data for the current pair of exposure levels
    pair_data <- exposure_outcome_data[exposure_outcome_data$Exposure %in% c(exposure_levels[i], exposure_levels[j]), ]
    pair_data$Exposure <- droplevels(pair_data$Exposure)  # Drop unused levels
    
    # Check if there are exactly two levels in the Exposure variable for this pair
    if (length(unique(pair_data$Exposure)) == 2) {
      cat("\n======================\n")
      cat("Comparing:", exposure_levels[i], "vs", exposure_levels[j], "\n")
      cat("======================\n")
      
      # Run the Wilcoxon test
      test <- wilcox.test(as.numeric(as.character(pair_data$Health_status)) ~ pair_data$Exposure)
      
      # Extract results from the Wilcoxon test
      wilcox_results <- rbind(wilcox_results, data.frame(
        Exposure_Comparison = paste(exposure_levels[i], "vs", exposure_levels[j]),
        W_Statistic = test$statistic,
        P_Value = test$p.value
      ))
      
      # Print the test result
      print(test)
    } else {
      cat("\nSkipping:", exposure_levels[i], "vs", exposure_levels[j], "- Not enough data.\n")
    }
  }
}

# Print the dataframe with results
print(wilcox_results)


# ---- 3.22 Boxplot ----
ggplot(exposure_outcome_data, aes(x = Exposure, y = as.numeric(Health_status), fill = Exposure)) +
  geom_boxplot(outlier.shape = NA, color = "black", width = 0.6) +
  geom_jitter(width = 0.2, alpha = 0.4, color = "gray40", size = 1.2) +
  scale_fill_brewer(palette = "Set2") +  # Try "Pastel1", "Dark2", etc.
  labs(title = "Health Status by Exposure Type",
       x = "Exposure Group",
       y = "Health Status (1 = Excellent, 5 = Poor)") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1))