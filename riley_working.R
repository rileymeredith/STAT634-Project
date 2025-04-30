library(haven)
library(dplyr)
library(tidyr)

# ==== 1. Read datasets ====

# --- 1.1 Identifying our sample population: adults (40-65) with Diabetes, Cancer, or CHD ----
demographic_data <- read_xpt("data/DEMO_L.xpt")
med_diagnosis_data <- read_xpt("data/MCQ_L.xpt")
diabetes_data <- read_xpt("data/DIQ_L.xpt")

# ---- 1.2 Exposure: Medication, Healthy Lifestyle Decisions, or Both ----
medication_data <- read_xpt("data/RXQ_RX_L.xpt")
diet_data <- read_xpt("data/DR1TOT_L.xpt")
physical_fit_data <- read_xpt("data/PAQ_L.xpt")

# ---- 1.3 Potential Confounding: height, gender, age (demo) ----
body_measurement_data <- read_xpt("data/BMX_L.xpt")

# ---- 1.4 Outcome Variables ----
blood_press_data <- read_xpt("data/BPXO_L.xpt")
fast_gluc_data <- read_xpt("data/GLU_L.xpt")
HDL_data <- read_xpt("data/HDL_L.xpt")
tot_chol_data <- read_xpt("data/TCHOL_L.xpt")
HSCRP_data <- read_xpt("data/HSCRP_L.xpt")
insulin_data <- read_xpt("data/INS_L.xpt")
health_status_data <- read_xpt("data/HUQ_L.xpt")

# ==== 2. Prepare dataset ====

# ---- 2.1 Isolate sample population ----
# identify population who are age 40-65; kept age, gender columns
demographic_data <- demographic_data %>%
  select(SEQN,RIAGENDR,RIDAGEYR) %>%
  filter(RIDAGEYR >= 40 & RIDAGEYR <= 65)

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
      TRUE ~ NA_character_  # keeps NA for anything else
    ),
    Cancer = case_when(
      MCQ220 == 1 ~ "Yes",
      MCQ220 == 2 ~ "No",
      TRUE ~ NA_character_  # keeps NA for anything else
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
    RXQ033 == 2 ~ "No"))

diet_data <- diet_data %>%
  select(SEQN,DRQSDIET) %>%
  mutate(Diet = case_when(
    DRQSDIET == 1 ~ "Yes",
    DRQSDIET == 2 ~ "No"))

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
  select(SEQN,Gender,Age,Medicine,Diet,Meets_req_fit_150) %>%
  mutate(
    Medicine = replace_na(Medicine, "0"),
    Meets_req_fit_150 = replace_na(Meets_req_fit_150, "0"),
    Diet = replace_na(Diet, "0"),
    Exposure = case_when(
      Medicine %in% c("Yes") & Meets_req_fit_150 %in% c("No", "0") & Diet %in% c("No", "0") ~ "Meds Only",
      Medicine %in% c("Yes") & (Meets_req_fit_150 == "Yes" | Diet == "Yes") ~ "Both",
      Medicine %in% c("No", "0") & (Meets_req_fit_150 == "Yes" | Diet == "Yes") ~ "Lifestyle only",
      Medicine %in% c("No", "0") & Meets_req_fit_150 %in% c("No", "0") & Diet %in% c("No", "0") ~ "None"))

sample_data %>%
  count(Exposure)









