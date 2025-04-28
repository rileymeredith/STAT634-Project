library(haven)

# ==== 1.Read datasets ====

# --- 1.1 Identifying our sample population: adults with Diabetes, Cancer, or CHD ----
demographic_data <- read_xpt("data/DEMO_L.xpt")
med_diagnosis_data <- read_xpt("data/MCQ_L.xpt")

# ---- 1.2 Exposure: Medication, Healthy Lifestyle Decsions, or Both ----
RXQ_RX_data <- read_xpt("data/RXQ_RX_L.xpt")
weight_data <- read_xpt("data/WHQ_L.xpt")
diet_data <- read_xpt("data/DBQ_L.xpt")
physical_fit_data <- read_xpt("data/PAQ_L.xpt")
alcohol_data <- read_xpt("data/ALQ_L.xpt")
smoking_data <- read_xpt("data/SMQ_L.xpt")
smoking1_data <- read_xpt("data/SMQRTU_L.xpt")


# ---- 1.3 Potential Confounding: height, gender ----
body_measurement_data <- read_xpt("data/BMX_L.xpt")


# ---- 1.4 Outcome Variables ----
BP_data <- read_xpt("data/BPXO_L.xpt")
fast_gluc_data <- read_xpt("data/GLU_L.xpt")
HDL_data <- read_xpt("data/HDL_L.xpt")
tot_chol_data <- read_xpt("data/TCHOL_L.xpt")




