# Model:
# GWIndex_it = a + b1*SIZE_it + b2*ROA_it + b3*FOREIGN_it + b4*VIOLATION_it
#              + b5*LEVERAGE_it + b6*SUBSECTOR_it + e_it

library(plm)        # regresi data panel, uji Chow (pFtest), uji Hausman (phtest)
library(lmtest)      # bptest (heteroskedastisitas), coeftest
library(car)         # vif (multikolinearitas)
library(tseries)     # jarque.bera.test (normalitas)
library(sandwich)    # standard error robust (vcovHC)
library(readxl)      # jika data disimpan dalam .xlsx
library(dplyr)

## -----------------------------------------------------------------------------
## 1. IMPORT DATA
## -----------------------------------------------------------------------------
# Format data HARUS long panel: satu baris = satu perusahaan-tahun.
# Kolom minimal: FIRM (kode/nama perusahaan), YEAR, GWINDEX, SIZE, ROA,
#                FOREIGN, VIOLATION, LEVERAGE, SUBSECTOR
#
# Sesuaikan path & nama file dengan datamu:
# df <- read_excel("data_greenwashing.xlsx", sheet = "Sheet1")
# atau:
# df <- read.csv("data_greenwashing.csv")

# ---- Contoh struktur data (hapus/ganti dengan data asli) --------------------
# df <- data.frame(
#   FIRM      = rep(paste0("F", 1:15), each = 6),
#   YEAR      = rep(2020:2025, times = 15),
#   GWINDEX   = rnorm(90, 0, 1),
#   SIZE      = rnorm(90, 28, 2),
#   ROA       = rnorm(90, 0.05, 0.03),
#   FOREIGN   = runif(90, 0, 80),
#   VIOLATION = sample(c(0, 1), 90, replace = TRUE),
#   LEVERAGE  = rnorm(90, 0.6, 0.2),
#   SUBSECTOR = sample(c(0, 1), 90, replace = TRUE)
# )

# Pastikan tipe data benar
# df$FIRM      <- as.factor(df$FIRM)
# df$VIOLATION <- as.factor(df$VIOLATION)
# df$SUBSECTOR <- as.factor(df$SUBSECTOR)

## Ubah menjadi objek pdata.frame (format panel plm)
# pdata <- pdata.frame(df, index = c("FIRM", "YEAR"))

## Cek keseimbangan panel (balanced/unbalanced)
# pdim(pdata)

## -----------------------------------------------------------------------------
## 2. STATISTIK DESKRIPTIF
## -----------------------------------------------------------------------------
# summary(df[, c("GWINDEX","SIZE","ROA","FOREIGN","LEVERAGE")])

## -----------------------------------------------------------------------------
## 3. UJI ASUMSI KLASIK (sebelum pemilihan model panel)
## -----------------------------------------------------------------------------
# Uji asumsi klasik pada regresi panel umumnya dilakukan atas model pooled OLS
# sebagai basis awal, sebelum lanjut ke uji Chow/Hausman.

model_pooled <- plm(
  GWINDEX ~ SIZE + ROA + FOREIGN + VIOLATION + LEVERAGE + SUBSECTOR,
  data  = pdata,
  model = "pooling"
)
summary(model_pooled)

## 3a. Uji Normalitas (Jarque-Bera pada residual)
jarque.bera.test(residuals(model_pooled))
# H0: residual berdistribusi normal. p > 0.05 -> asumsi normalitas terpenuhi.

## 3b. Uji Multikolinearitas (VIF)
# vif() butuh model lm biasa (bukan objek plm), jadi hitung dari OLS pooled
model_ols_check <- lm(
  GWINDEX ~ SIZE + ROA + FOREIGN + VIOLATION + LEVERAGE + SUBSECTOR,
  data = df
)
vif(model_ols_check)
# Aturan umum: VIF < 10 (atau < 5 lebih ketat) -> tidak ada multikolinearitas serius.

## 3c. Uji Heteroskedastisitas (Breusch-Pagan)
bptest(
  GWINDEX ~ SIZE + ROA + FOREIGN + VIOLATION + LEVERAGE + SUBSECTOR,
  data = df,
  studentize = TRUE
)
# H0: homoskedastisitas. p > 0.05 -> tidak ada masalah heteroskedastisitas.
# Jika p < 0.05, gunakan robust standard error (vcovHC) pada tahap estimasi akhir.

## 3d. Uji Autokorelasi (khusus data panel)
pbgtest(model_pooled)  # Breusch-Godfrey untuk panel
# H0: tidak ada autokorelasi. p > 0.05 -> tidak ada masalah autokorelasi.

## -----------------------------------------------------------------------------
## 4. PEMILIHAN MODEL PANEL: Common Effect vs Fixed Effect vs Random Effect
## -----------------------------------------------------------------------------

## Estimasi tiga model dasar
model_common <- plm(
  GWINDEX ~ SIZE + ROA + FOREIGN + VIOLATION + LEVERAGE + SUBSECTOR,
  data  = pdata,
  model = "pooling"
)

model_fixed <- plm(
  GWINDEX ~ SIZE + ROA + FOREIGN + VIOLATION + LEVERAGE + SUBSECTOR,
  data  = pdata,
  model = "within"      # Fixed Effect Model
)

model_random <- plm(
  GWINDEX ~ SIZE + ROA + FOREIGN + VIOLATION + LEVERAGE + SUBSECTOR,
  data  = pdata,
  model = "random"       # Random Effect Model
)

## 4a. Uji Chow (Common Effect vs Fixed Effect)
pFtest(model_fixed, model_common)
# H0: Common Effect Model lebih sesuai.
# p < 0.05 -> tolak H0 -> pilih Fixed Effect Model, lanjut ke uji Hausman.
# p > 0.05 -> gunakan Common Effect Model (berhenti di sini).

## 4b. Uji Hausman (Fixed Effect vs Random Effect)
phtest(model_fixed, model_random)
# H0: Random Effect Model lebih sesuai (efek acak tidak berkorelasi dgn regressor).
# p < 0.05 -> tolak H0 -> pilih Fixed Effect Model.
# p > 0.05 -> pilih Random Effect Model.

## -----------------------------------------------------------------------------
## 5. MODEL FINAL & PENGUJIAN HIPOTESIS
## -----------------------------------------------------------------------------
# Ganti `model_final` sesuai hasil uji Chow & Hausman di atas.
# Contoh jika hasilnya Fixed Effect:

model_final <- model_fixed

## Jika uji heteroskedastisitas (3c) menunjukkan masalah, gunakan robust SE:
coeftest(model_final, vcov. = vcovHC(model_final, type = "HC1"))

## Ringkasan model (uji t per variabel, uji F simultan, R-squared)
summary(model_final)

## R-squared & Adjusted R-squared (khusus untuk within/FE model)
r.squared(model_final, dfcor = TRUE)

## -----------------------------------------------------------------------------
## 6. INTERPRETASI (kerangka, isi manual sesuai output)
## -----------------------------------------------------------------------------
# - Uji t per variabel (SIZE, ROA, FOREIGN, VIOLATION) -> H1-H4 didukung/ditolak
#   berdasarkan p-value < alpha (umumnya 0.05) dan arah koefisien.
# - Uji F (F-statistic pada summary(model_final)) -> menguji signifikansi
#   simultan seluruh variabel independen terhadap GWIndex.
# - R-squared -> proporsi variasi GWIndex yang dijelaskan oleh model.