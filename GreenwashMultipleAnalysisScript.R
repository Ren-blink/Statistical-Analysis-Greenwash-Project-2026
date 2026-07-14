# ==============================================================================
# ANALISIS REGRESI DATA PANEL: GREENWASHING SCORE
# ==============================================================================
# Model:
#   GW_Score_it = a + b1*SIZE_it + b2*ROA_it + b3*FOREIGN_it + b4*VIOLATION_it
#               + b5*DER_it + b6*SUBSECTOR_it + e_it
#
# Variabel:
#   Y  : GW_Score     -- Greenwashing Score (|skor narasi - skor kinerja aktual|)
#   X1 : SIZE         -- Ln(Total Aset)
#   X2 : ROA          -- Return on Assets (Laba Bersih / Total Aset)
#   X3 : FOREIGN      -- Kepemilikan asing (% dari total saham beredar)
#   X4 : VIOLATION    -- Skor ordinal PROPER (1=Hitam s.d. 5=Emas)
#   CV : DER          -- Debt-to-Equity Ratio (kontrol)
#   CV : SUBSECTOR    -- Dummy sub-sektor (1=Batu Bara, 0=Mineral Logam/Migas)
# ==============================================================================

library(plm)        # regresi data panel, uji Chow (pFtest), uji Hausman (phtest)
library(lmtest)     # bptest (heteroskedastisitas), coeftest
library(car)        # vif (multikolinearitas)
library(tseries)    # jarque.bera.test (normalitas)
library(sandwich)   # vcovHC (robust standard error)
library(readr)
library(dplyr)
library(ggplot2)
library(stringr)
library(DiagrammeR) # grViz untuk diagram hipotesis

## -----------------------------------------------------------------------------
## 0. DIAGRAM KERANGKA HIPOTESIS
## -----------------------------------------------------------------------------
grViz("
digraph hipotesis {

  graph [rankdir = LR, fontname = Helvetica, fontsize = 11, nodesep = 0.6, ranksep = 1.2]

  # --- Variabel Independen ---
  node [shape = rectangle, style = filled, fillcolor = '#D6EAF8',
        color = '#2E86C1', fontcolor = '#1B4F72', fontname = Helvetica, width = 2.4]

  X1 [label = 'SIZE (X1)\nLn(Total Aset)']
  X2 [label = 'ROA (X2)\nLaba / Total Aset']
  X3 [label = 'FOREIGN (X3)\n% Kepemilikan Asing']
  X4 [label = 'VIOLATION (X4)\nSkor Ordinal PROPER']

  # --- Variabel Kontrol ---
  node [shape = rectangle, style = 'filled,dashed', fillcolor = '#F9F9F9',
        color = '#999999', fontcolor = '#666666', fontname = Helvetica, width = 2.2]

  CV1 [label = 'DER\n(Kontrol)']
  CV2 [label = 'SUBSECTOR\n(Kontrol)']

  # --- Variabel Dependen ---
  node [shape = rectangle, style = filled, fillcolor = '#D5F5E3',
        color = '#1E8449', fontcolor = '#1E8449', fontname = 'Helvetica-Bold', width = 2.6]

  Y [label = 'GW_Score (Y)\nGreenwashing Score']

  # --- Panah Hipotesis ---
  X1 -> Y [label = 'H1', fontname = 'Helvetica-Bold', color = '#2E86C1', fontcolor = '#2E86C1']
  X2 -> Y [label = 'H2', fontname = 'Helvetica-Bold', color = '#2E86C1', fontcolor = '#2E86C1']
  X3 -> Y [label = 'H3', fontname = 'Helvetica-Bold', color = '#2E86C1', fontcolor = '#2E86C1']
  X4 -> Y [label = 'H4', fontname = 'Helvetica-Bold', color = '#2E86C1', fontcolor = '#2E86C1']

  # --- Panah Kontrol (putus-putus) ---
  CV1 -> Y [style = dashed, color = '#999999', fontcolor = '#999999']
  CV2 -> Y [style = dashed, color = '#999999', fontcolor = '#999999']
}
")

## -----------------------------------------------------------------------------
## 1. IMPORT DATA
## -----------------------------------------------------------------------------
# File ini merupakan gabungan antara:
#   - GW_Score dari hasil text mining (hasil_greenwashing_score.csv)
#   - Variabel keuangan & kepemilikan dari laporan tahunan + KLHK

df <- read_csv("data_panel_lengkap.csv", show_col_types = FALSE) |>
  mutate(
    FIRM      = as.factor(FIRM),
    YEAR      = as.integer(YEAR),
    SUBSECTOR = as.factor(SUBSECTOR),
    VIOLATION = as.integer(VIOLATION)
  )

# Cek struktur
glimpse(df)

# Cek keberadaan NA
cat("\nJumlah NA per kolom:\n")
colSums(is.na(df))

## Buat objek pdata.frame (format panel untuk plm)
pdata <- pdata.frame(df, index = c("FIRM", "YEAR"))

## Cek keseimbangan panel
pdim(pdata)
# Balanced: 10 perusahaan x 5 tahun = 50 observasi

## -----------------------------------------------------------------------------
## 2. STATISTIK DESKRIPTIF
## -----------------------------------------------------------------------------
summary(df[, c("GW_Score", "SIZE", "ROA", "FOREIGN", "VIOLATION", "DER")])

## -----------------------------------------------------------------------------
## 2b. VISUALISASI DATA EKSPLORATORI
## -----------------------------------------------------------------------------

# -- Plot 1: Distribusi GW_Score --
ggplot(df, aes(x = GW_Score)) +
  geom_histogram(aes(y = after_stat(density)), bins = 12, fill = "#2E86C1", alpha = 0.7) +
  geom_density(color = "#1B4F72", linewidth = 0.8) +
  labs(title = "Distribusi GW_Score",
       x = "GW_Score (Greenwashing)", y = "Densitas")

# -- Plot 2: Tren GW_Score per Perusahaan (2020–2024) --
ggplot(df, aes(x = YEAR, y = GW_Score, group = FIRM, color = FIRM)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = 2020:2024) +
  labs(title = "Tren GW_Score per Perusahaan (2020–2024)",
       x = "Tahun", y = "GW_Score", color = "Perusahaan")

# -- Plot 3: SIZE vs GW_Score --
ggplot(df, aes(x = SIZE, y = GW_Score)) +
  geom_point(aes(color = FIRM), size = 2.5) +
  geom_smooth(method = "lm", se = TRUE, color = "black", linewidth = 0.8) +
  labs(title = "Ukuran Perusahaan (SIZE) vs GW_Score",
       x = "SIZE – Ln(Total Aset)", y = "GW_Score", color = "Perusahaan")

# -- Plot 4: FOREIGN vs GW_Score --
ggplot(df, aes(x = FOREIGN, y = GW_Score)) +
  geom_point(aes(color = FIRM), size = 2.5) +
  geom_smooth(method = "lm", se = TRUE, color = "black", linewidth = 0.8) +
  labs(title = "Kepemilikan Asing (FOREIGN) vs GW_Score",
       x = "FOREIGN (%)", y = "GW_Score", color = "Perusahaan")

# -- Plot 5: GW_Score per Peringkat PROPER (VIOLATION) --
ggplot(df, aes(x = factor(VIOLATION), y = GW_Score)) +
  geom_boxplot(fill = "#D6EAF8", color = "#2E86C1") +
  geom_jitter(width = 0.15, alpha = 0.6, color = "#1B4F72") +
  scale_x_discrete(labels = c("2" = "Merah (2)", "3" = "Biru (3)",
                               "4" = "Hijau (4)", "5" = "Emas (5)")) +
  labs(title = "GW_Score per Peringkat PROPER (VIOLATION)",
       x = "Peringkat PROPER", y = "GW_Score")

## -----------------------------------------------------------------------------
## 3. UJI ASUMSI KLASIK
## -----------------------------------------------------------------------------
# Uji dilakukan pada Pooled OLS terlebih dahulu sebagai basis,
# sebelum pemilihan model (Chow & Hausman).

model_pooled <- plm(
  GW_Score ~ SIZE + ROA + FOREIGN + VIOLATION + DER + SUBSECTOR,
  data  = pdata,
  model = "pooling"
)
summary(model_pooled)

## 3a. Uji Normalitas Residual (Jarque-Bera)
jarque.bera.test(residuals(model_pooled))
# H0: residual berdistribusi normal.
# p > 0.05 -> asumsi normalitas terpenuhi.

## 3b. Uji Multikolinearitas (VIF)
# VIF dihitung dari OLS biasa (bukan objek plm)
model_ols_check <- lm(
  GW_Score ~ SIZE + ROA + FOREIGN + VIOLATION + DER + SUBSECTOR,
  data = df
)
vif(model_ols_check)
# VIF < 10 (atau < 5 untuk standar lebih ketat) -> tidak ada multikolinearitas serius.

## 3c. Uji Heteroskedastisitas (Breusch-Pagan)
bptest(
  GW_Score ~ SIZE + ROA + FOREIGN + VIOLATION + DER + SUBSECTOR,
  data       = df,
  studentize = TRUE
)
# H0: homoskedastisitas (varian residual konstan).
# p < 0.05 -> ada heteroskedastisitas -> gunakan robust SE di tahap estimasi akhir.

## 3d. Uji Autokorelasi (Breusch-Godfrey untuk panel)
pbgtest(model_pooled)
# H0: tidak ada autokorelasi serial.
# p > 0.05 -> tidak ada masalah autokorelasi.

## -----------------------------------------------------------------------------
## 4. PEMILIHAN MODEL PANEL
## -----------------------------------------------------------------------------
## Tiga model dasar
model_common <- plm(
  GW_Score ~ SIZE + ROA + FOREIGN + VIOLATION + DER + SUBSECTOR,
  data  = pdata,
  model = "pooling"    # Common Effect Model (CEM)
)

model_fixed <- plm(
  GW_Score ~ SIZE + ROA + FOREIGN + VIOLATION + DER + SUBSECTOR,
  data  = pdata,
  model = "within"     # Fixed Effect Model (FEM)
)

model_random <- plm(
  GW_Score ~ SIZE + ROA + FOREIGN + VIOLATION + DER + SUBSECTOR,
  data  = pdata,
  model = "random"     # Random Effect Model (REM)
)

## 4a. Uji Chow: Common Effect vs Fixed Effect
pFtest(model_fixed, model_common)
# H0: Common Effect Model lebih sesuai.
# p < 0.05 -> tolak H0 -> pilih Fixed Effect, lanjut ke Hausman.
# p > 0.05 -> gunakan Common Effect (selesai).

## 4b. Uji Hausman: Fixed Effect vs Random Effect
phtest(model_fixed, model_random)
# H0: Random Effect lebih sesuai (efek acak tidak berkorelasi dengan regressor).
# p < 0.05 -> tolak H0 -> pilih Fixed Effect.
# p > 0.05 -> pilih Random Effect.

## -----------------------------------------------------------------------------
## 5. MODEL FINAL & PENGUJIAN HIPOTESIS
## -----------------------------------------------------------------------------
# Hasil uji Chow: p = 0.127 > 0.05 -> Common Effect Model (CEM) terpilih.
# Uji Hausman tidak dilanjutkan karena CEM sudah terpilih di tahap Chow.

model_final <- model_common  # Common Effect Model (Pooled OLS)

## Ringkasan model: koefisien, uji t, uji F, R-squared
summary(model_final)

## R-squared & Adjusted R-squared
cat("R-squared         :", summary(model_final)$r.squared[[1]], "\n")
cat("Adj. R-squared    :", summary(model_final)$r.squared[[2]], "\n")

## Heteroskedastisitas tidak terdeteksi (BP p = 0.335), namun robust SE
## tetap disertakan sebagai referensi tambahan:
coeftest(model_final, vcov. = vcovHC(model_final, type = "HC1"))

## -----------------------------------------------------------------------------
## 6. INTERPRETASI HASIL (panduan manual)
## -----------------------------------------------------------------------------
# Uji t per variabel (SIZE, ROA, FOREIGN, VIOLATION):
#   - Koefisien positif & p < 0.05 -> variabel berpengaruh positif signifikan
#     terhadap GW_Score (meningkatkan kecenderungan greenwashing)
#   - Koefisien negatif & p < 0.05 -> berpengaruh negatif signifikan
#   - p > 0.05 -> tidak berpengaruh signifikan
#
# Uji F (F-statistic) -> signifikansi simultan seluruh variabel independen
#
# R-squared -> proporsi variasi GW_Score yang dijelaskan model
#
# Catatan VIOLATION (X4):
#   Skor PROPER 1-5 (Hitam s.d. Emas). Koefisien negatif yang signifikan
#   akan mendukung hipotesis bahwa perusahaan dengan kinerja lingkungan lebih
#   baik cenderung memiliki gap narasi vs kinerja yang lebih kecil.
