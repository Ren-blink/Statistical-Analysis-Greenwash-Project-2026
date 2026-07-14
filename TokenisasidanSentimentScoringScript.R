# ==============================================================================
# TEXT MINING: TOKENISASI & SENTIMENT SCORING
# Studi kasus: Narasi Sustainability Report (Bahasa Indonesia) untuk Greenwashing Score
# ==============================================================================
#
# ALUR SCRIPT INI:
#   1. Setup package
#   2. Import data teks
#   3. Preprocessing (cleaning teks mentah)
#   4. Tokenisasi (memecah teks jadi kata per kata)
#   5. Hapus stopwords (kata umum yang tidak bermakna sentimen)
#   6. Sentiment scoring pakai leksikon InSet (Bahasa Indonesia)
#   7. Hitung Sentiment Score per dokumen/perusahaan/tahun
#   8. (Bonus) Frekuensi kata aspirasional -- relevan untuk indikator greenwashing
#
# ==============================================================================


# ------------------------------------------------------------------------------
# 1. SETUP PACKAGE
# ------------------------------------------------------------------------------
# tidytext   -> untuk tokenisasi & join dengan leksikon sentimen (gaya "tidy data")
# quanteda   -> untuk membangun corpus & document-feature matrix (dfm), berguna
#               kalau nanti mau lanjut ke analisis frekuensi/readability
# dplyr, stringr, readr -> manipulasi data & teks
# tibble     -> struktur data rapi

packages <- c("tidytext", "quanteda", "quanteda.textstats",
              "dplyr", "stringr", "readr", "tibble", "purrr", "pdftools")

installed <- packages %in% rownames(installed.packages())
if (any(!installed)) {
  install.packages(packages[!installed])
}

lapply(packages, library, character.only = TRUE)


# ------------------------------------------------------------------------------
# 2. IMPORT DATA TEKS
# ------------------------------------------------------------------------------
# Ganti bagian ini dengan cara kamu membaca data asli.
# Struktur yang dipakai di script ini: satu baris = satu dokumen/narasi
# (misalnya: satu baris per perusahaan per tahun, atau per bagian laporan)
#
# Kolom minimal yang dibutuhkan:
#   - doc_id : identitas dokumen (misal "PT_A_2023")
#   - text   : isi narasi sustainability report

folder_pdf <- "C:/Users/Rayhan Nauvan/OneDrive/Documents/Journals/Journal GREENWASH/Folder Report/TOBA"

file_list <- list.files(folder_pdf, pattern = "\\.pdf$", full.names = TRUE)

if(length(file_list) == 0) {
  stop("Tidak ada file PDF ditemukan di folder watch ", folder_pdf, "'. Cek kembali path foldernya")
}

extract_pdf_text <- function(path) {
  halaman <- pdftools::pdf_text(path)
  paste(halaman, collapse = " ")
}

reports <- tibble(
  doc_id = file_list %>% basename() %>% str_remove("\\.pdf$"),
  text   = map_chr(file_list, extract_pdf_text)
)

# Cek cepat: pastikan teks berhasil terbaca (tidak kosong) untuk tiap dokumen
reports %>%
  mutate(jumlah_karakter = str_length(text)) %>%
  select(doc_id, jumlah_karakter) %>%
  print()

# --- Kalau hanya mau coba dengan data dummy dulu (tanpa PDF), aktifkan ini: ---
# reports <- tibble(
#   doc_id = c("PT_A_2023", "PT_B_2023", "PT_C_2023"),
#   text = c(
#     "Perusahaan kami berkomitmen penuh terhadap keberlanjutan lingkungan dan telah mengurangi emisi karbon secara signifikan.",
#     "Kami berupaya menjaga kelestarian alam meskipun masih menghadapi tantangan dalam pengelolaan limbah industri.",
#     "Program CSR kami memberikan dampak positif bagi masyarakat sekitar, namun beberapa kritik terkait pencemaran belum sepenuhnya kami tanggapi."
#   )
# )


# ------------------------------------------------------------------------------
# 3. PREPROCESSING (CLEANING TEKS MENTAH)
# ------------------------------------------------------------------------------
# Tujuan: membersihkan teks dari elemen yang mengganggu analisis kata,
# seperti angka, tanda baca, dan huruf kapital yang tidak konsisten.

reports_clean <- reports %>%
  mutate(
    text_clean = text %>%
      str_to_lower() %>%                      # ubah semua ke huruf kecil
      str_replace_all("[[:punct:]]", " ") %>% # hapus tanda baca
      str_replace_all("[[:digit:]]", " ") %>% # hapus angka
      str_squish()                            # rapikan spasi berlebih
  )


# ------------------------------------------------------------------------------
# 4. TOKENISASI
# ------------------------------------------------------------------------------
# Tokenisasi = memecah kalimat/paragraf menjadi satuan kata (token).
# unnest_tokens() dari tidytext akan otomatis:
#   - memecah setiap teks jadi baris per kata
#   - tetap menyimpan doc_id, supaya kita tahu kata itu berasal dari dokumen mana
#
# Hasilnya: format "tidy" -> 1 baris = 1 kata + info dokumen asalnya

tokens <- reports_clean %>%
  unnest_tokens(output = word, input = text_clean, token = "words")

# Cek hasil tokenisasi
head(tokens, 15)


# ------------------------------------------------------------------------------
# 5. HAPUS STOPWORDS (BAHASA INDONESIA)
# ------------------------------------------------------------------------------
# Stopwords = kata-kata umum yang sering muncul tapi tidak membawa makna
# sentimen (contoh: "yang", "dan", "di", "ke", "untuk").
# Package tidytext tidak punya daftar stopwords Bahasa Indonesia bawaan,
# jadi kita ambil dari package "stopwords".

if (!"stopwords" %in% rownames(installed.packages())) install.packages("stopwords")
library(stopwords)

stopwords_id <- tibble(word = stopwords::stopwords("id", source = "stopwords-iso"))

tokens_clean <- tokens %>%
  anti_join(stopwords_id, by = "word") %>%
  filter(str_length(word) > 2)  # buang token terlalu pendek (biasanya sisa noise)

head(tokens_clean, 15)


# ------------------------------------------------------------------------------
# 6. SENTIMENT SCORING DENGAN LEKSIKON INSET (BAHASA INDONESIA)
# ------------------------------------------------------------------------------
# InSet Lexicon (Koto & Rahmaningtyas, 2017): leksikon sentimen Bahasa Indonesia
# berisi kata positif & negatif dengan bobot -5 s.d. +5.
# Sumber resmi: https://github.com/fajri91/InSet
#
# Kita unduh langsung dari GitHub supaya script ini reproducible.

folder_lexicon <- "C:/Users/Rayhan Nauvan/OneDrive/Documents/StatisticalAnalysisGreenwashProject2026/lexicon"

lex_pos <- read_tsv(paste0(folder_lexicon, "/positive.tsv"), col_names = c("word", "weight"), skip = 1, show_col_types = FALSE)
lex_neg <- read_tsv(paste0(folder_lexicon, "/negative.tsv"), col_names = c("word", "weight"), skip = 1, show_col_types = FALSE)

# Gabungkan jadi satu leksikon: kolom "word" dan "weight" (bobot sentimen)
lexicon_id <- bind_rows(lex_pos, lex_neg) %>%
  distinct(word, .keep_all = TRUE)

# --- Kalau tidak ada akses internet, pakai leksikon manual sederhana sebagai cadangan ---
# lexicon_id <- tibble(
#   word = c("berkomitmen", "keberlanjutan", "positif", "kritik", "pencemaran", "tantangan"),
#   weight = c(3, 3, 4, -2, -4, -1)
# )

# Join token dengan leksikon -> setiap kata yang cocok akan dapat skor
tokens_scored <- tokens_clean %>%
  inner_join(lexicon_id, by = "word")

head(tokens_scored, 15)


# ------------------------------------------------------------------------------
# 7. HITUNG SENTIMENT SCORE PER DOKUMEN
# ------------------------------------------------------------------------------
# Ada beberapa cara umum menghitung skor sentimen per dokumen:
#   a) Total skor mentah (jumlah bobot semua kata bersentimen)
#   b) Skor rata-rata per kata bersentimen
#   c) Skor ternormalisasi -> dibagi total kata dalam dokumen (lebih adil untuk
#      dokumen dengan panjang berbeda-beda, DIREKOMENDASIKAN untuk lintas dokumen)

doc_length <- tokens_clean %>%
  count(doc_id, name = "total_kata")

sentiment_score <- tokens_scored %>%
  group_by(doc_id) %>%
  summarise(
    skor_total   = sum(weight),
    jml_kata_pos = sum(weight > 0),
    jml_kata_neg = sum(weight < 0),
    .groups = "drop"
  ) %>%
  left_join(doc_length, by = "doc_id") %>%
  mutate(
    skor_ternormalisasi = skor_total / total_kata,   # ini yang biasanya dipakai sbg "Sentiment Score"
    label_sentimen = case_when(
      skor_total > 0  ~ "Positif",
      skor_total < 0  ~ "Negatif",
      TRUE            ~ "Netral"
    )
  )

print(sentiment_score)

#===============================================================================

# ------------------------------------------------------------------------------
# 8. KATA ASPIRASIONAL -- PENDEKATAN DATA-DRIVEN
# ------------------------------------------------------------------------------
# Daripada menebak sendiri kata mana yang "aspirasional" (rawan bias peneliti),
# pendekatan di sini membiarkan DATA yang menunjukkan kandidatnya:
#   8a. Hitung kata yang paling sering muncul di seluruh korpus (term frequency)
#   8b. Kamu validasi manual: kandidat mana yang benar aspirasional/klaim komitmen
#       (proses ini dilakukan SEKALI di luar R, misal di Excel)
#   8c. Baca kembali daftar yang sudah divalidasi, lalu hitung frekuensinya per dokumen
#
# Kenapa tetap perlu validasi manual? Karena "aspirasional" itu konsep semantik
# (soal MAKNA klaim), bukan pola statistik murni -- tidak ada algoritma yang bisa
# 100% otomatis menentukan itu tanpa human judgment. Tapi dengan cara ini,
# kandidatnya OBJEKTIF berasal dari data (kata yang benar-benar sering dipakai di
# laporanmu), bukan ditebak duluan oleh peneliti sebelum lihat datanya.

# --- 8a. HITUNG FREKUENSI KATA DI SELURUH KORPUS ---
# Kita hitung berapa kali tiap kata muncul, ditotal dari SEMUA dokumen sekaligus, 
# supaya kandidat yang muncul adalah kata yang memang umum dipakai lintas laporan
# (bukan cuma kebetulan sering di satu perusahaan saja).

frekuensi_kata <- tokens_clean %>%
  count(word, sort = TRUE, name = "frekuensi")

# Ambil top N kata (misal 150) sebagai KANDIDAT untuk divalidasi manual.
# N bisa disesuaikan -- makin besar N, makin lengkap tapi makin lama proses validasinya.
top_n_kandidat <- 150

kandidat_aspirasional <- frekuensi_kata %>%
  slice_max(frekuensi, n = top_n_kandidat) %>%
  mutate(
    # Dihitung hanya untuk kandidat top-N (bukan seluruh vocabulary) supaya cepat
    jumlah_dokumen_muncul = map_int(word, ~ sum(str_detect(reports_clean$text_clean, .x))),
    aspirasional = NA  # kolom kosong ini yang akan kamu isi manual
  )

# Ekspor ke CSV supaya bisa dibuka & diisi di Excel
write_csv(kandidat_aspirasional, "kandidat_kata_aspirasional.csv")
getwd("kandidat_kata_aspirasional.csv")
# --- 8b. VALIDASI MANUAL (DILAKUKAN DI LUAR R) ---
# 1. Buka file "kandidat_kata_aspirasional.csv" yang baru dibuat di folder kerja R kamu
#    (cek lokasinya dengan getwd())
# 2. Di kolom "aspirasional", isi TRUE untuk kata yang menurutmu benar mencerminkan
#    klaim/komitmen/citra positif (misal: "berkomitmen", "berkelanjutan", "inovatif",
#    "unggul", "bertanggung jawab"), dan FALSE untuk kata umum/netral/teknis yang
#    tidak relevan (misal: "perusahaan", "tahun", "laporan", "tabel")
# 3. Simpan file-nya (tetap format .csv), lalu lanjut ke bagian 8c di bawah

# --- 8c. BACA KEMBALI DAFTAR YANG SUDAH DIVALIDASI & HITUNG FREKUENSINYA ---
# PENTING: jalankan baris ini SETELAH kamu selesai isi kolom "aspirasional" manual.
# Kalau belum sempat validasi dan cuma mau lihat alurnya jalan dulu, boleh skip ke
# bagian bawah yang pakai fallback daftar manual sederhana.

kandidat_tervalidasi <- read_csv("C:/Users/Rayhan Nauvan/OneDrive/Documents/StatisticalAnalysisGreenwashProject2026/kandidat_kata_aspirasional.csv", show_col_types = FALSE)

kata_aspirasional <- kandidat_tervalidasi %>%
  filter(aspirasional == TRUE) %>%
  pull(word)

# Cek daftar kata aspirasional final yang akan dipakai
print(kata_aspirasional)

# --- Fallback: kalau belum sempat validasi manual dan mau coba jalankan dulu ---
# kata_aspirasional <- c("berkomitmen", "berkelanjutan", "peduli", "ramah lingkungan",
#                        "inovatif", "unggul", "terdepan", "bertanggung jawab",
#                        "berwawasan lingkungan", "hijau")

freq_aspirasional <- reports_clean %>%
  mutate(
    jml_aspirasional = str_count(text_clean, paste(kata_aspirasional, collapse = "|"))
  ) %>%
  left_join(doc_length, by = "doc_id") %>%
  mutate(rasio_aspirasional = jml_aspirasional / total_kata) %>%
  select(doc_id, jml_aspirasional, total_kata, rasio_aspirasional)

print(freq_aspirasional)


# ------------------------------------------------------------------------------
# 9. GABUNGKAN JADI SATU TABEL HASIL AKHIR
# ------------------------------------------------------------------------------
# Tabel ini yang nantinya bisa kamu gabungkan dengan data PROPER untuk
# membentuk Greenwashing Score (tahap regresi data panel selanjutnya).

hasil_akhir <- sentiment_score %>%
  left_join(freq_aspirasional, by = "doc_id")

print(hasil_akhir)

# Simpan hasil ke CSV
write_csv(hasil_akhir, "hasil_text_mining_sentimen_TOBA.csv")


# ------------------------------------------------------------------------------    
# 10. NORMALISASI MIN-MAX LINTAS SAMPEL -> SKOR NARASI (0-1)
# ------------------------------------------------------------------------------
# PENTING: normalisasi di sini dilakukan LINTAS SEMUA DOKUMEN SEKALIGUS
# (bukan per dokumen satu-satu), supaya perbandingan antar perusahaan/tahun adil.
# Ini kenapa tahap ini baru bisa dilakukan SETELAH semua dokumen selesai diproses
# di tahap 1-9 -- kamu butuh nilai MIN dan MAX dari SELURUH sampel dulu.
#
# Komponen yang dinormalisasi:
#   a) skor_ternormalisasi (dari sentiment scoring InSet, tahap 7)
#   b) rasio_aspirasional  (dari frekuensi kata aspirasional, tahap 8)
# Skor Narasi = rata-rata dari kedua komponen yang sudah di skala 0-1

normalize_minmax <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)) 
}

hasil_akhir <- hasil_akhir %>%
  mutate(
    sentimen_norm = normalize_minmax(skor_ternormalisasi),
    aspirasional_norm = normalize_minmax(rasio_aspirasional),
    skor_narasi = (sentimen_norm + aspirasional_norm) / 2
  )

print(hasil_akhir %>% select(
  doc_id, skor_ternormalisasi, sentimen_norm, rasio_aspirasional, aspirasional_norm, skor_narasi
))

# ------------------------------------------------------------------------------
# 10b. GABUNGKAN SEMUA CSV HASIL STEP 9 (JALANKAN SETELAH SEMUA PT SELESAI)
# ------------------------------------------------------------------------------
# Jalankan blok ini hanya SEKALI setelah semua PT selesai diproses (step 1-9).
# Hasilnya akan menjadi hasil_akhir gabungan yang siap dinormalisasi di step 10.

folder_hasil <- "hasil tes"   # <-- ganti sesuai nama folder penyimpanan CSV hasil step 9

file_csv <- list.files(folder_hasil,
                       pattern = "hasil_text_mining_sentimen.*\\.csv$",
                       full.names = TRUE)

hasil_akhir <- purrr::map(file_csv, readr::read_csv, show_col_types = FALSE) |>
  dplyr::bind_rows()

# Konfirmasi: pastikan semua PT & tahun terbaca
hasil_akhir |> dplyr::count(doc_id)

# Normalisasi min-max lintas seluruh sampel (step 10)
normalize_minmax <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

hasil_akhir <- hasil_akhir |>
  dplyr::mutate(
    sentimen_norm     = normalize_minmax(skor_ternormalisasi),
    aspirasional_norm = normalize_minmax(rasio_aspirasional),
    skor_narasi       = (sentimen_norm + aspirasional_norm) / 2
  )

# Simpan gabungan agar tidak perlu diulang
readr::write_csv(hasil_akhir, "hasil_text_mining_sentimen_GABUNGAN.csv")
cat("Tersimpan:", nrow(hasil_akhir), "baris ke hasil_text_mining_sentimen_GABUNGAN.csv\n")

# ------------------------------------------------------------------------------
# 11. IMPORT DATA PROPER & KONVERSI KE SKOR KINERJA AKTUAL (0-1)
# ------------------------------------------------------------------------------
# Data PROPER ini TIDAK dihasilkan dari text mining -- ini data sekunder yang kamu
# kumpulkan sendiri dari publikasi Kementerian Lingkungan Hidup dan Kehutanan (KLHK),
# biasanya berupa file Excel/CSV yang kamu susun manual per perusahaan per tahun.
#
# FORMAT FILE YANG DIHARAPKAN (silakan sesuaikan nama kolom bila beda):
#   doc_id        : HARUS PERSIS SAMA dengan doc_id di atas (misal "PT_ADRO_2021")
#                   supaya bisa di-join dengan benar
#   proper_rating : peringkat PROPER, boleh berupa TEKS ("Hitam","Merah","Biru",
#                   "Hijau","Emas") ATAU ANGKA (1-5), keduanya didukung di bawah

folder_proper <- "FolderProper"   # <-- ganti sesuai lokasi folder data PROPER kamu

# Baca dan gabungkan semua file CSV PROPER sekaligus
data_proper <- list.files(folder_proper,
                           pattern = "\\.csv$",
                           full.names = TRUE) |>
  purrr::map(readr::read_csv, show_col_types = FALSE) |>
  dplyr::bind_rows()

# Cek dulu bentuk datanya sebelum lanjut
head(data_proper)

# Konversi ordinal (1-5) ke skala 0-1 sesuai rumus:
konversi_proper <- function(skor_ordinal) {
  (skor_ordinal - 1) / (5 - 1) 
}

data_proper <- data_proper %>%
  mutate(
    proper_ordinal = as.numeric(proper_ordinal),
    skor_kinerja_aktual = konversi_proper(proper_ordinal)
  ) %>%
  select(doc_id, proper_ordinal, skor_kinerja_aktual)

print(data_proper)

data_proper %>%
  filter(is.na(proper_ordinal)) %>%
  select(doc_id)


# ------------------------------------------------------------------------------
# 12. HITUNG GW_SCORE (GAP/SELISIH) & KATEGORISASI
# ------------------------------------------------------------------------------
# GW_Score = |Skor Narasi - Skor Kinerja Aktual|
# Semakin besar, semakin besar indikasi greenwashing (narasi jauh lebih positif
# dibanding kinerja lingkungan aktualnya).

hasil_akhir <- hasil_akhir %>%
  left_join(data_proper, by = "doc_id")

# Cek dulu apakah ada doc_id yang tidak match (PROPER-nya NA) -- ini pertanda
# ada perbedaan format penulisan doc_id antara data teks dan data PROPER
hasil_akhir %>%
  filter(is.na(skor_kinerja_aktual)) %>%
  select(doc_id)

hasil_akhir <- hasil_akhir %>%
  mutate(
    GW_Score = abs(skor_narasi - skor_kinerja_aktual),
    kategori_greenwashing = case_when(
      GW_Score <= 0.20                     ~ "Fair disclosure",
      GW_Score > 0.20 & GW_Score <= 0.40    ~ "Indikasi greenwashing ringan",
      GW_Score > 0.40 & GW_Score <= 0.60    ~ "Indikasi greenwashing sedang",
      GW_Score > 0.60                       ~ "Indikasi greenwashing kuat"
    )
  )

print(hasil_akhir %>% select(doc_id, skor_narasi, skor_kinerja_aktual,
                             GW_Score, kategori_greenwashing))

# Simpan hasil akhir lengkap (termasuk GW_Score) ke CSV
write_csv(hasil_akhir, "hasil_greenwashing_score.csv")

# Tabel inilah yang kolom GW_Score-nya akan jadi VARIABEL DEPENDEN (GWIndex)
# di tahap regresi data panel selanjutnya (analisis_greenwashing.R)

# VISUALISASI HEATMAP
library(ggplot2)
library(dplyr)
library(readr)
library(stringr)

hasil_akhir <- read_csv("hasil_greenwashing_score.csv", show_col_types = FALSE)

plot_data <- hasil_akhir |>
  mutate(
    PT    = str_extract(doc_id, "(?<=PT_)[A-Z]+"),
    tahun = str_extract(doc_id, "\\d{4}$")
  )

ggplot(plot_data, aes(x = tahun, y = PT, fill = GW_Score)) +
  geom_tile(color = "white", linewidth = 0.8) +
  geom_text(aes(label = round(GW_Score, 2)), size = 3.2, color = "white", fontface = "bold") +
  scale_fill_gradientn(
    colours = c("#2E7D32", "#FDD835", "#E53935"),
    values  = scales::rescale(c(0, 0.2, 0.4, 1)),
    limits  = c(0, 1),
    name    = "GW_Score"
  ) +
  labs(
    title    = "Greenwashing Score per Perusahaan per Tahun",
    subtitle = "0 = tidak ada gap  |  1 = gap maksimal antara narasi dan kinerja aktual",
    x        = "Tahun",
    y        = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid  = element_blank(),
    axis.text.y = element_text(face = "bold")
  )

