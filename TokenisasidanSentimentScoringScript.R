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
              "dplyr", "stringr", "readr", "tibble", "purrr")

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

# CONTOH data dummy -- HAPUS/ganti dengan data asli kamu
# (misalnya baca dari csv: reports <- read_csv("data_sustainability_report.csv"))
reports <- tibble(
  doc_id = c("PT_A_2023", "PT_B_2023", "PT_C_2023"),
  text = c(
    "Perusahaan kami berkomitmen penuh terhadap keberlanjutan lingkungan dan telah mengurangi emisi karbon secara signifikan.",
    "Kami berupaya menjaga kelestarian alam meskipun masih menghadapi tantangan dalam pengelolaan limbah industri.",
    "Program CSR kami memberikan dampak positif bagi masyarakat sekitar, namun beberapa kritik terkait pencemaran belum sepenuhnya kami tanggapi."
  )
)

# Kalau data kamu berupa banyak file .txt terpisah (misal 1.txt, 2.txt, dst),
# kamu bisa pakai pola berikut:
#
# file_list <- list.files("folder_data/", pattern = "\\.txt$", full.names = TRUE)
# reports <- tibble(
#   doc_id = basename(file_list),
#   text   = map_chr(file_list, ~ paste(readLines(.x, warn = FALSE), collapse = " "))
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

url_pos <- "https://raw.githubusercontent.com/fajri91/InSet/master/positive.tsv"
url_neg <- "https://raw.githubusercontent.com/fajri91/InSet/master/negative.tsv"

lex_pos <- read_tsv(url_pos, col_names = c("word", "weight"), show_col_types = FALSE)
lex_neg <- read_tsv(url_neg, col_names = c("word", "weight"), show_col_types = FALSE)

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


# ------------------------------------------------------------------------------
# 8. (BONUS) FREKUENSI KATA ASPIRASIONAL
# ------------------------------------------------------------------------------
# Di riset greenwashing, selain skor sentimen, sering dihitung juga frekuensi
# "kata aspirasional" -- kata yang menonjolkan citra positif/berkomitmen tapi
# belum tentu dibarengi bukti/tindakan nyata (indikasi greenwashing kalau
# frekuensinya tinggi tapi kinerja lingkungan aktual -- misalnya skor PROPER -- rendah).
#
# Silakan sesuaikan daftar kata ini dengan kajian literatur greenwashing kamu.

kata_aspirasional <- c("berkomitmen", "berkelanjutan", "peduli", "ramah lingkungan",
                       "inovatif", "unggul", "terdepan", "bertanggung jawab",
                       "berwawasan lingkungan", "hijau")

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
write_csv(hasil_akhir, "hasil_text_mining_sentimen.csv")