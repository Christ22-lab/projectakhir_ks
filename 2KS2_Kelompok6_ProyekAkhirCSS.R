library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(readxl)
library(lmtest)
library(car)
library(DT)
library(haven)  # For SPSS files
library(readr)  # For CSV files
library(foreign) # For additional formats
library(e1071)  # Ditambahkan untuk menghitung Skewness

# Modern CSS styling with enhanced icons
modern_css <- "
<style>
  @import url('https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700&display=swap');
@import url('https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0/css/all.min.css');

/* Global Styling */
  body, .content-wrapper, .right-side {
    font-family: 'Inter', sans-serif !important;
    background: #f4f6f9 !important; /* MODIFIKASI: Latar belakang global lebih netral */
  }

/* Header Styling */
  .main-header .navbar {
    background: linear-gradient(90deg, #2C3E50, #3498DB) !important;
                                border: none !important;
                                box-shadow: 0 2px 10px rgba(0,0,0,0.1) !important;
  }

.main-header .navbar-custom-menu > .navbar-nav > li > .dropdown-menu {
  background: white !important;
}

.main-header .logo {
  background: linear-gradient(90deg, #2C3E50, #3498DB) !important;
                              color: white !important;
                              border: none !important;
                              font-weight: 600 !important;
                              font-size: 18px !important;
}

/* Sidebar Styling */
  .main-sidebar, .left-side {
    background: linear-gradient(180deg, #2C3E50 0%, #34495E 100%) !important;
  }

.sidebar-menu > li > a {
  color: #ECF0F1 !important;
    border-left: 3px solid transparent !important;
  transition: all 0.3s ease !important;
  font-weight: 500 !important;
}

.sidebar-menu > li:hover > a, .sidebar-menu > li.active > a {
  background: rgba(52, 152, 219, 0.2) !important;
  border-left: 3px solid #3498DB !important;
  color: white !important;
}

.sidebar-menu > li > a > .fa {
  color: #3498DB !important;
}

/* Content Area */
  .content-wrapper {
    background: #f4f6f9 !important; /* MODIFIKASI: Latar belakang konten netral */
      min-height: 100vh !important;
  }

.content {
  padding: 20px !important;
}

/* Box Styling */
  .box {
    border-radius: 12px !important; /* MODIFIKASI: Radius lebih subtle */
      box-shadow: 0 4px 6px -1px rgba(0, 0, 0, 0.1), 0 2px 4px -1px rgba(0, 0, 0, 0.06) !important; /* MODIFIKASI: Shadow lebih modern dan halus */
      border: none !important;
    margin-bottom: 25px !important;
    transition: transform 0.3s ease, box-shadow 0.3s ease !important;
    background: white !important;
  }

.box:hover {
  transform: translateY(-5px) !important;
  box-shadow: 0 10px 15px -3px rgba(0, 0, 0, 0.1), 0 4px 6px -2px rgba(0, 0, 0, 0.05) !important; /* MODIFIKASI: Shadow hover lebih jelas */
}

.box-header {
  border-radius: 12px 12px 0 0 !important;
  padding: 20px !important;
  border-bottom: 1px solid #f0f0f0 !important; /* MODIFIKASI: Border header lebih tipis */
}

.box-primary > .box-header {
  background: linear-gradient(135deg, #3498DB, #2980B9) !important;
                              color: white !important;
}

.box-info > .box-header {
  background: linear-gradient(135deg, #1ABC9C, #16A085) !important;
                              color: white !important;
}

.box-success > .box-header {
  background: linear-gradient(135deg, #2ECC71, #27AE60) !important;
                              color: white !important;
}

.box-warning > .box-header {
  background: linear-gradient(135deg, #F39C12, #E67E22) !important;
                              color: white !important;
}

.box-title {
  font-weight: 600 !important;
  font-size: 18px !important;
  margin: 0 !important;
}

.box-body {
  padding: 25px !important;
  background-color: #fdfdfd !important; /* MODIFIKASI: Latar belakang body box sedikit off-white */
    border-radius: 0 0 12px 12px !important;
}

/* --- MODIFIKASI UTAMA UNTUK HASIL REGRESI --- */
  
  /* Bagian ini mengubah 'result-section' menjadi seperti kartu yang rapi */
  .result-section {
    background: white !important; /* Latar belakang kartu putih bersih */
      border: 1px solid #eef2f7 !important; /* Border tipis untuk definisi */
    padding: 0 !important; /* Padding di-nol-kan karena header akan mengaturnya */
      margin: 20px 0 !important; /* Jarak antar kartu */
      border-radius: 12px !important; /* Radius yang konsisten dengan .box */
      box-shadow: 0 2px 4px rgba(0,0,0,0.04) !important; /* Shadow sangat halus untuk kedalaman */
      overflow: hidden; /* Memastikan header tidak keluar dari radius */
  }

/* Header di dalam kartu interpretasi */
  .interpretation-header {
    background: linear-gradient(135deg, #6a82fb, #3d5af1) !important; /* Gradient baru yang lebih fresh */
                                color: white !important;
                                padding: 15px 20px !important;
                                margin: 0 !important; /* Dihilangkan margin negatif */
                                  border-radius: 0 !important; /* Radius diatur oleh .result-section */
                                  border-bottom: 1px solid #eef2f7 !important;
                                font-weight: 600 !important;
  }

/* Menambahkan padding untuk konten di bawah header kartu */
  .result-section p, .result-section div, .result-section h5, .result-section ul {
    padding: 0 20px; /* Atur padding kiri-kanan */
  }

.result-section > p, .result-section > div, .result-section > h5, .result-section > ul {
  margin-bottom: 15px;
  margin-top: 15px;
}


/* -- AKHIR MODIFIKASI UTAMA -- */
  
  
  /* Icon Enhancement */
  .fa, .fas, .far, .fab {
    margin-right: 8px !important;
  }

/* Button Styling */
  .btn {
    border-radius: 8px !important; /* MODIFIKASI: Radius tombol lebih modern */
      font-weight: 600 !important;
    padding: 10px 25px !important;
    transition: all 0.3s ease !important;
    border: none !important;
    text-transform: uppercase !important;
    letter-spacing: 0.5px !important;
    box-shadow: 0 1px 3px rgba(0,0,0,0.1) !important;
  }

.btn-primary {
  background: #3498DB !important;
    box-shadow: 0 4px 15px rgba(52, 152, 219, 0.2) !important;
}

.btn-primary:hover {
  background: #2980B9 !important;
    transform: translateY(-2px) !important;
  box-shadow: 0 6px 20px rgba(52, 152, 219, 0.3) !important;
}

/* Table Styling */
  .table {
    border-radius: 10px !important;
    overflow: hidden !important;
    box-shadow: 0 0 20px rgba(0,0,0,0.05) !important;
  }

.table thead th {
  background: #34495E !important;
    color: white !important;
  font-weight: 600 !important;
  border: none !important;
  padding: 15px !important;
}

/* Text Output Styling */
  pre {
    background: #f0f2f5 !important;
      border: 1px solid #e0e0e0 !important;
    border-radius: 10px !important;
    padding: 20px !important;
    font-family: 'Monaco', 'Consolas', monospace !important;
    font-size: 13px !important;
    line-height: 1.5 !important;
    color: #2C3E50 !important;
      box-shadow: inset 0 2px 5px rgba(0,0,0,0.05) !important;
  }
.selectize-dropdown,
.selectize-dropdown.form-control {
  z-index: 9999 !important;
  position: absolute !important;
}


</style>
  "

# Fungsi untuk menghapus pencilan menggunakan IQR
remove_outliers <- function(data, variable) {
  cat("\nMengolah variabel:", variable, "\n")
  cat("Jenis data:", class(data[[variable]]), "\n")
  Q1 <- quantile(data[[variable]], 0.25, na.rm = TRUE)
  Q3 <- quantile(data[[variable]], 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  cat("Batas bawah IQR:", lower_bound, "\n")
  cat("Batas atas IQR:", upper_bound, "\n")
  data <- data %>% filter(data[[variable]] >= lower_bound & data[[variable]] <= upper_bound)
  cat("Pencilan pada variabel", variable, "telah dihapus menggunakan metode IQR.\n")
  return(data)
}

# Enhanced function to read multiple file formats
read_data_file <- function(file_path, file_ext) {
  switch(file_ext,
         ".xlsx" = read_excel(file_path),
         ".xls" = read_excel(file_path),
         ".csv" = read_csv(file_path),
         ".sav" = read_sav(file_path),
         ".dta" = read_dta(file_path),
         ".txt" = read_delim(file_path, delim = "\t"),
         ".tsv" = read_tsv(file_path),
         stop("Format file tidak didukung")
  )
}

# Fungsi untuk memproses data contoh dengan multiple Y variables
process_data <- function(data) {
  # Asumsi kolom: Tanggal_Y, Beras_Y, Beras_Y1, Beras_Y2, Beras_Y3, Beras_Y4, Beras_Y5, Beras_Y6, Tanggal_X, Tavg_X1, RR_X2
  expected_cols <- c("Tanggal_Y", "Beras_Y", "Beras_Y1", "Beras_Y2", "Beras_Y3", "Beras_Y4", "Beras_Y5", "Beras_Y6", "Tanggal_X", "Tavg_X1", "RR_X2")
  
  if (ncol(data) >= length(expected_cols)) {
    colnames(data)[1:length(expected_cols)] <- expected_cols
  } else {
    # Fallback untuk data dengan struktur berbeda
    colnames(data) <- c("Tanggal_Y", "Beras_Y", "Tanggal_X", "Tavg_X1", "RR_X2")[1:ncol(data)]
  }
  
  data$Tanggal_Y <- as.Date(data$Tanggal_Y, format = "%d-%m-%Y")
  data$Tanggal_X <- as.Date(data$Tanggal_X, format = "%d-%m-%Y")
  
  # Pilih kolom yang tersedia
  available_cols <- intersect(expected_cols, colnames(data))
  data_clean <- data %>% select(all_of(available_cols))
  
  data_clean <- data_clean %>% filter(!is.na(Tanggal_Y) & !is.na(Tanggal_X))
  merged_data <- merge(data_clean, data_clean, by.x = "Tanggal_X", by.y = "Tanggal_Y", suffixes = c("", "_X"))
  
  final_data_cleaned <- merged_data %>% filter(Tavg_X1 != 888 & Tavg_X1 != 999 & RR_X2 != 888 & RR_X2 != 999)
  
  # Convert Y variables to numeric
  y_vars <- c("Beras_Y", "Beras_Y1", "Beras_Y2", "Beras_Y3", "Beras_Y4", "Beras_Y5", "Beras_Y6")
  available_y_vars <- intersect(y_vars, colnames(final_data_cleaned))
  
  for (var in available_y_vars) {
    final_data_cleaned[[var]] <- as.numeric(gsub(",", "", final_data_cleaned[[var]]))
  }
  
  final_data_cleaned$Tavg_X1 <- as.numeric(final_data_cleaned$Tavg_X1)
  final_data_cleaned$RR_X2 <- as.numeric(final_data_cleaned$RR_X2)
  
  # Remove outliers for all Y variables and X variables
  for (var in available_y_vars) {
    if (var %in% colnames(final_data_cleaned)) {
      final_data_cleaned <- remove_outliers(final_data_cleaned, var)
    }
  }
  
  final_data_cleaned <- final_data_cleaned %>%
    remove_outliers(., "Tavg_X1") %>%
    remove_outliers(., "RR_X2")
  
  # Select relevant columns
  final_cols <- c(available_y_vars, "Tavg_X1", "RR_X2")
  final_data_no_outliers <- final_data_cleaned %>% select(all_of(intersect(final_cols, colnames(final_data_cleaned))))
  
  return(final_data_no_outliers)
}

# Fungsi untuk memproses data pengguna dengan variabel yang dapat disesuaikan
process_user_custom_data <- function(data, num_vars, var_labels) {
  expected_cols <- num_vars + 1
  if (ncol(data) < expected_cols) {
    stop(paste("Data harus memiliki minimal", expected_cols, "kolom (Y dan", num_vars, "variabel X)"))
  }
  
  selected_data <- data[, 1:expected_cols]
  col_names <- c("Y", paste0("X", 1:num_vars))
  colnames(selected_data) <- col_names
  
  for (col in col_names) {
    selected_data[[col]] <- as.numeric(selected_data[[col]])
  }
  
  selected_data <- selected_data %>% filter(complete.cases(.))
  
  for (col in col_names) {
    selected_data <- remove_outliers(selected_data, col)
  }
  
  return(selected_data)
}

# UI: Bagian Antarmuka Pengguna
ui <- dashboardPage(
  dashboardHeader(title = HTML("<i class='fas fa-chart-line'></i> Dashboard Analisis ")),
  dashboardSidebar(
    sidebarMenu(
      menuItem(HTML("<i class='fas fa-home'></i> Beranda"), tabName = "home", icon = NULL),
      menuItem(HTML("<i class='fas fa-info-circle'></i> Metadata Statistik"), tabName = "metadata", icon = NULL),
      menuItem(HTML("<i class='fas fa-database'></i> Data Contoh"), tabName = "example_data", icon = NULL),
      menuItem(HTML("<i class='fas fa-upload'></i> Import Data Custom"), tabName = "import_custom", icon = NULL),
      menuItem(HTML("<i class='fas fa-chart-bar'></i> Statistik Deskriptif"), tabName = "desc_stats", icon = NULL),
      menuItem(HTML("<i class='fas fa-chart-area'></i> Visualisasi Data"), tabName = "visualization", icon = NULL),
      menuItem(HTML("<i class='fas fa-cogs'></i> Model Regresi & Hasil"), tabName = "regression", icon = NULL)
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML(modern_css))
    ),
    tabItems(
      # Halaman Beranda
      tabItem(tabName = "home",
              fluidRow(
                box(title = HTML("<i class='fas fa-rocket'></i> Selamat Datang di Dashboard Analisis"), 
                    status = "primary", solidHeader = TRUE, width = 12,
                    div(style = "text-align: center; padding: 20px;",
                        h1(HTML("<i class='fas fa-chart-line'></i> Platform Analisis Statistik Terpadu"), style = "color: #2C3E50; margin-bottom: 20px;"),
                        p("Dashboard ini memungkinkan Anda menganalisis hubungan kompleks antara berbagai faktor dengan menggunakan teknologi statistik terdepan.", 
                          style = "font-size: 16px; color: #34495E; margin-bottom: 30px;")
                    )
                )
              ),
              
              fluidRow(
                box(title = HTML("<i class='fas fa-star'></i> Fitur Unggulan Platform"), status = "info", solidHeader = TRUE, width = 6,
                    div(class = "feature-card",
                        HTML("</i>"),
                        h4(HTML("<i class='fas fa-check-circle'></i><strong> Analisis Data Contoh Terintegrasi</strong>"), style = "margin: 0;"),
                        p("Eksplorasi harga berbagai jenis beras dengan faktor cuaca", style = "margin: 5px 0 0 0;")
                    ),
                    div(class = "feature-card",
                        p(HTML("</i>"),
                          h4(HTML("<i class='fas fa-check-circle'></i><strong> Import Multi-Format Fleksibel</strong>"), style = "margin: 0;")),
                        p("Upload Excel, CSV, SPSS, Stata dan format lainnya", style = "margin: 5px 0 0 0;")
                    ),
                    div(class = "feature-card",
                        p(HTML("</i>"),
                          h4(HTML("<i class='fas fa-check-circle'></i><strong> Visualisasi Data Interaktif</strong>"), style = "margin: 0;")),
                        p("Grafik modern dan interpretasi mendalam", style = "margin: 5px 0 0 0;")
                    ),
                    div(class = "feature-card",
                        p(HTML("</i>"),
                          h4(HTML("<i class='fas fa-check-circle'></i><strong> Model Regresi Linier Berganda</strong>"), style = "margin: 0;")),
                        p("Analisis inferensia dengan validasi model", style = "margin: 5px 0 0 0;")
                    )
                ),
                
                box(title = HTML("<i class='fas fa-question-circle'></i> Panduan Cepat"), status = "warning", solidHeader = TRUE, width = 6,
                    h4(HTML("<i class='fas fa-rocket'></i><strong> Langkah-langkah Penggunaan:</strong>")),
                    tags$ol(
                      tags$li(HTML("<i class='fas fa-angle-right'></i> Kunjungi tab 'Metadata Statistik' untuk memahami konsep analisis")),
                      tags$li(HTML("<i class='fas fa-angle-right'></i> Pilih 'Data Contoh' untuk eksplorasi analisis harga beras dan cuaca")),
                      tags$li(HTML("<i class='fas fa-angle-right'></i> Atau gunakan 'Import Data Custom' untuk data Anda sendiri")),
                      tags$li(HTML("<i class='fas fa-angle-right'></i> Lihat 'Statistik Deskriptif' untuk ringkasan data")),
                      tags$li(HTML("<i class='fas fa-angle-right'></i> Eksplorasi 'Visualisasi Data' untuk pola dan tren")),
                      tags$li(HTML("<i class='fas fa-angle-right'></i> Analisis lengkap di 'Model Regresi & Hasil'"))
                    ),
                    br(),
                    downloadButton("downloadGuidebook", HTML("</i> Unduh Panduan Lengkap"), class = "btn-primary btn-lg")
                )
              ),
              
              fluidRow(
                box(title = HTML("<i class='fas fa-play-circle'></i> Tutorial Video Interaktif"), status = "success", solidHeader = TRUE, width = 12,
                    div(style = "text-align: center;",
                        tags$iframe(width = "100%", height = "400", 
                                    src = "https://www.youtube.com/embed/CMaeLS2GenU?si=xZZ15e32PdPPZ5OK", 
                                    frameborder = "0", 
                                    allow = "accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture", 
                                    allowfullscreen = TRUE,
                                    style = "max-width: 800px; border-radius: 15px; box-shadow: 0 8px 25px rgba(0,0,0,0.15);")
                    )
                )
              )
      ),
      
      # Halaman Metadata Statistik (Versi Ditingkatkan dengan Definisi BPS)
      tabItem(tabName = "metadata",
              fluidRow(
                box(title = HTML("<i class='fas fa-book'></i> Metadata Statistik & Metodologi"),
                    status = "primary", solidHeader = TRUE, width = 12,
                    div(style = "text-align: center; padding: 20px;",
                        h2(HTML("<i class='fas fa-microscope'></i> Pusat Informasi Konsep & Definisi Statistik"), style = "color: #2C3E50;"),
                        p("Memahami konsep, definisi, dan klasifikasi statistik yang digunakan dalam analisis sesuai dengan standar Badan Pusat Statistik (BPS).", style = "font-size: 16px; color: #34495E;")
                    )
                )
              ),
              
              fluidRow(
                box(title = HTML("<i class='fas fa-tasks'></i> Tahapan Kegiatan Statistik"),
                    status = "info", solidHeader = TRUE, width = 6,
                    div(class = "metadata-item",
                        h4(HTML("<i class='fas fa-calculator'></i> 1. Analisis Statistik Deskriptif"), style = "color: #2C3E50; margin-bottom: 10px;"),
                        p(HTML("<strong>Definisi:</strong> Kegiatan untuk memberikan gambaran atau deskripsi suatu data tanpa melakukan generalisasi. Statistik deskriptif meringkas kumpulan data menjadi informasi yang lebih sederhana dan mudah dipahami melalui ukuran pemusatan, penyebaran, dan distribusi frekuensi."))
                    ),
                    
                    div(class = "metadata-item",
                        h4(HTML("<i class='fas fa-chart-line'></i> 2. Analisis Statistik Inferensia"), style = "color: #2C3E50; margin-bottom: 10px;"),
                        p(HTML("<strong>Definisi:</strong> Metode yang digunakan untuk menganalisis data sampel dan hasilnya diberlakukan untuk populasi yang lebih besar. Tujuannya adalah untuk menarik kesimpulan (inferensi) mengenai karakteristik populasi berdasarkan data sampel dengan tingkat kepercayaan tertentu."))
                    ),
                    
                    div(class = "metadata-item",
                        h4(HTML("<i class='fas fa-cogs'></i> 3. Pemodelan Regresi & Validasi"), style = "color: #2C3E50; margin-bottom: 10px;"),
                        p(HTML("<strong>Definisi:</strong> Suatu metode statistik untuk mengestimasi hubungan fungsional antara variabel dependen dengan satu atau lebih variabel independen. Model yang dihasilkan kemudian diuji validitasnya untuk memastikan asumsi-asumsi statistik terpenuhi sehingga kesimpulan yang ditarik dapat dipertanggungjawabkan."))
                    )
                ),
                
                box(title = HTML("<i class='fas fa-tags'></i> Konsep & Definisi Variabel"),
                    status = "warning", solidHeader = TRUE, width = 6,
                    div(class = "metadata-item",
                        h4(HTML("<i class='fas fa-bullseye'></i> Variabel Dependen (Y) - Variabel Terikat"), style = "color: #2C3E50; margin-bottom: 10px;"),
                        p(HTML("<strong>Definisi BPS:</strong> Variabel yang nilainya dipengaruhi atau dijelaskan oleh variabel lain. Dalam konteks ini, variabel tersebut adalah <strong>Harga Beras (Rupiah/Kg)</strong>, yang pergerakannya coba diprediksi oleh model.")),
                        p(HTML("<strong>Jenis Variabel:</strong> Kuantitatif kontinu dengan skala pengukuran rasio."))
                    ),
                    
                    div(class = "metadata-item",
                        h4(HTML("<i class='fas fa-arrows-alt-h'></i> Variabel Independen (X) - Variabel Bebas"), style = "color: #2C3E50; margin-bottom: 10px;"),
                        p(HTML("<strong>Definisi BPS:</strong> Variabel yang diduga mempengaruhi atau menjelaskan perubahan pada variabel dependen. Variabel ini juga dikenal sebagai variabel penjelas (eksplanatori) atau prediktor.")),
                        p(HTML("<strong>X1: Suhu Udara Rata-rata (°C)</strong> - Rata-rata suhu udara harian, mingguan, atau bulanan di suatu wilayah. Diukur dalam skala interval.")),
                        p(HTML("<strong>X2: Curah Hujan (mm)</strong> - Ketinggian air hujan yang terkumpul dalam tempat yang datar, tidak menguap, tidak meresap, dan tidak mengalir. Diukur dalam skala rasio."))
                    )
                )
              ),
              
              fluidRow(
                box(title = HTML("<i class='fas fa-microscope'></i> Metode Analisis & Kriteria Uji"),
                    status = "success", solidHeader = TRUE, width = 12,
                    fluidRow(
                      column(6,
                             h4(HTML("<strong>Metode Estimasi & Uji Signifikansi</strong>")),
                             tags$ul(
                               tags$li(HTML("<strong>Regresi Linier Berganda:</strong> Teknik untuk memodelkan hubungan linier antara variabel terikat dengan dua atau lebih variabel bebas.")),
                               tags$li(HTML("<strong>Koefisien Determinasi (R²):</strong> Mengukur seberapa baik variabel independen dapat menjelaskan variasi variabel dependen. Nilai berkisar 0-1, semakin tinggi semakin baik modelnya.")),
                               tags$li(HTML("<strong>Uji F (Uji Simultan):</strong> Menentukan kelayakan model. <strong>Kriteria:</strong> Model layak (signifikan) jika nilai <strong>p-value < 0.05</strong>.")),
                               tags$li(HTML("<strong>Uji t (Uji Parsial):</strong> Menentukan signifikansi pengaruh masing-masing variabel independen. <strong>Kriteria:</strong> Variabel berpengaruh signifikan jika nilai <strong>p-value < 0.05</strong>."))
                             )
                      ),
                      column(6,
                             h4(HTML("<strong>Asumsi Klasik & Uji Validasi Model</strong>")),
                             tags$ul(
                               tags$li(HTML("<strong>Uji Normalitas (Shapiro-Wilk):</strong> Menguji apakah sisaan (residual) berdistribusi normal. <strong>Kriteria:</strong> Asumsi terpenuhi jika <strong>p-value > 0.05</strong>.")),
                               tags$li(HTML("<strong>Uji Homoskedastisitas (Breusch-Pagan):</strong> Menguji apakah varians sisaan bersifat konstan. <strong>Kriteria:</strong> Asumsi terpenuhi (tidak ada heteroskedastisitas) jika <strong>p-value > 0.05</strong>.")),
                               tags$li(HTML("<strong>Uji Multikolinearitas (VIF):</strong> Mendeteksi korelasi kuat antar variabel independen. <strong>Kriteria:</strong> Tidak ada masalah multikolinearitas jika nilai <strong>VIF < 10</strong> (beberapa ahli menyarankan VIF < 5)."))
                             )
                      )
                    )
                )
              )
      ),
      
      # Halaman Data Contoh
      tabItem(tabName = "example_data",
              fluidRow(
                box(title = HTML("<i class='fas fa-chart-line'></i> Eksplorasi Data Contoh Beras & Cuaca"), 
                    status = "primary", solidHeader = TRUE, width = 12,
                    div(style = "text-align: center; padding: 15px;",
                        h3(HTML("<i class='fas fa-seedling'></i> Analisis Hubungan Harga Beras dengan Faktor Cuaca"), style = "color: #2C3E50; margin-bottom: 15px;"),
                        p("Jelajahi bagaimana suhu rata-rata dan curah hujan mempengaruhi harga berbagai jenis beras", 
                          style = "font-size: 16px; color: #34495E;")
                    )
                )
              ),
              
              fluidRow(
                box(title = HTML("<i class='fas fa-bullseye'></i> Pilih Jenis Beras untuk Analisis"), status = "info", solidHeader = TRUE, width = 12,
                    fluidRow(
                      column(8,
                             h4(HTML("<i class='fas fa-seedling'></i> Pilih Varietas Beras:"), style = "color: #2C3E50; margin-bottom: 15px;"),
                             p("Setiap jenis beras memiliki karakteristik harga yang unik dan merespons faktor cuaca dengan cara yang berbeda.", style = "margin-bottom: 15px;"),
                             uiOutput("y_variable_selector")
                      ),
                      column(4,
                             div(class = "stats-highlight",
                                 h4(HTML("<i class='fas fa-lightbulb'></i> Tips Analisis"), style = "margin: 0;"),
                                 br(),
                                 p(HTML("<i class='fas fa-search'></i> Bandingkan berbagai jenis beras")),
                                 p(HTML("<i class='fas fa-poll'></i> Perhatikan pola harga vs cuaca")),
                                 p(HTML("<i class='fas fa-chart-line'></i> Gunakan untuk prediksi harga")),
                                 p(HTML("<i class='fas fa-check-square'></i> Validasi dengan data historis"))
                             )
                      )
                    )
                )
              ),
              
              fluidRow(
                box(title = HTML("<i class='fas fa-table'></i> Data Contoh yang Telah Dibersihkan"), status = "success", solidHeader = TRUE, width = 12,
                    div(style = "margin-bottom: 20px;",
                        h4(HTML("<i class='fas fa-table'></i> Data Analisis Lengkap:"), style = "color: #2C3E50; margin-bottom: 10px;"),
                        p("Semua data yang telah dibersihkan dan siap untuk analisis dengan fitur pencarian dan sorting:", style = "color: #34495E;")
                    ),
                    DT::dataTableOutput("exampleCleanedData")
                )
              )
      ),
      
      # Halaman Import Data Custom
      tabItem(tabName = "import_custom",
              fluidRow(
                box(title = HTML("<i class='fas fa-folder-open'></i> Platform Import Data Kustom"), 
                    status = "primary", solidHeader = TRUE, width = 12,
                    div(style = "text-align: center; padding: 15px;",
                        h3(HTML("<i class='fas fa-rocket'></i> Analisis Data Anda Sendiri"), style = "color: #2C3E50; margin-bottom: 15px;"),
                        p("Ikuti 3 langkah mudah untuk menganalisis data Anda dengan teknologi regresi linier berganda", 
                          style = "font-size: 16px; color: #34495E;")
                    )
                )
              ),
              
              fluidRow(
                box(title = HTML("<i class='fas fa-list-ol'></i> Langkah 1: Konfigurasi Analisis"), status = "info", solidHeader = TRUE, width = 12,
                    fluidRow(
                      column(8,
                             h4(HTML("<i class='fas fa-sitemap'></i> Tentukan Struktur Data Anda:"), style = "color: #2C3E50; margin-bottom: 15px;"),
                             p("Berapa banyak faktor (variabel independen) yang ingin Anda analisis pengaruhnya terhadap variabel target?", style = "margin-bottom: 15px;"),
                             numericInput("num_vars", "Jumlah Faktor Prediktor:", 
                                          value = 2, min = 1, max = 10, step = 1),
                             p(em("Contoh: Untuk menganalisis pengaruh suhu dan curah hujan terhadap harga beras, pilih 2"), style = "color: #7F8C8D;")
                      ),
                      column(4,
                             div(class = "stats-highlight",
                                 h4(HTML("<i class='fas fa-clipboard-list'></i> Panduan Cepat"), style = "margin: 0;"),
                                 br(),
                                 p(HTML("<i class='fas fa-bullseye'></i> 1 Variabel Target (Y)")),
                                 p(HTML("<i class='fas fa-arrows-alt-h'></i> 1-10 Faktor Prediktor (X)")),
                                 p(HTML("<i class='fas fa-sort-numeric-down'></i> Minimum 20 observasi")),
                                 p(HTML("<i class='fas fa-file-excel'></i> Format file yang didukung"))
                             )
                      )
                    )
                )
              ),
              
              fluidRow(
                box(title = HTML("<i class='fas fa-tags'></i> Langkah 2: Penamaan Variabel"), status = "warning", solidHeader = TRUE, width = 12,
                    h4(HTML("<i class='fas fa-pencil-alt'></i> Berikan Nama Deskriptif untuk Setiap Variabel:"), style = "color: #2C3E50; margin-bottom: 15px;"),
                    p("Nama yang jelas akan memudahkan interpretasi hasil analisis.", style = "margin-bottom: 20px;"),
                    uiOutput("variable_labels_ui"),
                    div(style = "background: #f8f9fa; padding: 15px; border-radius: 10px; margin-top: 15px;",
                        h5(HTML("<i class='fas fa-lightbulb'></i> Tips Penamaan Variabel:"), style = "color: #2C3E50; margin-bottom: 10px;"),
                        p("• Gunakan nama yang spesifik: 'Harga Beras per Kg' bukan 'Harga'", style = "margin: 5px 0;"),
                        p("• Sertakan satuan jika diperlukan: 'Suhu Rata-rata (°C)'", style = "margin: 5px 0;"),
                        p("• Hindari singkatan yang tidak jelas", style = "margin: 5px 0;")
                    )
                )
              ),
              
              fluidRow(
                box(title = HTML("<i class='fas fa-upload'></i> Langkah 3: Upload & Pemrosesan Data"), status = "success", solidHeader = TRUE, width = 12,
                    fluidRow(
                      column(8,
                             h4(HTML("<i class='fas fa-file-alt'></i> Format File yang Dibutuhkan:"), style = "color: #2C3E50; margin-bottom: 15px;"),
                             div(style = "background: #e8f5e8; padding: 15px; border-radius: 10px; margin-bottom: 20px;",
                                 h5(HTML("<i class='fas fa-check-circle'></i> Struktur Data:"), style = "color: #27AE60; margin-bottom: 10px;"),
                                 tags$ul(
                                   tags$li(HTML(" Kolom pertama: Variabel target yang ingin diprediksi (Y)")),
                                   tags$li(HTML(" Kolom berikutnya: Faktor-faktor prediktor (X1, X2, X3, ...)")),
                                   tags$li(HTML(" Semua data harus berupa angka (numerik)")),
                                   tags$li(HTML(" Tidak boleh ada sel kosong atau teks")),
                                   tags$li(HTML(" Minimal 20 baris data untuk analisis yang valid"))
                                 )
                             ),
                             fileInput("file_custom", HTML("<i class='fas fa-file-upload'></i> Pilih File Data Anda:"), 
                                       accept = c(".xlsx", ".xls", ".csv", ".sav", ".dta", ".txt", ".tsv"),
                                       buttonLabel = HTML("<i class='fas fa-folder-open'></i> Browse..."),
                                       placeholder = "Belum ada file yang dipilih"),
                             div(style = "background: #e3f2fd; padding: 10px; border-radius: 8px; margin-top: 10px;",
                                 h6(HTML("<i class='fas fa-info-circle'></i> Format File yang Didukung:"), style = "color: #1976D2; margin-bottom: 8px;"),
                                 p(HTML("<i class='fas fa-file-excel'></i> Excel (.xlsx, .xls) | <i class='fas fa-file-csv'></i> CSV (.csv) | <i class='fas fa-chart-bar'></i> SPSS (.sav) | <i class='fas fa-database'></i> Stata (.dta) | <i class='fas fa-file-alt'></i> Text (.txt, .tsv)"), 
                                   style = "margin: 0; color: #1976D2; font-size: 12px;")
                             ),
                             br(),
                             actionButton("process_custom_data", HTML("<i class='fas fa-rocket'></i> Proses & Analisis Data"), 
                                          class = "btn-success btn-lg",
                                          style = "width: 100%; padding: 15px; font-size: 16px;")
                      ),
                      column(4,
                             div(class = "stats-highlight",
                                 h4(HTML("<i class='fas fa-clipboard-check'></i> Checklist"), style = "margin: 0;"),
                                 br(),
                                 p(HTML("<i class='fas fa-check'></i> Data dalam format angka")),
                                 p(HTML("<i class='fas fa-check'></i> Tidak ada sel kosong")),
                                 p(HTML("<i class='fas fa-check'></i> Minimal 20 observasi")),
                                 p(HTML("<i class='fas fa-check'></i> Format file didukung")),
                                 p(HTML("<i class='fas fa-check'></i> Kolom terstruktur rapi"))
                             )
                      )
                    )
                )
              ),
              
              fluidRow(
                box(title = HTML("<i class='fas fa-info-circle'></i> Informasi Variabel Anda"), status = "primary", solidHeader = TRUE, width = 6,
                    div(style = "min-height: 200px;",
                        verbatimTextOutput("variable_info")
                    )
                ),
                box(title = HTML("<i class='fas fa-tasks'></i> Status Pemrosesan"), status = "info", solidHeader = TRUE, width = 6,
                    div(style = "min-height: 200px; padding: 20px; text-align: center;",
                        div(id = "processing-status",
                            h4(HTML("<i class='fas fa-hourglass-start'></i> Menunggu Data"), style = "color: #7F8C8D; margin-bottom: 15px;"),
                            p("Upload file Anda untuk memulai pemrosesan", style = "color: #95A5A6;"),
                            hr(),
                            p(HTML("<i class='fas fa-sync-alt'></i> Sistem akan otomatis:"), style = "color: #34495E; font-weight: 600; margin-bottom: 10px;"),
                            tags$ul(style = "text-align: left; color: #7F8C8D; list-style-type: none; padding-left: 0;",
                                    tags$li(HTML("<i class='fas fa-broom'></i> Membersihkan data missing values")),
                                    tags$li(HTML("<i class='fas fa-search-minus'></i> Mendeteksi dan menghapus outlier")),
                                    tags$li(HTML("<i class='fas fa-check'></i> Memvalidasi format data")),
                                    tags$li(HTML("<i class='fas fa-cogs'></i> Menyiapkan untuk analisis regresi"))
                            )
                        )
                    )
                )
              ),
              
              fluidRow(
                box(title = HTML("<i class='fas fa-table'></i> Data yang Telah Dibersihkan"), status = "success", solidHeader = TRUE, width = 12,
                    div(style = "margin-bottom: 20px;",
                        h4(HTML("<i class='fas fa-table'></i> Data Siap Analisis:"), style = "color: #2C3E50; margin-bottom: 10px;"),
                        p("Semua data Anda yang telah dibersihkan dan siap untuk dianalisis dengan fitur pencarian dan sorting:", style = "color: #34495E;")
                    ),
                    DT::dataTableOutput("customCleanedData")
                )
              )
      ),
      
      # Halaman Statistik Deskriptif
      tabItem(tabName = "desc_stats",
              fluidRow(
                box(title = HTML("<i class='fas fa-clipboard-list'></i> Analisis Statistik Deskriptif"), 
                    status = "primary", solidHeader = TRUE, width = 12,
                    div(style = "text-align: center; padding: 15px;",
                        h3(HTML("<i class='fas fa-poll'></i> Eksplorasi Karakteristik Data"), style = "color: #2C3E50; margin-bottom: 15px;"),
                        p("Memahami distribusi, pemusatan, dan penyebaran data melalui statistik deskriptif komprehensif", 
                          style = "font-size: 16px; color: #34495E;")
                    )
                )
              ),
              
              fluidRow(
                box(title = HTML("<i class='fas fa-database'></i> Pilihan Sumber Data"), status = "info", solidHeader = TRUE, width = 12,
                    fluidRow(
                      column(8,
                             h4(HTML("<i class='fas fa-cogs'></i> Pilih Dataset untuk Analisis:"), style = "color: #2C3E50; margin-bottom: 15px;"),
                             radioButtons("data_choice_stats", "Sumber Data:", 
                                          choiceNames = list(HTML("</i> Data Contoh (Beras & Cuaca)"), HTML("</i> Data Kustom Saya")),
                                          choiceValues = list("example", "custom"),
                                          selected = "example"),
                             p("Pilih sumber data yang ingin Anda analisis karakteristik statistiknya.", style = "color: #7F8C8D;")
                      ),
                      column(4,
                             div(class = "stats-highlight",
                                 h4(HTML("<i class='fas fa-chart-bar'></i> Yang Akan Dianalisis"), style = "margin: 0;"),
                                 br(),
                                 p(HTML("<i class='fas fa-bullseye'></i> Ukuran Pemusatan")),
                                 p(HTML("<i class='fas fa-ruler-horizontal'></i> Ukuran Penyebaran")),
                                 p(HTML("<i class='fas fa-arrows-alt-v'></i> Nilai Ekstrem")),
                                 p(HTML("<i class='fas fa-clipboard-list'></i> Ringkasan Komprehensif"))
                             )
                      )
                    )
                )
              ),
              
              fluidRow(
                box(
                  title = HTML("<i class='fas fa-poll'></i> Hasil Analisis Statistik Deskriptif"), 
                  status = "success", 
                  solidHeader = TRUE, 
                  width = 12,
                  
                  div(style = "margin-bottom: 20px;",
                      h4(
                        HTML("<i class='fas fa-clipboard-list'></i> Interpretasi Statistik:"), 
                        style = "color: #2C3E50; margin-bottom: 10px;"
                      ),
                      p(
                        "Analisis ini memberikan gambaran komprehensif tentang karakteristik data Anda:", 
                        style = "color: #34495E; margin-bottom: 20px;"
                      ),
                      
                      fluidRow(
                        column(3,
                               div(class = "stats-highlight", style = "text-align: center;",
                                   h5(
                                     HTML("<i class='fas fa-bullseye'></i> Pemusatan"), 
                                     style = "margin: 0; font-size: 14px;"
                                   ),
                                   p("Mean, Median", style = "margin: 5px 0 0 0; font-size: 12px;")
                               )
                        ),
                        column(3,
                               div(class = "stats-highlight", style = "text-align: center;",
                                   h5(
                                     HTML("<i class='fas fa-ruler-horizontal'></i> Penyebaran"), 
                                     style = "margin: 0; font-size: 14px;"
                                   ),
                                   p("Min, Max, Range", style = "margin: 5px 0 0 0; font-size: 12px;")
                               )
                        ),
                        column(3,
                               div(class = "stats-highlight", style = "text-align: center;",
                                   h5(
                                     HTML("<i class='fas fa-wave-square'></i> Variabilitas"), 
                                     style = "margin: 0; font-size: 14px;"
                                   ),
                                   p("Standar Deviasi", style = "margin: 5px 0 0 0; font-size: 12px;")
                               )
                        ),
                        column(3,
                               div(class = "stats-highlight", style = "text-align: center;",
                                   h5(
                                     HTML("<i class='fas fa-chart-bar'></i> Distribusi"), 
                                     style = "margin: 0; font-size: 14px;"
                                   ),
                                   p("Skewness, Outlier", style = "margin: 5px 0 0 0; font-size: 12px;")
                               )
                        )
                      )
                  ),
                  
                  verbatimTextOutput("descStats")
                )
              )
              
      ),
      
      # Halaman Visualisasi Data
      tabItem(tabName = "visualization",
              fluidRow(
                box(title = HTML("<i class='fas fa-chart-area'></i> Pusat Visualisasi Data Interaktif"), 
                    status = "primary", solidHeader = TRUE, width = 12,
                    div(style = "text-align: center; padding: 15px;",
                        h3(HTML("<i class='fas fa-poll'></i> Eksplorasi Visual Model Regresi"), style = "color: #2C3E50; margin-bottom: 15px;"),
                        p("Analisis mendalam kualitas model melalui grafik diagnostik dan visualisasi prediksi", 
                          style = "font-size: 16px; color: #34495E;")
                    )
                )
              ),
              
              fluidRow(
                box(title = HTML("<i class='fas fa-database'></i> Pilihan Sumber Data"), status = "info", solidHeader = TRUE, width = 12,
                    fluidRow(
                      column(8,
                             h4(HTML("<i class='fas fa-cogs'></i> Pilih Dataset untuk Visualisasi:"), style = "color: #2C3E50; margin-bottom: 15px;"),
                             radioButtons("data_choice_viz", "Sumber Data:",
                                          choiceNames = list(HTML("</i> Data Contoh (Beras & Cuaca)"), HTML("</i> Data Kustom Saya")),
                                          choiceValues = list("example", "custom"),
                                          selected = "example"),
                             p("Grafik akan menampilkan analisis diagnostik model regresi dari dataset yang dipilih.", style = "color: #7F8C8D;")
                      ),
                      column(4,
                             div(class = "stats-highlight",
                                 h4(HTML("<i class='fas fa-image'></i> Jenis Visualisasi"), style = "margin: 0;"),
                                 br(),
                                 p(HTML("<i class='fas fa-chart-line'></i> Residual vs Fitted")),
                                 p(HTML("<i class='fas fa-chart-bar'></i> Distribusi Residual")),
                                 p(HTML("<i class='fas fa-bullseye'></i> Actual vs Predicted")),
                                 p(HTML("<i class='fas fa-check-double'></i> Validasi Model"))
                             )
                      )
                    )
                )
              ),
              
              fluidRow(
                box(title = HTML("<i class='fas fa-chart-line'></i> Grafik Residual vs Prediksi"), status = "primary", solidHeader = TRUE, width = 12,
                    div(style = "margin-bottom: 20px;",
                        h4(HTML("<i class='fas fa-search'></i> Analisis Pola Residual:"), style = "color: #2C3E50; margin-bottom: 10px;"),
                        p("Grafik ini menunjukkan hubungan antara nilai prediksi dengan residual (kesalahan prediksi). Pola acak menandakan model yang baik.", style = "color: #34495E; margin-bottom: 15px;"),
                        div(style = "background: #e3f2fd; padding: 10px; border-radius: 8px;",
                            p(HTML("<i class='fas fa-lightbulb'></i> Interpretasi: Titik tersebar acak = Model valid | Pola tertentu = Perlu perbaikan model"), style = "margin: 0; color: #1976D2; font-weight: 500;")
                        )
                    ),
                    plotOutput("residualsPlot", height = 400)
                )
              ),
              
              fluidRow(
                box(title = HTML("<i class='fas fa-chart-bar'></i> Distribusi Kesalahan Prediksi"), status = "warning", solidHeader = TRUE, width = 12,
                    div(style = "margin-bottom: 20px;",
                        h4(HTML("<i class='fas fa-bell'></i> Histogram Residual:"), style = "color: #2C3E50; margin-bottom: 10px;"),
                        p("Distribusi kesalahan prediksi menunjukkan normalitas residual. Bentuk lonceng (normal) menandakan asumsi model terpenuhi.", style = "color: #34495E; margin-bottom: 15px;"),
                        div(style = "background: #fff3e0; padding: 10px; border-radius: 8px;",
                            p(HTML("<i class='fas fa-lightbulb'></i> Interpretasi: Bentuk lonceng = Residual normal | Skewed/bimodal = Perlu transformasi data"), style = "margin: 0; color: #F57C00; font-weight: 500;")
                        )
                    ),
                    plotOutput("histResiduals", height = 400)
                )
              ),
              
              fluidRow(
                box(title = HTML("<i class='fas fa-bullseye'></i> Perbandingan Nilai Asli vs Prediksi"), status = "success", solidHeader = TRUE, width = 12,
                    div(style = "margin-bottom: 20px;",
                        h4(HTML("<i class='fas fa-check-double'></i> Akurasi Prediksi Model:"), style = "color: #2C3E50; margin-bottom: 10px;"),
                        p("Scatter plot yang membandingkan nilai aktual dengan prediksi model. Kedekatan dengan garis diagonal menunjukkan akurasi tinggi.", style = "color: #34495E; margin-bottom: 15px;"),
                        div(style = "background: #e8f5e8; padding: 10px; border-radius: 8px;",
                            p(HTML("<i class='fas fa-lightbulb'></i> Interpretasi: Dekat garis merah = Prediksi akurat | Tersebar jauh = Prediksi kurang akurat"), style = "margin: 0; color: #388E3C; font-weight: 500;")
                        )
                    ),
                    plotOutput("predictedVsActual", height = 400)
                )
              )
      ),
      
      # Halaman Model Regresi & Hasil
      tabItem(tabName = "regression",
              fluidRow(
                box(title = HTML("<i class='fas fa-microscope'></i> Laboratorium Analisis Regresi"), 
                    status = "primary", solidHeader = TRUE, width = 12,
                    div(style = "text-align: center; padding: 15px;",
                        h3(HTML("<i class='fas fa-calculator'></i> Hasil Analisis dan Kesimpulan Statistik"), style = "color: #2C3E50; margin-bottom: 15px;"),
                        p("Interpretasi komprehensif model regresi linier berganda dengan validasi statistik mendalam", 
                          style = "font-size: 16px; color: #34495E;")
                    )
                )
              ),
              
              fluidRow(
                box(title = HTML("<i class='fas fa-database'></i> Pilihan Sumber Data"), status = "info", solidHeader = TRUE, width = 12,
                    fluidRow(
                      column(8,
                             h4(HTML("<i class='fas fa-cogs'></i> Pilih Dataset untuk Analisis Regresi:"), style = "color: #2C3E50; margin-bottom: 15px;"),
                             radioButtons("data_choice_reg", "Sumber Data:",
                                          choiceNames = list(HTML("<i class='fas fa-chart-line'></i> Data Contoh (Beras & Cuaca)"), HTML("<i class='fas fa-folder-open'></i> Data Kustom Saya")),
                                          choiceValues = list("example", "custom"),
                                          selected = "example"),
                             p("Sistem akan melakukan analisis regresi linier berganda lengkap dengan interpretasi yang mudah dipahami.", style = "color: #7F8C8D;")
                      ),
                      column(4,
                             div(class = "stats-highlight",
                                 h4(HTML("<i class='fas fa-microscope'></i> Analisis Meliputi"), style = "margin: 0;"),
                                 br(),
                                 p(HTML("<i class='fas fa-cogs'></i> Koefisien Regresi")),
                                 p(HTML("<i class='fas fa-vial'></i> Uji Signifikansi")),
                                 p(HTML("<i class='fas fa-check-circle'></i> R² dan Model Fit")),
                                 p(HTML("<i class='fas fa-shield-alt'></i> Validasi Asumsi"))
                             )
                      )
                    )
                )
              ),
              
              fluidRow(
                box(title = HTML("<i class='fas fa-poll'></i> Detail Teknis Model Regresi"), status = "primary", solidHeader = TRUE, width = 12,
                    div(style = "margin-bottom: 20px;",
                        h4(HTML("<i class='fas fa-microscope'></i> Output Statistik Lengkap:"), style = "color: #2C3E50; margin-bottom: 10px;"),
                        p("Berikut adalah hasil lengkap dari analisis regresi linier berganda termasuk koefisien, standard error, t-value, dan p-value:", style = "color: #34495E; margin-bottom: 15px;"),
                        div(style = "background: #e3f2fd; padding: 10px; border-radius: 8px; margin-bottom: 15px;",
                            fluidRow(
                              column(3,
                                     p(HTML("<i class='fas fa-calculator'></i> Estimate: Koefisien regresi"), 
                                       style = "margin: 0; color: #1976D2; font-size: 13px; text-align: center;")
                              ),
                              column(3,
                                     p(HTML("<i class='fas fa-ruler-vertical'></i> Std. Error: Standar error"), 
                                       style = "margin: 0; color: #1976D2; font-size: 13px; text-align: center;")
                              ),
                              column(3,
                                     p(HTML("<i class='fas fa-vial'></i> t value: Statistik uji"), 
                                       style = "margin: 0; color: #1976D2; font-size: 13px; text-align: center;")
                              ),
                              column(3,
                                     p(HTML("<i class='fas fa-star'></i> Pr(>|t|): Nilai p"), 
                                       style = "margin: 0; color: #1976D2; font-size: 13px; text-align: center;")
                              )
                            )
                            
                        )
                    ),
                    verbatimTextOutput("regressionSummary")
                )
              ),
              
              fluidRow(
                box(title = HTML("<i class='fas fa-brain'></i> Interpretasi dan Kesimpulan Praktis"), status = "success", solidHeader = TRUE, width = 12,
                    div(style = "margin-bottom: 20px;",
                        h4(HTML("<i class='fas fa-list-alt'></i> Analisis Hasil yang Mudah Dipahami:"), style = "color: #2C3E50; margin-bottom: 10px;"),
                        p("Interpretasi komprehensif dari hasil analisis regresi dalam format terstruktur dengan rekomendasi praktis:", style = "color: #34495E; margin-bottom: 15px;"),
                        div(style = "background: #e8f5e8; padding: 10px; border-radius: 8px; margin-bottom: 15px;",
                            fluidRow(
                              column(4,
                                     p(HTML("<i class='fas fa-bullseye'></i> Kualitas Model: Seberapa baik model memprediksi"), style = "margin: 0; color: #388E3C; font-size: 13px;")
                              ),
                              column(4,
                                     p(HTML("<i class='fas fa-chart-bar'></i> Pengaruh Faktor: Mana yang signifikan berpengaruh"), style = "margin: 0; color: #388E3C; font-size: 13px;")
                              ),
                              column(4,
                                     p(HTML("<i class='fas fa-lightbulb'></i> Rekomendasi: Saran untuk langkah selanjutnya"), style = "margin: 0; color: #388E3C; font-size: 13px;")
                              )
                            )
                        )
                    ),
                    htmlOutput("regressionInterpretation")
                )
              )
      )
    )
  )
)

# Server: Bagian Fungsi untuk Menangani Logika Aplikasi
server <- function(input, output, session) {
  
  # Reactive values untuk menyimpan data
  example_data <- reactiveVal(NULL)
  custom_data <- reactiveVal(NULL)
  custom_labels <- reactiveVal(NULL)
  custom_num_vars <- reactiveVal(2)
  selected_y_var <- reactiveVal("Beras_Y")
  
  # Memuat dan memproses data contoh
  observe({
    tryCatch({
      df <- read_excel("Data.xlsx")
      example_data(process_data(df))
    }, error = function(e) {
      # Buat data contoh dummy jika file tidak tersedia
      dummy_data <- data.frame(
        Beras_Y = c(12000, 13000, 11500, 14000, 12500, 13500, 11000, 15000, 12800, 13200,
                    11800, 12200, 13800, 14500, 11200, 12900, 13100, 14200, 11900, 13400),
        Beras_Y1 = c(10000, 11000, 9500, 12000, 10500, 11500, 9000, 13000, 10800, 11200,
                     9800, 10200, 11800, 12500, 9200, 10900, 11100, 12200, 9900, 11400),
        Beras_Y2 = c(10500, 11500, 10000, 12500, 11000, 12000, 9500, 13500, 11300, 11700,
                     10300, 10700, 12300, 13000, 9700, 11400, 11600, 12700, 10400, 11900),
        Beras_Y3 = c(13000, 14000, 12500, 15000, 13500, 14500, 12000, 16000, 13800, 14200,
                     12800, 13200, 14800, 15500, 12200, 13900, 14100, 15200, 12900, 14400),
        Beras_Y4 = c(13500, 14500, 13000, 15500, 14000, 15000, 12500, 16500, 14300, 14700,
                     13300, 13700, 15300, 16000, 12700, 14400, 14600, 15700, 13400, 14900),
        Beras_Y5 = c(15000, 16000, 14500, 17000, 15500, 16500, 14000, 18000, 15800, 16200,
                     14800, 15200, 16800, 17500, 14200, 15900, 16100, 17200, 14900, 16400),
        Beras_Y6 = c(16000, 17000, 15500, 18000, 16500, 17500, 15000, 19000, 16800, 17200,
                     15800, 16200, 17800, 18500, 15200, 16900, 17100, 18200, 15900, 17400),
        Tavg_X1 = c(28, 29, 27, 30, 28.5, 29.5, 26, 31, 28.8, 29.2,
                    27.5, 28.2, 30.1, 31.2, 26.8, 29.8, 28.9, 30.5, 27.8, 29.9),
        RR_X2 = c(150, 120, 180, 100, 160, 110, 200, 90, 140, 130,
                  170, 125, 95, 85, 190, 105, 135, 115, 175, 145)
      )
      example_data(dummy_data)
    })
  })
  
  # UI untuk memilih variabel Y
  output$y_variable_selector <- renderUI({
    data <- example_data()
    if (is.null(data)) return(NULL)
    
    # Identifikasi variabel Y yang tersedia
    y_vars <- colnames(data)[grepl("^Beras_Y", colnames(data))]
    
    # Buat pilihan dengan label yang lebih deskriptif
    choices <- list()
    for (var in y_vars) {
      if (var == "Beras_Y") {
        choices[["Beras Umum (Y)"]] <- var
      } else if (var == "Beras_Y1") {
        choices[["Beras Kualitas Bawah I (Y1)"]] <- var
      } else if (var == "Beras_Y2") {
        choices[["Beras Kualitas Bawah II (Y2)"]] <- var
      } else if (var == "Beras_Y3") {
        choices[["Beras Kualitas Medium I (Y3)"]] <- var
      } else if (var == "Beras_Y4") {
        choices[["Beras Kualitas Medium II (Y4)"]] <- var
      } else if (var == "Beras_Y5") {
        choices[["Beras Kualitas Super I (Y5)"]] <- var
      } else if (var == "Beras_Y6") {
        choices[["Beras Kualitas Super II (Y6)"]] <- var
      } else {
        choices[[var]] <- var
      }
    }
    
    selectInput("selected_y_variable", 
                "Pilih Jenis Beras:", 
                choices = choices,
                selected = selected_y_var(),
                width = "70%")  # atau "300px"
    
  })
  
  # Update selected Y variable
  observeEvent(input$selected_y_variable, {
    if (!is.null(input$selected_y_variable)) {
      selected_y_var(input$selected_y_variable)
    }
  })
  
  # Handler untuk mengunduh guidebook
  output$downloadGuidebook <- downloadHandler(
    filename = function() {
      paste("Guidebook_Dashboard_Analisis_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      guidebook_path <- "guidebook.pdf"
      
      if (file.exists(guidebook_path)) {
        file.copy(guidebook_path, file)
      } else {
        tryCatch({
          pdf(file, width = 8.5, height = 11)
          plot.new()
          text(0.5, 0.9, "GUIDEBOOK DASHBOARD ANALISIS", cex = 2, font = 2)
          text(0.5, 0.8, "Panduan Penggunaan Dashboard", cex = 1.5)
          text(0.1, 0.7, "1. Pilih jenis beras untuk data contoh", adj = 0)
          text(0.1, 0.65, "2. Atau pilih jumlah faktor untuk data custom", adj = 0)
          text(0.1, 0.6, "3. Beri nama untuk setiap variabel", adj = 0)
          text(0.1, 0.55, "4. Upload file Excel dengan format yang benar", adj = 0)
          text(0.1, 0.5, "5. Lihat hasil analisis di tab yang tersedia", adj = 0)
          text(0.1, 0.4, "Format Data Excel:", font = 2, adj = 0)
          text(0.1, 0.35, "- Kolom 1: Variabel target (Y)", adj = 0)
          text(0.1, 0.3, "- Kolom 2-n: Faktor-faktor (X1, X2, ...)", adj = 0)
          text(0.1, 0.25, "- Semua data harus berupa angka", adj = 0)
          text(0.1, 0.2, "- Tidak boleh ada sel kosong", adj = 0)
          dev.off()
        }, error = function(e) {
          writeLines(c(
            "GUIDEBOOK DASHBOARD ANALISIS",
            "",
            "Panduan Penggunaan:",
            "1. Pilih jenis beras untuk data contoh",
            "2. Atau pilih jumlah faktor untuk data custom",
            "3. Beri nama untuk setiap variabel", 
            "4. Upload file Excel dengan format yang benar",
            "5. Lihat hasil analisis di tab yang tersedia",
            "",
            "Format Data Excel:",
            "- Kolom 1: Variabel target (Y)",
            "- Kolom 2-n: Faktor-faktor (X1, X2, ...)",
            "- Semua data harus berupa angka",
            "- Tidak boleh ada sel kosong"
          ), file)
        })
      }
    },
    contentType = "application/pdf"
  )
  
  # UI dinamis untuk label variabel
  output$variable_labels_ui <- renderUI({
    num_vars <- input$num_vars
    if (is.null(num_vars) || num_vars < 1) return(NULL)
    
    label_inputs <- list()
    
    label_inputs[[1]] <- textInput("label_Y", "Nama untuk Variabel yang Ingin Diprediksi (Y):", 
                                   value = "Variabel Target", placeholder = "Contoh: Harga Beras, Penjualan, dll.")
    
    for (i in 1:num_vars) {
      label_inputs[[i+1]] <- textInput(paste0("label_X", i), 
                                       paste("Nama untuk Faktor", i, ":"), 
                                       value = paste("Faktor", i), 
                                       placeholder = "Contoh: Suhu, Curah Hujan, dll.")
    }
    
    do.call(tagList, label_inputs)
  })
  
  # Menangani unggahan file custom dengan multiple format support
  observeEvent(input$process_custom_data, {
    req(input$file_custom, input$num_vars)
    file_path <- input$file_custom$datapath
    file_name <- input$file_custom$name
    file_ext <- tools::file_ext(file_name)
    file_ext_with_dot <- paste0(".", file_ext)
    num_vars <- input$num_vars
    
    labels <- list()
    labels[["Y"]] <- ifelse(is.null(input$label_Y) || input$label_Y == "", "Variabel Target", input$label_Y)
    
    for (i in 1:num_vars) {
      label_input <- paste0("label_X", i)
      labels[[paste0("X", i)]] <- ifelse(is.null(input[[label_input]]) || input[[label_input]] == "", 
                                         paste0("Faktor", i), input[[label_input]])
    }
    
    tryCatch({
      # Read data using the enhanced function that supports multiple formats
      data <- read_data_file(file_path, file_ext_with_dot)
      processed_data <- process_user_custom_data(data, num_vars, labels)
      custom_data(processed_data)
      custom_labels(labels)
      custom_num_vars(num_vars)
      
      showModal(modalDialog(
        title = HTML("<i class='fas fa-check-circle'></i> Berhasil!"),
        HTML(paste("<i class='fas fa-thumbs-up'></i> Data Anda dengan", num_vars, "faktor telah berhasil diproses dan siap dianalisis!<br>",
                   "<i class='fas fa-file'></i> Format file:", toupper(file_ext), "<br>",
                   "<i class='fas fa-database'></i> Jumlah observasi:", nrow(processed_data))),
        easyClose = TRUE,
        footer = NULL
      ))
    }, error = function(e) {
      showModal(modalDialog(
        title = HTML("<i class='fas fa-exclamation-triangle'></i> Ada Masalah"),
        HTML(paste("<i class='fas fa-times-circle'></i> Gagal memproses data. Pastikan:", 
                   "<br>• <i class='fas fa-file-check'></i> File memiliki format yang benar", 
                   "<br>• <i class='fas fa-calculator'></i> Semua data berupa angka", 
                   "<br>• <i class='fas fa-ban'></i> Tidak ada sel yang kosong", 
                   "<br>• <i class='fas fa-list'></i> Urutan kolom: Y, X1, X2, ...",
                   "<br><br><i class='fas fa-bug'></i> Detail error:", e$message)),
        easyClose = TRUE,
        footer = NULL
      ))
    })
  })
  
  output$variable_info <- renderText({
    if (!is.null(custom_labels())) {
      labels <- custom_labels()
      info_text <- "Variabel yang Akan Dianalisis:\n\n"
      info_text <- paste0(info_text, "Target Prediksi: ", labels[["Y"]], "\n")
      info_text <- paste0(info_text, "\nFaktor-faktor yang Mempengaruhi:\n")
      
      x_vars <- names(labels)[names(labels) != "Y"]
      for (i in 1:length(x_vars)) {
        var_name <- x_vars[i]
        info_text <- paste0(info_text, "   ", i, ". ", labels[[var_name]], "\n")
      }
      return(info_text)
    } else {
      return("Belum ada data yang diproses. Silakan unggah file Anda terlebih dahulu.")
    }
  })
  
  # Function to get example data with selected Y variable
  get_example_data_with_selected_y <- reactive({
    data <- example_data()
    y_var <- selected_y_var()
    
    if (is.null(data) || is.null(y_var)) return(NULL)
    
    if (y_var %in% colnames(data)) {
      # Create new dataset with selected Y variable renamed to a standard name
      selected_data <- data %>%
        select(all_of(c(y_var, "Tavg_X1", "RR_X2"))) %>%
        rename(Selected_Y = !!y_var)
      
      return(selected_data)
    }
    
    return(NULL)
  })
  
  output$exampleCleanedData <- DT::renderDataTable({
    data <- get_example_data_with_selected_y()
    if (is.null(data)) return(NULL)
    
    # Rename columns for display
    display_data <- data
    y_var_name <- selected_y_var()
    
    # Get descriptive name for Y variable
    y_display_name <- switch(y_var_name,
                             "Beras_Y" = "Beras Umum",
                             "Beras_Y1" = "Beras Kualitas Bawah I",
                             "Beras_Y2" = "Beras Kualitas Bawah II", 
                             "Beras_Y3" = "Beras Kualitas Medium I",
                             "Beras_Y4" = "Beras Kualitas Medium II",
                             "Beras_Y5" = "Beras Kualitas Super I",
                             "Beras_Y6" = "Beras Kualitas Super II",
                             y_var_name)
    
    colnames(display_data) <- c(y_display_name, "Suhu Rata-rata (°C)", "Curah Hujan (mm)")
    
    DT::datatable(display_data, 
                  extensions = 'Buttons',
                  options = list(
                    pageLength = -1,
                    lengthMenu = list(c(10, 25, 50, 100, -1), c('10', '25', '50', '100', 'Semua')),
                    searchHighlight = TRUE,
                    dom = 'Blfrtip',
                    buttons = c('copy', 'csv', 'excel'),
                    language = list(
                      search = HTML("<i class='fa fa-search'></i> Cari:"),
                      lengthMenu = "Tampilkan _MENU_ data per halaman",
                      info = HTML("<i class='fa fa-info-circle'></i> Menampilkan _START_ sampai _END_ dari _TOTAL_ data"),
                      paginate = list(
                        previous = HTML("<i class='fa fa-arrow-left'></i> Sebelumnya"),
                        'next' = HTML("Selanjutnya <i class='fa fa-arrow-right'></i>")
                      )
                    )
                  ),
                  rownames = FALSE,
                  class = 'cell-border stripe')
  })
  
  output$customCleanedData <- DT::renderDataTable({
    req(custom_data())
    
    DT::datatable(custom_data(), 
                  extensions = 'Buttons',
                  options = list(
                    pageLength = -1,
                    lengthMenu = list(c(10, 25, 50, 100, -1), c('10', '25', '50', '100', 'Semua')),
                    searchHighlight = TRUE,
                    dom = 'Blfrtip',
                    buttons = c('copy', 'csv', 'excel'),
                    language = list(
                      search = HTML("<i class='fa fa-search'></i> Cari:"),
                      lengthMenu = "Tampilkan _MENU_ data per halaman",
                      info = HTML("<i class='fa fa-info-circle'></i> Menampilkan _START_ sampai _END_ dari _TOTAL_ data"),
                      paginate = list(previous = HTML("<i class='fa fa-arrow-left'></i> Sebelumnya"), 'next' = HTML("Selanjutnya <i class='fa fa-arrow-right'></i>"))
                    )
                  ),
                  rownames = FALSE,
                  class = 'cell-border stripe')
  })
  
  selected_data_stats <- reactive({
    if (input$data_choice_stats == "example") {
      return(get_example_data_with_selected_y())
    } else if (input$data_choice_stats == "custom") {
      req(custom_data())
      return(custom_data())
    }
    return(NULL)
  })
  
  selected_data_viz <- reactive({
    if (input$data_choice_viz == "example") {
      return(get_example_data_with_selected_y())
    } else if (input$data_choice_viz == "custom") {
      req(custom_data())
      return(custom_data())
    }
    return(NULL)
  })
  
  selected_data_reg <- reactive({
    if (input$data_choice_reg == "example") {
      return(get_example_data_with_selected_y())
    } else if (input$data_choice_reg == "custom") {
      req(custom_data())
      return(custom_data())
    }
    return(NULL)
  })
  
  output$descStats <- renderPrint({
    data <- selected_data_stats()
    if (is.null(data) || nrow(data) == 0) {
      cat("Silakan pilih data yang tersedia atau unggah data custom Anda terlebih dahulu.")
      return()
    }
    
    cat("RINGKASAN STATISTIK DATA\n")
    cat(paste(rep("=", 40), collapse = ""), "\n\n")
    
    if (input$data_choice_stats == "example") {
      y_var_name <- selected_y_var()
      y_display_name <- switch(y_var_name,
                               "Beras_Y" = "Beras Umum",
                               "Beras_Y1" = "Beras Kualitas Bawah I",
                               "Beras_Y2" = "Beras Kualitas Bawah II",
                               "Beras_Y3" = "Beras Kualitas Medium I",
                               "Beras_Y4" = "Beras Kualitas Medium II",
                               "Beras_Y5" = "Beras Kualitas Super I",
                               "Beras_Y6" = "Beras Kualitas Super II",
                               y_var_name)
      
      cat("Variabel Target:", y_display_name, "\n")
      cat("   Min: Rp", format(min(data$Selected_Y, na.rm = TRUE), big.mark = ","), "\n")
      cat("   Max: Rp", format(max(data$Selected_Y, na.rm = TRUE), big.mark = ","), "\n")
      cat("   Rata-rata: Rp", format(round(mean(data$Selected_Y, na.rm = TRUE), 0), big.mark = ","), "\n")
      cat("   Median: Rp", format(round(median(data$Selected_Y, na.rm = TRUE), 0), big.mark = ","), "\n")
      cat("   Standar Deviasi: Rp", format(round(sd(data$Selected_Y, na.rm = TRUE), 0), big.mark = ","), "\n")
      cat("   Skewness:", round(skewness(data$Selected_Y, na.rm = TRUE), 2), "\n\n")
      
      cat("Suhu Rata-rata:\n")
      cat("   Min:", min(data$Tavg_X1, na.rm = TRUE), "°C\n")
      cat("   Max:", max(data$Tavg_X1, na.rm = TRUE), "°C\n")
      cat("   Rata-rata:", round(mean(data$Tavg_X1, na.rm = TRUE), 2), "°C\n")
      cat("   Median:", round(median(data$Tavg_X1, na.rm = TRUE), 2), "°C\n")
      cat("   Standar Deviasi:", round(sd(data$Tavg_X1, na.rm = TRUE), 2), "°C\n")
      cat("   Skewness:", round(skewness(data$Tavg_X1, na.rm = TRUE), 2), "\n\n")
      
      cat("Curah Hujan:\n")
      cat("   Min:", min(data$RR_X2, na.rm = TRUE), "mm\n")
      cat("   Max:", max(data$RR_X2, na.rm = TRUE), "mm\n")
      cat("   Rata-rata:", round(mean(data$RR_X2, na.rm = TRUE), 2), "mm\n")
      cat("   Median:", round(median(data$RR_X2, na.rm = TRUE), 2), "mm\n")
      cat("   Standar Deviasi:", round(sd(data$RR_X2, na.rm = TRUE), 2), "mm\n")
      cat("   Skewness:", round(skewness(data$RR_X2, na.rm = TRUE), 2), "\n\n")
      
    } else if ("Y" %in% colnames(data) && !is.null(custom_labels())) {
      labels <- custom_labels()
      
      # Process Y variable
      cat("Variabel Target:", labels[["Y"]], "\n")
      cat("   Min:", min(data$Y, na.rm = TRUE), "\n")
      cat("   Max:", max(data$Y, na.rm = TRUE), "\n")
      cat("   Rata-rata:", round(mean(data$Y, na.rm = TRUE), 2), "\n")
      cat("   Median:", round(median(data$Y, na.rm = TRUE), 2), "\n")
      cat("   Standar Deviasi:", round(sd(data$Y, na.rm = TRUE), 2), "\n")
      cat("   Skewness:", round(skewness(data$Y, na.rm = TRUE), 2), "\n\n")
      
      # Process X variables
      x_vars <- colnames(data)[colnames(data) != "Y"]
      for (var in x_vars) {
        var_label <- labels[[var]]
        cat(var_label, ":\n")
        cat("   Min:", min(data[[var]], na.rm = TRUE), "\n")
        cat("   Max:", max(data[[var]], na.rm = TRUE), "\n")
        cat("   Rata-rata:", round(mean(data[[var]], na.rm = TRUE), 2), "\n")
        cat("   Median:", round(median(data[[var]], na.rm = TRUE), 2), "\n")
        cat("   Standar Deviasi:", round(sd(data[[var]], na.rm = TRUE), 2), "\n")
        cat("   Skewness:", round(skewness(data[[var]], na.rm = TRUE), 2), "\n\n")
      }
    } else {
      # Fallback for generic data frame
      print(summary(data))
      cat("\n")
      for(col in colnames(data)) {
        if(is.numeric(data[[col]])) {
          cat("Statistik untuk", col, ":\n")
          cat("  Standar Deviasi:", round(sd(data[[col]], na.rm=TRUE), 2), "\n")
          cat("  Skewness:", round(skewness(data[[col]], na.rm=TRUE), 2), "\n\n")
        }
      }
    }
    
    cat(paste(rep("-", 40), collapse = ""), "\n")
    cat("Catatan:\n")
    cat("1. Skewness: Ukuran ketidaksimetrisan distribusi data.\n")
    cat("   - Nilai 0: Simetris sempurna.\n")
    cat("   - Nilai > 0: Ekor kanan lebih panjang (positively skewed).\n")
    cat("   - Nilai < 0: Ekor kiri lebih panjang (negatively skewed).\n")
    cat("2. Outlier (pencilan) telah dihapus dari data menggunakan metode IQR.\n")
  })
  
  model_viz <- reactive({
    data <- selected_data_viz()
    if (is.null(data)) return(NULL)
    
    tryCatch({
      if ("Selected_Y" %in% colnames(data)) {
        return(lm(Selected_Y ~ Tavg_X1 + RR_X2, data = data))
      } else if ("Y" %in% colnames(data)) {
        x_vars <- colnames(data)[colnames(data) != "Y"]
        formula_str <- paste("Y ~", paste(x_vars, collapse = " + "))
        return(lm(as.formula(formula_str), data = data))
      }
      return(NULL)
    }, error = function(e) {
      return(NULL)
    })
  })
  
  model_reg <- reactive({
    data <- selected_data_reg()
    if (is.null(data)) return(NULL)
    
    tryCatch({
      if ("Selected_Y" %in% colnames(data)) {
        return(lm(Selected_Y ~ Tavg_X1 + RR_X2, data = data))
      } else if ("Y" %in% colnames(data)) {
        x_vars <- colnames(data)[colnames(data) != "Y"]
        formula_str <- paste("Y ~", paste(x_vars, collapse = " + "))
        return(lm(as.formula(formula_str), data = data))
      }
      return(NULL)
    }, error = function(e) {
      return(NULL)
    })
  })
  
  output$residualsPlot <- renderPlot({
    model <- model_viz()
    if (is.null(model)) {
      plot(1, type="n", xlab="", ylab="", xlim=c(0, 10), ylim=c(0, 10))
      text(5, 5, "Silakan pilih data yang tersedia\natau unggah data custom terlebih dahulu", cex=1.2)
      return()
    }
    
    residuals <- model$residuals
    fitted_values <- model$fitted.values
    ggplot(data.frame(fitted = fitted_values, residuals = residuals), aes(x = fitted, y = residuals)) +
      geom_point(alpha = 0.6, color = "steelblue") +
      geom_hline(yintercept = 0, linetype = "dashed", color = "red", size = 1) +
      labs(title = "Grafik Residual vs Prediksi", 
           x = "Nilai Prediksi", 
           y = "Residual (Selisih)",
           subtitle = "Titik yang tersebar acak menunjukkan model yang baik") +
      theme_minimal()
  })
  
  output$histResiduals <- renderPlot({
    model <- model_viz()
    if (is.null(model)) {
      plot(1, type="n", xlab="", ylab="", xlim=c(0, 10), ylim=c(0, 10))
      text(5, 5, "Silakan pilih data yang tersedia\natau unggah data custom terlebih dahulu", cex=1.2)
      return()
    }
    
    residuals <- model$residuals
    ggplot(data.frame(residuals = residuals), aes(x = residuals)) +
      geom_histogram(fill = "lightblue", color = "darkblue", alpha = 0.7, bins = 30) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "red", size = 1) +
      labs(title = "Distribusi Kesalahan Prediksi", 
           x = "Kesalahan Prediksi", 
           y = "Frekuensi",
           subtitle = "Bentuk seperti lonceng menunjukkan model yang baik") +
      theme_minimal()
  })
  
  output$predictedVsActual <- renderPlot({
    model <- model_viz()
    data <- selected_data_viz()
    if (is.null(model) || is.null(data)) {
      plot(1, type="n", xlab="", ylab="", xlim=c(0, 10), ylim=c(0, 10))
      text(5, 5, "Silakan pilih data yang tersedia\natau unggah data custom terlebih dahulu", cex=1.2)
      return()
    }
    
    fitted_values <- model$fitted.values
    
    if ("Selected_Y" %in% colnames(data)) {
      y_var_name <- selected_y_var()
      y_display_name <- switch(y_var_name,
                               "Beras_Y" = "Beras Umum",
                               "Beras_Y1" = "Beras Kualitas Bawah I",
                               "Beras_Y2" = "Beras Kualitas Bawah II", 
                               "Beras_Y3" = "Beras Kualitas Medium I",
                               "Beras_Y4" = "Beras Kualitas Medium II",
                               "Beras_Y5" = "Beras Kualitas Super I",
                               "Beras_Y6" = "Beras Kualitas Super II",
                               y_var_name)
      
      ggplot(data, aes(x = Selected_Y, y = fitted_values)) +
        geom_point(alpha = 0.6, color = "darkgreen") +
        geom_abline(intercept = 0, slope = 1, color = "red", size = 1) +
        labs(title = "Perbandingan Nilai Asli vs Prediksi", 
             x = paste("Harga Asli", y_display_name, "(Rp)"), 
             y = paste("Prediksi Harga", y_display_name, "(Rp)"),
             subtitle = "Semakin dekat ke garis merah, semakin akurat") +
        theme_minimal()
    } else if ("Y" %in% colnames(data)) {
      y_label <- ifelse(!is.null(custom_labels()), custom_labels()[["Y"]], "Nilai Y")
      ggplot(data, aes(x = Y, y = fitted_values)) +
        geom_point(alpha = 0.6, color = "darkgreen") +
        geom_abline(intercept = 0, slope = 1, color = "red", size = 1) +
        labs(title = "Perbandingan Nilai Asli vs Prediksi", 
             x = paste("Nilai Asli", y_label), 
             y = paste("Prediksi", y_label),
             subtitle = "Semakin dekat ke garis merah, semakin akurat") +
        theme_minimal()
    }
  })
  
  output$regressionSummary <- renderPrint({
    model <- model_reg()
    if (is.null(model)) {
      cat("Silakan pilih data yang tersedia atau unggah data custom Anda terlebih dahulu.")
      return()
    }
    summary(model)
  })
  
  output$regressionInterpretation <- renderText({
    model <- model_reg()
    data <- selected_data_reg()
    
    if (is.null(model) || is.null(data)) {
      return("Silakan pilih data yang tersedia atau unggah data custom Anda terlebih dahulu.")
    }
    
    tryCatch({
      summary_model <- summary(model)
      coefs <- coefficients(model)
      residuals <- model$residuals
      
      # Determine variable names based on data type
      dep_var_name <- "variabel yang ingin diprediksi"
      if (input$data_choice_reg == "example") {
        y_var_name <- selected_y_var()
        dep_var_name <- switch(y_var_name,
                               "Beras_Y" = "harga Beras Umum",
                               "Beras_Y1" = "harga Beras Kualitas Bawah I",
                               "Beras_Y2" = "harga Beras Kualitas Bawah II", 
                               "Beras_Y3" = "harga Beras Kualitas Medium I",
                               "Beras_Y4" = "harga Beras Kualitas Medium II",
                               "Beras_Y5" = "harga Beras Kualitas Super I",
                               "Beras_Y6" = "harga Beras Kualitas Super II",
                               "harga beras")
      } else if (!is.null(custom_labels())) {
        dep_var_name <- custom_labels()[["Y"]]
      }
      
      r_squared <- summary_model$r.squared
      adj_r_squared <- summary_model$adj.r.squared
      
      # Build structured HTML output
      html_output <- paste0(
        '<div class="result-section">',
        '<div class="interpretation-header"><i class="fas fa-info-circle section-icon"></i>INFORMASI DASAR MODEL</div>',
        '<p><i class="fas fa-database"></i> <strong>Jumlah observasi:</strong> ', nrow(data), ' data</p>',
        '<p><i class="fas fa-layer-group"></i> <strong>Jumlah faktor prediksi:</strong> ', length(coefs) - 1, ' faktor</p>',
        '<p><i class="fas fa-bullseye"></i> <strong>Variabel target:</strong> ', dep_var_name, '</p>',
        '</div>'
      )
      
      # Model Quality Assessment
      quality_status <- ""
      quality_color <- ""
      quality_icon <- ""
      
      if (r_squared >= 0.9) {
        quality_status <- "LUAR BIASA"
        quality_color <- "#27AE60"
        quality_icon <- "fas fa-trophy"
      } else if (r_squared >= 0.8) {
        quality_status <- "SANGAT BAIK"
        quality_color <- "#2ECC71"
        quality_icon <- "fas fa-star"
      } else if (r_squared >= 0.6) {
        quality_status <- "CUKUP BAIK"
        quality_color <- "#F39C12"
        quality_icon <- "fas fa-thumbs-up"
      } else if (r_squared >= 0.4) {
        quality_status <- "SEDANG"
        quality_color <- "#E67E22"
        quality_icon <- "fas fa-exclamation-triangle"
      } else {
        quality_status <- "KURANG BAIK"
        quality_color <- "#E74C3C"
        quality_icon <- "fas fa-times-circle"
      }
      
      html_output <- paste0(html_output,
                            '<div class="result-section">',
                            '<div class="interpretation-header"><i class="fas fa-chart-line section-icon"></i>KUALITAS PREDIKSI MODEL</div>',
                            '<div style="background: ', quality_color, '; color: white; padding: 15px; border-radius: 10px; margin: 10px 0;">',
                            '<h4 style="margin: 0;"><i class="', quality_icon, '"></i> ', quality_status, ' (R² = ', round(r_squared, 3), ')</h4>',
                            '</div>',
                            '<p><i class="fas fa-percentage"></i> <strong>Akurasi prediksi:</strong> ', round(r_squared * 100, 1), '% dari perubahan ', dep_var_name, ' dapat dijelaskan oleh model</p>',
                            '<p><i class="fas fa-question-circle"></i> <strong>Faktor lain:</strong> ', round((1-r_squared) * 100, 1), '% dipengaruhi faktor di luar model</p>',
                            '</div>'
      )
      
      # Statistical Significance Test
      f_stat <- summary_model$fstatistic
      if (!is.null(f_stat)) {
        f_p_value <- pf(f_stat[1], f_stat[2], f_stat[3], lower.tail = FALSE)
        sig_status <- ""
        sig_color <- ""
        
        if (f_p_value < 0.001) {
          sig_status <- "SANGAT SIGNIFIKAN (p < 0.001)"
          sig_color <- "#27AE60"
        } else if (f_p_value < 0.01) {
          sig_status <- "SIGNIFIKAN (p < 0.01)"
          sig_color <- "#2ECC71"
        } else if (f_p_value < 0.05) {
          sig_status <- "CUKUP SIGNIFIKAN (p < 0.05)"
          sig_color <- "#F39C12"
        } else {
          sig_status <- "TIDAK SIGNIFIKAN (p ≥ 0.05)"
          sig_color <- "#E74C3C"
        }
        
        html_output <- paste0(html_output,
                              '<div class="result-section">',
                              '<div class="interpretation-header"><i class="fas fa-vial section-icon"></i>UJI KELAYAKAN MODEL</div>',
                              '<div style="background: ', sig_color, '; color: white; padding: 15px; border-radius: 10px; margin: 10px 0;">',
                              '<h4 style="margin: 0;"><i class="fas fa-check-circle"></i> ', sig_status, '</h4>',
                              '</div>',
                              '</div>'
        )
      }
      
      # Individual Factor Analysis
      html_output <- paste0(html_output,
                            '<div class="result-section">',
                            '<div class="interpretation-header"><i class="fas fa-microscope section-icon"></i>ANALISIS SETIAP FAKTOR</div>'
      )
      
      significant_vars <- 0
      
      if (input$data_choice_reg == "example") {
        # Handle example data factors
        if (length(coefs) >= 2) {
          p_value_temp <- summary_model$coefficients[2, 4]
          coef_temp <- coefs[2]
          
          factor_color <- if (p_value_temp < 0.05) "#2ECC71" else "#E74C3C"
          factor_icon <- if (p_value_temp < 0.05) "fas fa-check-circle" else "fas fa-times-circle"
          effect_direction <- if (coef_temp > 0) "POSITIF" else "NEGATIF"
          effect_icon <- if (coef_temp > 0) "fas fa-arrow-up" else "fas fa-arrow-down"
          
          if (p_value_temp < 0.05) significant_vars <- significant_vars + 1
          
          html_output <- paste0(html_output,
                                '<div style="border: 2px solid ', factor_color, '; border-radius: 10px; padding: 15px; margin: 10px 0;">',
                                '<h5><i class="fas fa-thermometer-half"></i> SUHU RATA-RATA</h5>',
                                '<div style="background: ', factor_color, '; color: white; padding: 10px; border-radius: 5px; margin: 5px 0;">',
                                '<span><i class="', factor_icon, '"></i> ', if (p_value_temp < 0.05) "BERPENGARUH SIGNIFIKAN" else "TIDAK BERPENGARUH", '</span>',
                                '</div>',
                                '<p><i class="', effect_icon, '"></i> <strong>Arah pengaruh:</strong> ', effect_direction, '</p>',
                                '<p><i class="fas fa-calculator"></i> <strong>Besaran pengaruh:</strong> Setiap kenaikan 1°C akan ', 
                                if (coef_temp > 0) "meningkatkan" else "menurunkan", ' ', dep_var_name, ' sebesar Rp ', 
                                format(round(abs(coef_temp), 0), big.mark = ","), '</p>',
                                '</div>'
          )
        }
        
        if (length(coefs) >= 3) {
          p_value_rain <- summary_model$coefficients[3, 4]
          coef_rain <- coefs[3]
          
          factor_color <- if (p_value_rain < 0.05) "#2ECC71" else "#E74C3C"
          factor_icon <- if (p_value_rain < 0.05) "fas fa-check-circle" else "fas fa-times-circle"
          effect_direction <- if (coef_rain > 0) "POSITIF" else "NEGATIF"
          effect_icon <- if (coef_rain > 0) "fas fa-arrow-up" else "fas fa-arrow-down"
          
          if (p_value_rain < 0.05) significant_vars <- significant_vars + 1
          
          html_output <- paste0(html_output,
                                '<div style="border: 2px solid ', factor_color, '; border-radius: 10px; padding: 15px; margin: 10px 0;">',
                                '<h5><i class="fas fa-cloud-rain"></i> CURAH HUJAN</h5>',
                                '<div style="background: ', factor_color, '; color: white; padding: 10px; border-radius: 5px; margin: 5px 0;">',
                                '<span><i class="', factor_icon, '"></i> ', if (p_value_rain < 0.05) "BERPENGARUH SIGNIFIKAN" else "TIDAK BERPENGARUH", '</span>',
                                '</div>',
                                '<p><i class="', effect_icon, '"></i> <strong>Arah pengaruh:</strong> ', effect_direction, '</p>',
                                '<p><i class="fas fa-calculator"></i> <strong>Besaran pengaruh:</strong> Setiap kenaikan 1mm akan ', 
                                if (coef_rain > 0) "meningkatkan" else "menurunkan", ' ', dep_var_name, ' sebesar Rp ', 
                                format(round(abs(coef_rain), 2), big.mark = ","), '</p>',
                                '</div>'
          )
        }
      } else if (!is.null(custom_labels())) {
        # Handle custom data factors
        labels <- custom_labels()
        x_vars <- names(coefs)[-1]
        
        for (i in 1:length(x_vars)) {
          var_name <- x_vars[i]
          coef_value <- coefs[i+1]
          p_value <- summary_model$coefficients[i+1, 4]
          
          var_label <- if (var_name %in% names(labels)) labels[[var_name]] else var_name
          
          factor_color <- if (p_value < 0.05) "#2ECC71" else "#E74C3C"
          factor_icon <- if (p_value < 0.05) "fas fa-check-circle" else "fas fa-times-circle"
          effect_direction <- if (coef_value > 0) "POSITIF" else "NEGATIF"
          effect_icon <- if (coef_value > 0) "fas fa-arrow-up" else "fas fa-arrow-down"
          
          if (p_value < 0.05) significant_vars <- significant_vars + 1
          
          html_output <- paste0(html_output,
                                '<div style="border: 2px solid ', factor_color, '; border-radius: 10px; padding: 15px; margin: 10px 0;">',
                                '<h5><i class="fas fa-cog"></i> ', toupper(var_label), '</h5>',
                                '<div style="background: ', factor_color, '; color: white; padding: 10px; border-radius: 5px; margin: 5px 0;">',
                                '<span><i class="', factor_icon, '"></i> ', if (p_value < 0.05) "BERPENGARUH SIGNIFIKAN" else "TIDAK BERPENGARUH", '</span>',
                                '</div>',
                                '<p><i class="', effect_icon, '"></i> <strong>Arah pengaruh:</strong> ', effect_direction, '</p>',
                                '<p><i class="fas fa-calculator"></i> <strong>Besaran pengaruh:</strong> Setiap kenaikan 1 unit pada ', var_label, ' akan ', 
                                if (coef_value > 0) "meningkatkan" else "menurunkan", ' ', labels[["Y"]], ' sebesar ', 
                                round(abs(coef_value), 4), '</p>',
                                '</div>'
          )
        }
      }
      
      html_output <- paste0(html_output, '</div>')
      
      # Model Validation
      validation_html <- '<div class="result-section"><div class="interpretation-header"><i class="fas fa-shield-alt section-icon"></i>VALIDASI MODEL</div>'
      
      # Normality test
      if (length(residuals) >= 3 && length(residuals) <= 5000) {
        shapiro_test <- shapiro.test(residuals)
        norm_color <- if (shapiro_test$p.value > 0.05) "#2ECC71" else "#E67E22"
        norm_icon <- if (shapiro_test$p.value > 0.05) "fas fa-check-circle" else "fas fa-exclamation-triangle"
        norm_status <- if (shapiro_test$p.value > 0.05) "NORMAL" else "TIDAK NORMAL"
        
        validation_html <- paste0(validation_html,
                                  '<p><i class="', norm_icon, '" style="color: ', norm_color, ';"></i> <strong>Distribusi kesalahan:</strong> ', norm_status, '</p>'
        )
      }
      
      # Homoscedasticity test
      tryCatch({
        bp_test <- bptest(model)
        homo_color <- if (bp_test$p.value > 0.05) "#2ECC71" else "#E67E22"
        homo_icon <- if (bp_test$p.value > 0.05) "fas fa-check-circle" else "fas fa-exclamation-triangle"
        homo_status <- if (bp_test$p.value > 0.05) "KONSISTEN" else "TIDAK KONSISTEN"
        
        validation_html <- paste0(validation_html,
                                  '<p><i class="', homo_icon, '" style="color: ', homo_color, ';"></i> <strong>Konsistensi kesalahan:</strong> ', homo_status, '</p>'
        )
      }, error = function(e) {})
      
      # Multicollinearity test
      if (length(coefs) > 2) {
        tryCatch({
          vif_result <- vif(model)
          multi_color <- if (all(vif_result < 5)) "#2ECC71" else if (all(vif_result < 10)) "#E67E22" else "#E74C3C"
          multi_icon <- if (all(vif_result < 5)) "fas fa-check-circle" else "fas fa-exclamation-triangle"
          multi_status <- if (all(vif_result < 5)) "INDEPENDEN" else if (all(vif_result < 10)) "SEDIKIT TERKAIT" else "SANGAT TERKAIT"
          
          validation_html <- paste0(validation_html,
                                    '<p><i class="', multi_icon, '" style="color: ', multi_color, ';"></i> <strong>Keterkaitan antar faktor:</strong> ', multi_status, '</p>'
          )
        }, error = function(e) {})
      }
      
      validation_html <- paste0(validation_html, '</div>')
      html_output <- paste0(html_output, validation_html)
      
      # Final Recommendations
      rec_html <- '<div class="result-section"><div class="interpretation-header"><i class="fas fa-lightbulb section-icon"></i>KESIMPULAN DAN REKOMENDASI</div>'
      
      if (significant_vars > 0 && r_squared >= 0.7) {
        rec_html <- paste0(rec_html,
                           '<div style="background: #27AE60; color: white; padding: 15px; border-radius: 10px; margin: 10px 0;">',
                           '<h4 style="margin: 0;"><i class="fas fa-trophy"></i> MODEL SANGAT BAIK & DAPAT DIANDALKAN</h4>',
                           '</div>',
                           '<p><i class="fas fa-check"></i> ', significant_vars, ' faktor berpengaruh signifikan</p>',
                           '<p><i class="fas fa-chart-line"></i> Akurasi prediksi tinggi (', round(r_squared * 100, 1), '%)</p>',
                           '<p><i class="fas fa-bullhorn"></i> <strong>Rekomendasi:</strong> Gunakan model untuk prediksi dan pengambilan keputusan</p>'
        )
      } else if (significant_vars > 0 && r_squared >= 0.5) {
        rec_html <- paste0(rec_html,
                           '<div style="background: #F39C12; color: white; padding: 15px; border-radius: 10px; margin: 10px 0;">',
                           '<h4 style="margin: 0;"><i class="fas fa-thumbs-up"></i> MODEL CUKUP BAIK</h4>',
                           '</div>',
                           '<p><i class="fas fa-check"></i> ', significant_vars, ' faktor berpengaruh signifikan</p>',
                           '<p><i class="fas fa-chart-line"></i> Akurasi prediksi sedang (', round(r_squared * 100, 1), '%)</p>',
                           '<p><i class="fas fa-bullhorn"></i> <strong>Rekomendasi:</strong> Tambahkan faktor lain untuk meningkatkan akurasi</p>'
        )
      } else {
        rec_html <- paste0(rec_html,
                           '<div style="background: #E74C3C; color: white; padding: 15px; border-radius: 10px; margin: 10px 0;">',
                           '<h4 style="margin: 0;"><i class="fas fa-exclamation-triangle"></i> MODEL PERLU DIPERBAIKI</h4>',
                           '</div>',
                           '<p><i class="fas fa-times"></i> Akurasi prediksi rendah (', round(r_squared * 100, 1), '%)</p>',
                           '<p><i class="fas fa-bullhorn"></i> <strong>Rekomendasi:</strong> Cari faktor yang lebih berpengaruh atau gunakan metode lain</p>'
        )
      }
      
      # Practical suggestions
      rec_html <- paste0(rec_html,
                         '<h5><i class="fas fa-list-ul"></i><strong> Saran Praktis:</strong></h5>',
                         '<ul>',
                         '<li> Kumpulkan lebih banyak data jika memungkinkan</li>',
                         '<li> Validasi model dengan data baru</li>'
      )
      
      if (r_squared < 0.5) {
        rec_html <- paste0(rec_html,
                           '<li>Eksplorasi faktor tambahan yang mungkin berpengaruh</li>',
                           '<li>Pertimbangkan transformasi data atau metode lain</li>'
        )
      }
      
      rec_html <- paste0(rec_html, '</ul></div>')
      html_output <- paste0(html_output, rec_html)
      
      return(html_output) 
      
    }, error = function(e) {
      return(paste('<div style="color: red;"><i class="fas fa-exclamation-triangle"></i> Terjadi kesalahan dalam analisis:', e$message, '</div>'))
    })
  })
}

# Menjalankan Aplikasi Shiny
shinyApp(ui = ui, server = server)