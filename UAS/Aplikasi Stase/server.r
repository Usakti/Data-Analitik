#Membuat fungsi Server dengan parameter input dan ouput
server <- function(input, output) {
  
  #Fungsi: kirim_data, untuk memproses data yang diupload, kemudian diperoleh data, dan dapat digunakan untuk berbagai keperluan
  kirim_data <- reactive({
    ambil_file_data <- input$ambil_file_data
    if(is.null(ambil_file_data))
      return(NULL)
    pemisah_variabel = input$pemisah_variabel
    
    #Memproses Jika Data yang Diinput .xlsx
    if(pemisah_variabel == 'xlsx') {
      inFile <- input$ambil_file_data
      if(is.null(inFile))
        return(NULL)
      file.rename(inFile$datapath,
                  paste(inFile$datapath, ".xlsx", sep=""))
      data <- read_excel(paste(inFile$datapath, ".xlsx", sep=""), 1)
      return(data)
    }
    
    #Memproses Jika Data yang Diupload .txt atau .csv
    if(pemisah_variabel != 'xlsx') {
      data <- read.csv(ambil_file_data$datapath, sep = input$pemisah_variabel)
      return(data)
    }
  })
  
  #Mencetak Data yang Telah Diupload
  output$dataku <- DT::renderDT({
    kirim_data()
  })
  
  #Fungsi Kirim Nama
  kirim_nama <- function(){
    dataku <- kirim_data()
    nama <- colnames(dataku)
    return(nama)
  }
  
  #Grafik Batang dan Statistik
  output$pemilihan_variabel_selectizeinput_single_input_bar_chart_1 <- renderUI({
    if(is.null(kirim_nama()))
      return(NULL)
    if(is.null(length( kirim_nama() ) == 0 ))
      return(NULL)
    selectizeInput('terpilih_selectizeinput_single_input_bar_chart_1', 'Pilih Variabel Kategori:', 
                   choices = c( kirim_nama()), selected=c(), 
                   multiple = FALSE)
  })
  
  #fungsi statistik untuk menghitung nilai rata-rata, nilai minimum, nilai maximum, serta nilai standar deviasi
  statistika <- function(){
    dataku <- kirim_data()
    x = dataku[,c(input$terpilih_selectizeinput_single_input_bar_chart_1)]
    df = data.frame(mean(x), min(x), max(x), sd(x))
    colnames(df) = c("Rata-rata","Minimum","Maximum", "Standar Deviasi")
    return(df)
  }
  
  #fungsi untuk menampilkan grafik berdasarkan frekuensi data
  grafik_batang_1 <- function(){
    dataku <- kirim_data()
    x = dataku[,c(input$terpilih_selectizeinput_single_input_bar_chart_1)]
    hitung_frekuensi_x = table(x)
    nama <- names(hitung_frekuensi_x)
    names(hitung_frekuensi_x) = NULL
    hitung_frekuensi_x = unlist(hitung_frekuensi_x)
    hitung_frekuensi_x = as.numeric(hitung_frekuensi_x)
    data_tabel = data.frame(nama, hitung_frekuensi_x)
    colnames(data_tabel) = c("name","freq")
    gambar <- ggplot(data = data_tabel, aes(x = name, y = freq, fill = name)) +
      geom_bar(stat="identity")
    gambar <- gambar + geom_text(
      aes(label = paste0(freq) ),
      size = 5)
    return(gambar)
  } 
  
  #output data yang ditampilkan berupa grafik
  output$bar_chart_1_500_500 <- renderPlot({
    grafik_batang_1()
  })
  
  #output data yang ditampilkan berupa hasil statistik (mean, dll) dalam bentuk tabel
  output$hasil_statistika <- DT::renderDT({
    DT::datatable(statistika())
  })
  
} #Akhir dari server