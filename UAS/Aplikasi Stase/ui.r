#Import Library yang dibutuhkan
library(ggplot2)
library(shiny)
library(DT)
library(readxl)

#fungsi untuk menampilkan UI
ui <- fluidPage(
  #nama aplikasi
  h2("Aplikasi Stase", style="
       font-family: 'Lobster', cursive;
        color: blue;
        text-align:center
       "),
  
  #biodata mahasiswa
  h4("Azhar Rizki Zulma - 065001900001", style="
    font-family: 'cursive';
    color: #ad1d28;
    text-align:center
    "),
  
  #foto mahasiswa
  HTML('<center><img src="azhar.jpeg" width="150"></center>'),
  
  #navbar
  tabsetPanel(
    
    #Opsi Navigasi Input/Import/Upload Data
    tabPanel("Input Data",
          
        #Judul
         h1("Input Data"),
         sidebarLayout(
           
           #panel kiri
           sidebarPanel(
             
             #fungsi upload data
             fileInput("ambil_file_data", "Choose .txt/.csv/.xlsx File",
                       accept = c(
                         "text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv",
                         ".xlsx")
             ),
             
             #fungsi memilih pemisah variabel/data
             radioButtons("pemisah_variabel", "Separator",
                          choices = c(Comma = ",",
                                      Semicolon = ";",
                                      Tab = "\t",
                                      xlsx = "xlsx"),
                          selected = ",", inline = TRUE)
             
             
           ),
           
           #panel kanan (panel utama) untuk menampilkan data
           mainPanel(
             DT::DTOutput("dataku")
           )
         ),
         br()             
      ),

      #Opsi Navigasi Statistik
      tabPanel("Statistika", 
         
        #Judul
         h1("Statistika"),
         br(),
         h2("Statistika", style="
            font-family: 'cursive';
            color: #0000cd;
            font-size:40px;
            font-weight: bold; 
            text-align:left
            "),
         br(),
         
        #UI output untuk menampilkan opsi pemilihan variabel
         uiOutput("pemilihan_variabel_selectizeinput_single_input_bar_chart_1"),
         br(),
         
        #UI output untuk menampilkan opsi hasil perhitungan statistik (mean, dll)
         DT::DTOutput("hasil_statistika"),
         
        #UI untuk menampilkan bar chart frekuensi
         shinycssloaders::withSpinner(plotOutput("bar_chart_1_500_500", width = "500px", height = "500px" )),
         br()             
    ),
  ),
  br()         
) #Akhir dari UI

