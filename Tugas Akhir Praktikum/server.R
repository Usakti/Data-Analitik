


server <- function(input, output) {
  
  basis_data <- read.table("basis_data.txt", header = T, sep = "\t")
  
  data_rapi <- basis_data
  
  warna_marker = data_rapi[,11]
  
  colnames(data_rapi) = c("Wilayah", "Longitude", "Latitude", "Harga November", "Harga Desember", "Harga Rerata", "Harga November Minggu Pertama", "Harga November Minggu Kedua", "Harga November Minggu Ketiga", "Harga November Minggu Keempat", "Warna", "Circle", "Marker")
  
  wilayah <- basis_data[,1]
  
  data_lat <- basis_data[,3]
  
  # Create tree geometries
  #tree_aceh <- st_point(c(long, lat))
  tree_aceh <- st_point(c(  basis_data[1,2]  , basis_data[1,3]  ))
  tree_sumut <- st_point(c(  basis_data[2,2]  , basis_data[2,3]  ))
  tree_sumbar <- st_point(c(  basis_data[3,2]  , basis_data[3,3]  ))
  tree_riau <- st_point(c(  basis_data[4,2]  , basis_data[4,3]  ))
  tree_kepri <- st_point(c(  basis_data[5,2]  , basis_data[5,3]  ))
  tree_jambi <- st_point(c(  basis_data[6,2]  , basis_data[6,3]  ))
  tree_bengkulu <- st_point(c(  basis_data[7,2]  , basis_data[7,3]  ))
  tree_sumsel <- st_point(c(  basis_data[8,2]  , basis_data[8,3]  ))
  tree_bangka <- st_point(c(  basis_data[9,2]  , basis_data[9,3]  ))
  tree_lampung <- st_point(c(  basis_data[10,2]  , basis_data[10,3]  ))
  tree_banten <- st_point(c(  basis_data[11,2]  , basis_data[11,3]  ))
  tree_jakarta <- st_point(c(  basis_data[12,2]  , basis_data[12,3]  ))
  tree_jabar <- st_point(c(  basis_data[13,2]  , basis_data[13,3]  ))
  tree_jateng <- st_point(c(  basis_data[14,2]  , basis_data[14,3]  ))
  tree_yogyakarta <- st_point(c(  basis_data[15,2]  , basis_data[15,3]  ))
  tree_jatim <- st_point(c(  basis_data[16,2]  , basis_data[16,3]  ))
  tree_bali <- st_point(c(  basis_data[17,2]  , basis_data[17,3]  ))
  tree_ntb <- st_point(c(  basis_data[18,2]  , basis_data[18,3]  ))
  tree_ntt <- st_point(c(  basis_data[19,2]  , basis_data[19,3]  ))
  tree_kalbar <- st_point(c(  basis_data[20,2]  , basis_data[20,3]  ))
  tree_kalteng <- st_point(c(  basis_data[21,2]  , basis_data[21,3]  ))
  tree_kalut <- st_point(c(  basis_data[22,2]  , basis_data[22,3]  ))
  tree_kaltim <- st_point(c(  basis_data[23,2]  , basis_data[23,3]  ))
  tree_kalsel <- st_point(c(  basis_data[24,2]  , basis_data[24,3]  ))
  tree_gorontalo <- st_point(c(  basis_data[25,2]  , basis_data[25,3]  ))
  tree_sulsel <- st_point(c(  basis_data[26,2]  , basis_data[26,3]  ))
  tree_sulbar <- st_point(c(  basis_data[27,2]  , basis_data[27,3]  ))
  tree_sulut <- st_point(c(  basis_data[28,2]  , basis_data[28,3]  ))
  tree_sulteng <- st_point(c(  basis_data[29,2]  , basis_data[29,3]  ))
  tree_sultenggara <- st_point(c(  basis_data[30,2]  , basis_data[30,3]  ))
  tree_maluku <- st_point(c(  basis_data[31,2]  , basis_data[31,3]  ))
  tree_malut <- st_point(c(  basis_data[32,2]  , basis_data[32,3]  ))
  tree_papua <- st_point(c(  basis_data[33,2]  , basis_data[33,3]  ))
  tree_papbar <- st_point(c(  basis_data[34,2]  , basis_data[34,3]  ))
  
  # Create sfc object with multiple sfg objects
  points_sfc <- st_sfc(tree_aceh, tree_sumut, tree_sumbar, tree_riau, tree_kepri, 
                       tree_jambi, tree_bengkulu, tree_sumsel, tree_bangka, tree_lampung, 
                       tree_banten, tree_jakarta, tree_jabar, tree_jateng, tree_yogyakarta, 
                       tree_jatim, tree_bali, tree_ntb, tree_ntt, tree_kalbar, tree_kalteng, 
                       tree_kalut, tree_kaltim, tree_kalsel, tree_gorontalo, tree_sulsel, 
                       tree_sulbar, tree_sulut, tree_sulteng, tree_sultenggara, tree_maluku, 
                       tree_malut, tree_papua, tree_papbar, crs = 4326)
  
  tree_data <- st_sf(basis_data, geometry = points_sfc)
  
  icons <- awesomeIcons(
    #icon = 'ios-close',
    #iconColor = 'black',
    library = 'ion',
    markerColor = warna_marker
  )
  
  output$mymap <- renderLeaflet({
    leaflet(data = tree_data) %>%
      addProviderTiles(providers$Stamen.Watercolor) %>%
      
      # Centre the map in the middle of Toronto
      addProviderTiles("OpenStreetMap") %>%
      setView( lng = 118
               , lat = -2.5
               , zoom = 4 ) %>%
      setMaxBounds( lng1 = 89.5
                    , lat1 = -14.5
                    , lng2 = 149.5
                    , lat2 = 9.5 ) %>%
      addCircleMarkers( ~ longitude, ~ latitude,
                        fillOpacity = 1,
                        radius = ~ circle,
                        popup = wilayah,
                        color = ~ warna) %>%
      addAwesomeMarkers( ~ longitude, ~latitude,  label =  wilayah,
                         icon = icons )
  })
  
  observeEvent(input$mymap_marker_click, { 
    p <- input$mymap_marker_click  # typo was on this line
    
    p_lat = p$lat
    
    terpilih <- which(data_lat == p_lat)
    
    if(!is.null(terpilih)){
      #############################
      ##########Tabel Data#########
      
      output$mytable = DT::renderDataTable({
        data_rapi[terpilih,]
      })
      
      ##########Menampilkan Grafik###########
      gambar_ku<- function(){
        #########Bulan########
        
        ekstrak_data = basis_data[terpilih,] #artinya menyimpan data pada wilayah yang dipilih
        
        x = c(ekstrak_data[1,4], ekstrak_data[1,5]) #X menyimpan jumlah perempuan dan jumlah laki-laki
        
        xlabel = c("November", "Desember")
        
        hargaBulan = data.frame(x, xlabel)
        
        phb = ggplot(data = hargaBulan, aes(x = xlabel, y = x, fill = xlabel)) + geom_bar(stat = "identity") +
          theme_tufte() +
          xlab("Perbandingan Harga Bulan (November & Desember)") +
          theme(legend.position = "none") + ylab("") + geom_text(aes(label = x), position = position_dodge(0.9), size = 5)
        
        ########Minggu########
        
        ekstrak_data = basis_data[terpilih,]
        
        x = c(ekstrak_data[1,7], ekstrak_data[1,8], ekstrak_data[1,9], ekstrak_data[1,10]) #menyatakan harga minggu ke 1 2 3 dan 4
        
        xlabel = c("Minggu 1", "Minggu 2", "Minggu 3", "Minggu 4")
        
        hargaMinggu = data.frame(x, xlabel)
        
        phm = ggplot(data = hargaMinggu, aes(x = xlabel, y = x, fill = xlabel)) + geom_bar(stat = "identity") +
          theme_tufte() +
          xlab("Perbandingan Harga Perminggu (November)") +
          theme(legend.position = "none") + ylab("") + geom_text(aes(label = x), position = position_dodge(0.9), size = 5) +
          scale_fill_manual(values = c("#F1948A", "#F9E79F", "#49E20E", "#85C1E9"))
        
        ##############
        
        figure <- ggarrange(phb, phm,
                            labels = c("", ""),
                            ncol = 2, nrow = 2)
        figure
        
      }
      
      output$gambar_ku <- renderPlot({
        print(gambar_ku())
      })
      
    }
    
  })
}