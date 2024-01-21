library(shiny)
library(readxl)
library(data.table)
library(reshape)
library(RSQLite)
library(DT)
library(ggplot2)
library(plotly)
library(ISOweek)
library(googleVis)
library(rmarkdown)

if (interactive()) {

  app <- shinyApp(
    
    ui = fluidPage(
      
      titlePanel("Aplikacja"),
      
      sidebarLayout(
        sidebarPanel(
          conditionalPanel(condition="input.tabselected==1",
                           selectInput('age', 'Wiek', c("0 - Inf", "00 - 04", "05 - 09", "10 - 14", "15 - 19", "20 - 24",
                                                        "25 - 29", "30 - 34", "35 - 39", "40 - 44", "45 - 49", "50 - 54",
                                                        "55 - 59", "60 - 64", "65 - 69", "70 - 74", "75 - 79", "80 - 84",
                                                        "85 - 89", "90 - Inf")),
                           selectInput('sex', 'Plec', c("Ogolem", "Mezczyzni", "Kobiety")),
                           selectInput('geo', 'Region', c("Polska", "Zachodniopomorskie", "Pomorskie", "Lubuskie", "Wielkopolskie", "Kujawsko-Pomorskie",
                                                          "Podlaskie", "Lubelskie", "Podkarpackie", "??dzkie", "Makroregion Wojew?dztwo Mazowieckie",
                                                          "Ma?opolskie", "Opolskie", "?wi?tokrzyskie", "Warmi?sko-Mazurskie", "Dolno?l?skie", "?l?skie")),
                           selectInput('time', 'Granulacja czasowa', c("tydzie?", "miesi?c", "rok"))
          ),
          conditionalPanel(condition="input.tabselected==2",
                           selectInput('sexEU', 'Plec', c("Total", "Males", "Females")),
                           selectInput('geoEU', 'Region', c("Poland", "Albania", "Andorra", "Armenia", "Austria", "Belgium"
                                                            , "Bulgaria", "Croatia", "Cyprus", "Czechia", "Denmark", "Estonia"
                                                            , "Finland", "France", "Georgia", "Germany (until 1990 former territory of the FRG)"
                                                            , "Greece", "Hungary", "Iceland", "Ireland", "Italy", "Latvia", "Liechtenstein"
                                                            , "Lithuania", "Luxembourg", "Malta", "Montenegro", "Netherlands"
                                                            , "Norway", "Portugal", "Romania", "Serbia", "Slovakia", "Slovenia"
                                                            , "Spain", "Sweden", "Switzerland", "United Kingdom")),
                           selectInput('timeEU', 'Granulacja czasowa', c("tydzie?", "rok"))
          ) 
        ),
        
        mainPanel(
          tabsetPanel(type = "tabs",
                      
                      tabPanel("Pobieranie danych",
                               HTML(paste0("<hr>Pobierz dane ze strony GUS:<hr>")),
                               actionButton(inputId = "open_page_GUS", label = "Otworz polaczenie http"),
                               HTML(paste0("<hr>Nastepnie przenies dane GUS do podkatalogu data.<hr>")),
                               
                               HTML(paste0("<hr>Pobierz dane ze strony EUROSTAT:<hr>")),
                               actionButton(inputId = "open_page_EUROSTAT", label = "Otworz polaczenie http"),
                               HTML(paste0("<hr>Nastepnie przenies dane GUS do podkatalogu data.<hr>")),
                               
                               HTML(paste0("<hr>W celu utworzenia bazy danych kliknij przycisk ponizej:<hr>")),
                               actionButton(inputId = "create_datebase", label = "Utworz baze danych")
                      ),
                      
                      tabPanel("SQL",
                               textInput("sqlQueryInput",
                                         label = "Zapytanie SQL",
                                         value = "select TIME as timestamp, SEX as variable, Value as value from EUROSTAT"    
                               ),
                               DT::dataTableOutput("dataSample"),
                               textInput("filenaming",
                                         label = "nazwa pliku csv",
                                         value = "przyklad"
                               ),
                               downloadButton("downloadData", "Eksportuj do csv"),
                               plotlyOutput("plot")
                      ),
                      
                      tabPanel("Mapa GUS",
                               fluidRow(
                                 column(6,
                                        textInput("MapaInput",
                                                  label = "Okres - poczatkowa data",
                                                  value = "2020-01-01"
                                        ),
                                        textInput("MapaInput2",
                                                  label = "Okres - koncowa data",
                                                  value = "2021-12-01"
                                        ),
                                        htmlOutput("MapaGUS")
                                 ),
                                 column(6,
                                        textInput("MapaInput_wzgledna",
                                                  label = "Okres do porownania - poczatkowa data",
                                                  value = "2015-01-01"
                                        ),
                                        textInput("MapaInput2_wzgledna",
                                                  label = "Okres do porownania - koncowa data",
                                                  value = "2019-12-31"
                                        ),
                                        htmlOutput("MapaGUS_wzgledna")
                                 )
                               )
                      ),
                      
                      tabPanel("Mapa EU",
                               fluidRow(
                                 column(6,
                                        textInput("MapaInputEU",
                                                  label = "Okres - poczatkowa data",
                                                  value = "2020-01-01"
                                        ),
                                        textInput("MapaInputEU2",
                                                  label = "Okres - koncowa data",
                                                  value = "2021-12-01"
                                        ),
                                        htmlOutput("MapaEU")
                                 ),
                                 column(6,
                                        textInput("MapaInputEU_wzgledna",
                                                  label = "Okres do porownania - poczatkowa data",
                                                  value = "2015-01-01"
                                        ),
                                        textInput("MapaInputEU2_wzgledna",
                                                  label = "Okres do porownania - koncowa data",
                                                  value = "2019-12-31"
                                        ),
                                        htmlOutput("MapaEU_wzgledna")
                                 )
                               )
                      ),
                      
                      tabPanel("Szeregi czasowe GUS", value=1,
                               fluidRow(
                                 column(6,
                                        textInput("SzeregczasowyInput",
                                                  label = "Okres - poczatkowa data",
                                                  value = "2020-01-01"
                                        ),
                                        textInput("SzeregczasowyInput2",
                                                  label = "Okres - koncowa data",
                                                  value = "2021-12-01"
                                        ),
                                        plotlyOutput("Szeregczasowy")
                                 ),
                                 column(6,
                                        textInput("SzeregczasowyInput_wzgledny",
                                                  label = "Okres do porownania - poczatkowa data",
                                                  value = "2015-01-01"
                                        ),
                                        textInput("SzeregczasowyInput2_wzgledny",
                                                  label = "Okres do porownania - koncowa data",
                                                  value = "2019-12-31"
                                        ),
                                        plotlyOutput("Szeregczasowy_wzgledny")
                                 )
                               )
                      ),
                      
                      tabPanel("Szeregi czasowe EU", value=2,
                               fluidRow(
                                 column(6,
                                        textInput("SzeregczasowyInputEU",
                                                  label = "Okres - poczatkowa data",
                                                  value = "2020-01-01"
                                        ),
                                        textInput("SzeregczasowyInputEU2",
                                                  label = "Okres - koncowa data",
                                                  value = "2021-12-01"
                                        ),
                                        plotlyOutput("SzeregczasowyEU")
                                 ),
                                 column(6,
                                        textInput("SzeregczasowyInputEU_wzgledny",
                                                  label = "Okres do porownania - poczatkowa data",
                                                  value = "2015-01-01"
                                        ),
                                        textInput("SzeregczasowyInputEU2_wzgledny",
                                                  label = "Okres do porownania - koncowa data",
                                                  value = "2019-12-31"
                                        ),
                                        plotlyOutput("SzeregczasowyEU_wzgledny")
                                 )
                               )
                      ),
                      
                      tabPanel("Report", downloadButton("downloadReport")),
                      
                      id="tabselected"
          )
        )
      )
      
    ),
    
    server = function(input, output) {
      
      fileVar <- reactiveValues(
        fileText = NULL
      )
      
      observeEvent(input$filenaming,{
        fileVar$fileText <- input$filenaming
      })
      
      observeEvent(input$open_page_GUS,{
        browseURL("https://stat.gov.pl/obszary-tematyczne/ludnosc/ludnosc/")
      })  
      
      observeEvent(input$open_page_EUROSTAT,{
        browseURL("https://ec.europa.eu/eurostat/web/population-demography/demography-population-stock-balance/database?node_code=demomwk")
      })  
      
      observeEvent(input$create_datebase,{
        
        # wczytanie danych GUS
        dataDir    <- file.path(getwd(),"data")
    
        unzip(file.path(dataDir,"zgony_wg_tygodni.zip"),exdir=file.path(dataDir),setTimes=T)
        
        hd <- getwd()
        setwd(file.path(dataDir,"zgony_wg_tygodni"))
        try({
          lapply(dir(),function(f){
            file.rename(
              from=f, 
              to = gsub(" ","_",gsub("\x88","l",f))
            )
          })
        })
        setwd(hd)
        
        czytajDaneLiczboweZZakladki <- function(f,sheet,plec){
          
          d <- as.data.frame(read_excel(f,sheet=sheet))
          colnames(d)[1:3] <- c("Grupa_wiekowa","Region_id","Region")
          d <- d[-c(1:(grep("^Og",d$Grupa_wiekowa)[1]-1)),]
          
          tygodnie <- 1:(ncol(d)-3)
          tygodnie[nchar(tygodnie)<2] <- paste0("0",tygodnie[nchar(tygodnie)<2])
          colnames(d)[4:ncol(d)] <- tygodnie
          
          d <- reshape::melt(d,id.vars=c("Grupa_wiekowa","Region_id","Region"))
          colnames(d) <- c("Grupa_wiekowa","Region_id","Region","Tydzien","Liczba")
          d$Grupa_wiekowa[grep("Og",d$Grupa_wiekowa)] <- "0 - Inf"
          d$Grupa_wiekowa[grep("wi",d$Grupa_wiekowa)] <- "90 - Inf"
          #d$Liczba[is.na(d$Liczba)] <- 0 
          d <- cbind("Plec"=plec,d)
          
          return(d)
          
        }

        hd <- getwd()
        setwd(file.path(dataDir,"zgony_wg_tygodni"))
        
        try({
          mainRet <- do.call("rbind",lapply(dir(),function(f){
            print(f)
            
            ogolem <- czytajDaneLiczboweZZakladki(f,1,"Ogolem")
            mezczyzni <- czytajDaneLiczboweZZakladki(f,2,"Mezczyzni")
            kobiety <- czytajDaneLiczboweZZakladki(f,3,"Kobiety")
            
            dane <- rbind(ogolem,mezczyzni,kobiety)

            tygodnie <- as.data.frame(read_excel(f,sheet=grep("tyg",tolower(excel_sheets(f)))))
            tygodnie <- do.call("rbind",lapply(split(tygodnie,tygodnie[,2]),function(x){
              return(data.frame(Tydzien=unique(x[,2]),Od=min(x[,1]),Do=max(x[,1])))
            }))
            tygodnie$Tydzien <- gsub("T|W","",unlist(lapply(strsplit(tygodnie$Tydzien,"-"),function(x){x[2]})))
            rownames(tygodnie) <- NULL
            
            dane <- merge(x=dane,y=tygodnie,by="Tydzien",all=T)
            dane <- dane[,-which(colnames(dane)=="Tydzien")]
            
            dane <- dane[c("Od","Do","Plec","Grupa_wiekowa","Region_id","Region","Liczba")]
            dane$Liczba <- as.integer(dane$Liczba)
            
            dane$Grupa_wiekowa[dane$Grupa_wiekowa=="0 - 4"] <- "00 - 04"
            dane$Grupa_wiekowa[dane$Grupa_wiekowa=="5 - 9"] <- "05 - 09"
            
            return(dane)
          }))
          
          write.table(mainRet,file="../GUS_dane_przetworzone_pelne.csv",sep=";",dec=",",row.names=F)
        })
        setwd(hd)
        
        #wczytanie danych EUROSTAT
        
        dataDir    <- file.path(getwd(),"data")
  
        unzip(file.path(dataDir,"demo_r_mwk_ts.zip"),exdir=file.path(dataDir),setTimes=T)
        
        #tworzennie bazy danych
        
        dbName <- file.path(dataDir,"GUS_data.db")
        
        con <- dbConnect(
          dbDriver("SQLite"),
          dbname = dbName
        )
        
        try({
          d <- read.table(file=file.path(dataDir,"GUS_dane_przetworzone_pelne.csv"),sep=";",dec=",",header=T)
          d2 <- read.table(file=file.path(dataDir, "demo_r_mwk_ts_1_Data.csv"),sep=",",dec=",",header=T,stringsAsFactors=F)
          d2$Value <- as.integer(gsub(",|:","",d2$Value))
          d2 <- d2[d2$TIME!="2021W99", ]
          dbWriteTable(con, "GUS", d, overwrite = TRUE, row.names = FALSE)
          dbWriteTable(con, "EUROSTAT", d2, overwrite = TRUE, row.names = FALSE)
        })

        dbDisconnect(con)
        
      }) 
      
      ret <- reactive ({
        try({
          dataDir <- file.path(getwd(),"data")
          dbName <- file.path(dataDir,"GUS_data.db")
          con <- dbConnect(
            dbDriver("SQLite"),
            dbname = dbName
          )
          d <- data.frame()
          d <- dbGetQuery(con, input$sqlQueryInput)
          return(d)
          dbDisconnect(con)
        },
        silent=T)
        return(data.frame())
      })
      
      output$dataSample <- DT::renderDataTable({
        DT::datatable(  
          ret(), 
          rownames = FALSE,
          filter="top",
          options = list(
            scrollX = TRUE,
            pageLength = 16,
            lengthMenu = seq(from=10,by=10,to=100) 
          )
        )
      })
      
      output$downloadData <- downloadHandler(
        filename = function() {
          paste(fileVar$fileText, ".csv", sep = "")
        },
        content = function(file) {
          write.csv(ret(), file, row.names = FALSE)
        }
      )
      
      output$plot<-renderPlotly({
        d=ret()
        if(ncol(d)==3){
          if(colnames(d)[1]=="timestamp" & colnames(d)[2]=="variable" & colnames(d)[3]=="value"){
            d <- aggregate(value ~ timestamp+variable, data=d, FUN=sum, na.rm=T)
            d$timestamp <- tryCatch(as.Date(d$timestamp), error = function(e) ISOweek2date(paste(substr(d$timestamp, 1, 4), "-", substr(d$timestamp, 5, 7), "-1", sep="")))
            img <- ggplotly(ggplot(d, aes(x=timestamp,y=value,col=variable)) + geom_line())
          } else {
            img <- ggplot(data.frame())
          }
        } else {
          img <- ggplot(data.frame())
        }
        print(img)
      })
      
      map <- reactive({
        try({
          dataDir    <- file.path(getwd(),"data")
          dbName <- file.path(dataDir,"GUS_data.db")
          # zapytanie 
          con <- dbConnect(
            dbDriver("SQLite"),
            dbname = dbName
          )
          d <- data.frame()
          d <- dbGetQuery(con, paste("select Od, Region, Liczba from GUS where Grupa_wiekowa='0 - Inf' and Plec='Ogolem'"))
          d$Od <- as.Date(d$Od)
          tryCatch({
            d <- d[d$Od>=input$MapaInput & d$Od<=input$MapaInput2, ]
            d <- aggregate(Liczba~Region, data=d, FUN=sum, na.rm=T)
            d <- d[d$Region=="Zachodniopomorskie" | d$Region=="Pomorskie" | d$Region=="Lubuskie" |
                     d$Region=="Wielkopolskie" | d$Region=="Kujawsko-Pomorskie" | d$Region=="Podlaskie" |
                     d$Region=="Lubelskie" | d$Region=="Podkarpackie" | d$Region=="??dzkie" |
                     d$Region=="Makroregion Wojew?dztwo Mazowieckie" | d$Region=="Ma?opolskie" | d$Region=="Opolskie" |
                     d$Region=="?wi?tokrzyskie" | d$Region=="Warmi?sko-Mazurskie" | d$Region=="Dolno?l?skie" |
                     d$Region=="?l?skie", ]
            d$Region=c("PL-DS", "PL-KP", "PL-LU", "PL-LB", "PL-LD", "PL-MZ", "PL-MA", "PL-OP", "PL-PK", "PL-PD",
                       "PL-PM", "PL-SL", "PL-SK", "PL-WN", "PL-WP", "PL-ZP")
          },
          error=function(e) {d <<- data.frame()})
          return(d)
          dbDisconnect(con)
        },
        silent=T)
        return(data.frame())
      })
      
      output$MapaGUS <- renderGvis({
        d=map()
        if(nrow(d)){
          img <- gvisGeoChart(
            d,
            locationvar="Region",
            colorvar="Liczba",
            options=list(region="PL",
                         displayMode="regions",
                         resolution="provinces",
                         width=400, height=400
            ))
        }else{
          img <- ggplot(data.frame())
        }
        return(img)
      })
      
      map_wzgledna <- reactive({
        try({
          dataDir    <- file.path(getwd(),"data")
          dbName <- file.path(dataDir,"GUS_data.db")
          # zapytanie 
          con <- dbConnect(
            dbDriver("SQLite"),
            dbname = dbName
          )
          d <- data.frame()
          d <- dbGetQuery(con, paste("select Od, Region, Liczba from GUS where Grupa_wiekowa='0 - Inf' and Plec='Ogolem'"))
          d$Od <- as.Date(d$Od)
          tryCatch({
            d1 <- d[d$Od>=input$MapaInput & d$Od<=input$MapaInput2, ]
            d2 <- d[d$Od>=input$MapaInput_wzgledna & d$Od<=input$MapaInput2_wzgledna, ]
            d1 <- aggregate(Liczba~Region, data=d1, FUN=sum, na.rm=T)
            d2 <- aggregate(Liczba~Region, data=d2, FUN=sum, na.rm=T)
            d <- d1
            d[,2] <- d1[,2]/d2[,2]
            d <- d[d$Region=="Zachodniopomorskie" | d$Region=="Pomorskie" | d$Region=="Lubuskie" |
                     d$Region=="Wielkopolskie" | d$Region=="Kujawsko-Pomorskie" | d$Region=="Podlaskie" |
                     d$Region=="Lubelskie" | d$Region=="Podkarpackie" | d$Region=="??dzkie" |
                     d$Region=="Makroregion Wojew?dztwo Mazowieckie" | d$Region=="Ma?opolskie" | d$Region=="Opolskie" |
                     d$Region=="?wi?tokrzyskie" | d$Region=="Warmi?sko-Mazurskie" | d$Region=="Dolno?l?skie" |
                     d$Region=="?l?skie", ]
            d$Region=c("PL-DS", "PL-KP", "PL-LU", "PL-LB", "PL-LD", "PL-MZ", "PL-MA", "PL-OP", "PL-PK", "PL-PD",
                       "PL-PM", "PL-SL", "PL-SK", "PL-WN", "PL-WP", "PL-ZP")
          },
          error=function(e) {d <<- data.frame()})
          return(d)
          dbDisconnect(con)
        },
        silent=T)
        return(data.frame())
      })
      
      output$MapaGUS_wzgledna <- renderGvis({
        d=map_wzgledna()
        if(nrow(d)){
          img <- gvisGeoChart(
            d,
            locationvar="Region",
            colorvar="Liczba",
            options=list(region="PL",
                         displayMode="regions",
                         resolution="provinces",
                         width=400, height=400
            ))
        }else{
          img <- ggplot(data.frame())
        }
        return(img)
      })
      
      mapEU <- reactive({
        try({
          dataDir    <- file.path(getwd(),"data")
          dbName <- file.path(dataDir,"GUS_data.db")
          # zapytanie 
          con <- dbConnect(
            dbDriver("SQLite"),
            dbname = dbName
          )
          d <- data.frame()
          d <- dbGetQuery(con, paste("select TIME, GEO, Value from EUROSTAT where SEX='Total'"))
          d$TIME <- ISOweek2date(paste(substr(d$TIME, 1, 4), "-", substr(d$TIME, 5, 7), "-1", sep=""))
          tryCatch({
            d <- d[d$TIME>=input$MapaInputEU & d$TIME<=input$MapaInputEU2, ]
            d <- aggregate(Value~GEO, data=d, FUN=sum, na.rm=T, na.action=na.pass)
            d$GEO=c("AL", "AD", "AM", "AT", "BE", "BG", "HR", "CY", "CZ", "DK", "EE", "FI", "FR", "GE", "DE", "GR", "HU", "IS",
                    "IE", "IT", "LV", "LI", "LT", "LU", "MT", "ME", "NL", "NO", "PL", "PT", "RO", "RS", "SK", "SI", "ES", "SE",
                    "CH", "GB")
            d <- d[d$Value!=0, ]
          },
          error=function(e) {d <<- data.frame()})
          return(d)
          dbDisconnect(con)
        },
        silent=T)
        return(data.frame())
      })
      
      output$MapaEU <- renderGvis({
        d=mapEU()
        if(nrow(d)){
          img <- gvisGeoChart(
            d,
            locationvar="GEO",
            colorvar="Value",
            options=list(region=150,
                         width=400, height=400
            ))
        }else{
          img <- ggplot(data.frame())
        }
        return(img)
      })
      
      mapEU_wzgledna <- reactive({
        try({
          dataDir    <- file.path(getwd(),"data")
          dbName <- file.path(dataDir,"GUS_data.db")
          # zapytanie 
          con <- dbConnect(
            dbDriver("SQLite"),
            dbname = dbName
          )
          d <- data.frame()
          d <- dbGetQuery(con, paste("select TIME, GEO, Value from EUROSTAT where SEX='Total'"))
          d$TIME <- ISOweek2date(paste(substr(d$TIME, 1, 4), "-", substr(d$TIME, 5, 7), "-1", sep=""))
          tryCatch({
            d1 <- d[d$TIME>=input$MapaInputEU & d$TIME<=input$MapaInputEU2, ]
            d2 <- d[d$TIME>=input$MapaInputEU_wzgledna & d$TIME<=input$MapaInputEU2_wzgledna, ]
            d1 <- aggregate(Value~GEO, data=d1, FUN=sum, na.rm=T, na.action=na.pass)
            d2 <- aggregate(Value~GEO, data=d2, FUN=sum, na.rm=T, na.action=na.pass)
            d <- d1
            d[,2] <- d1[,2]/d2[,2]
            d$GEO=c("AL", "AD", "AM", "AT", "BE", "BG", "HR", "CY", "CZ", "DK", "EE", "FI", "FR", "GE", "DE", "GR", "HU", "IS",
                    "IE", "IT", "LV", "LI", "LT", "LU", "MT", "ME", "NL", "NO", "PL", "PT", "RO", "RS", "SK", "SI", "ES", "SE",
                    "CH", "GB")
            d <- d[d$Value!=0, ]
          },
          error=function(e) {d <<- data.frame()})
          return(d)
          dbDisconnect(con)
        },
        silent=T)
        return(data.frame())
      })
      
      output$MapaEU_wzgledna <- renderGvis({
        d=mapEU_wzgledna()
        if(nrow(d)){
          img <- gvisGeoChart(
            d,
            locationvar="GEO",
            colorvar="Value",
            options=list(region=150,
                         width=400, height=400
            ))
        }else{
          img <- ggplot(data.frame())
        }
        return(img)
      })
      
      szeregczasowy <- reactive({
        try({
          dataDir <- file.path(getwd(),"data")
          dbName <- file.path(dataDir,"GUS_data.db")
          # zapytanie
          con <- dbConnect(
            dbDriver("SQLite"),
            dbname = dbName
          )
          d <- data.frame()
          d <- dbGetQuery(con, paste0("select Od, Do, Liczba from GUS where Region='", input$geo, "' and Grupa_wiekowa='", input$age, "' and Plec='", input$sex, "' order by Od", sep=""))
          d$Od <- as.Date(d$Od)
          d$Do <- as.Date(d$Do)
          d <- na.omit(d)
          tryCatch({
            d <- d[d$Od>=input$SzeregczasowyInput & d$Od<=input$SzeregczasowyInput2, ]
            endOfMonth <- function(X){
              X <- as.character(as.Date(paste0(substr(as.character(as.Date(paste0(substr(as.character(X),1,7),"-01")) + 31),1,7),"-01"))-1)
              return(X)
            }
            endOfYear <- function(X){
              X <- as.character(as.Date(paste0(substr(as.character(as.Date(paste0(substr(as.character(X),1,4),"-01-01")) + 366),1,4),"-01-01"))-1)
              return(X)
            }
            dailyData <- do.call("rbind",lapply(split(d,d$Od),function(x){
              tmpSeq <- seq(from=x$Od,to=x$Do,by="1 day")
              return(data.frame(Data=tmpSeq,Liczba=x$Liczba/length(tmpSeq)))
            }))
            if(input$time=="tydzie?"){
              colnames(d)[1] <- "Data"
            } else if(input$time=="miesi?c") {
              dailyData$RokMiesiac <- as.Date(endOfMonth(dailyData$Data))
              d <- aggregate(Liczba~RokMiesiac, data=dailyData, FUN=sum, na.rm=T)
              colnames(d)[1] <- "Data"
              d <- d[order(d$Data),]
              d <- d[-c(1,nrow(d)),]
            } else if(input$time=="rok") {
              dailyData$Rok <- as.Date(endOfYear(dailyData$Data))
              d <- aggregate(Liczba~Rok, data=dailyData, FUN=sum, na.rm=T)
              colnames(d)[1] <- "Data"
              d <- d[order(d$Data),]
              d <- d[-c(1,nrow(d)),]
            }
          },
          error=function(e) {d <<- data.frame()}
          )
          return(d)
          dbDisconnect(con)
        },
        silent=T)
        return(data.frame())
      })
      
      output$Szeregczasowy <- renderPlotly({
        d=szeregczasowy()
        if(nrow(d)){
          img <- (
            ggplotly(ggplot(d, aes(x=Data,y=Liczba))
                     + geom_line()
            ))
        }else{
          img <- ggplot(data.frame())
        }
        return(img)
      })
      
      szeregczasowy_wzgledny <- reactive({
        try({
          dataDir <- file.path(getwd(),"data")
          dbName <- file.path(dataDir,"GUS_data.db")
          # zapytanie
          con <- dbConnect(
            dbDriver("SQLite"),
            dbname = dbName
          )
          d <- data.frame()
          d <- dbGetQuery(con, paste0("select Od, Do, Liczba from GUS where Region='", input$geo, "' and Grupa_wiekowa='", input$age, "' and Plec='", input$sex, "' order by Od", sep=""))
          d$Od <- as.Date(d$Od)
          d$Do <- as.Date(d$Do)
          d <- na.omit(d)
          d$Od <- as.Date(d$Od)
          d$Do <- as.Date(d$Do)
          d <- na.omit(d)
          tryCatch({
            d1 <- d[d$Od>=input$SzeregczasowyInput & d$Od<=input$SzeregczasowyInput2, ]
            d2 <- d[d$Od>=input$SzeregczasowyInput_wzgledny & d$Od<=input$SzeregczasowyInput2_wzgledny, ]
            d <- d1
            endOfMonth <- function(X){
              X <- as.character(as.Date(paste0(substr(as.character(as.Date(paste0(substr(as.character(X),1,7),"-01")) + 31),1,7),"-01"))-1)
              return(X)
            }
            endOfYear <- function(X){
              X <- as.character(as.Date(paste0(substr(as.character(as.Date(paste0(substr(as.character(X),1,4),"-01-01")) + 366),1,4),"-01-01"))-1)
              return(X)
            }
            dailyData <- do.call("rbind",lapply(split(d,d$Od),function(x){
              tmpSeq <- seq(from=x$Od,to=x$Do,by="1 day")
              return(data.frame(Data=tmpSeq,Liczba=x$Liczba/length(tmpSeq)))
            }))
            dailyData2 <- do.call("rbind",lapply(split(d2,d2$Od),function(x){
              tmpSeq <- seq(from=x$Od,to=x$Do,by="1 day")
              return(data.frame(Data=tmpSeq,Liczba=x$Liczba/length(tmpSeq)))
            }))
            if(input$time=="tydzie?"){
              colnames(d)[1] <- "Data"
              d[,3] <- d[,3]/mean(d2[,3])
            } else if(input$time=="miesi?c") {
              dailyData$RokMiesiac <- as.Date(endOfMonth(dailyData$Data))
              d <- aggregate(Liczba~RokMiesiac, data=dailyData, FUN=sum, na.rm=T)
              colnames(d)[1] <- "Data"
              d <- d[order(d$Data),]
              d <- d[-c(1,nrow(d)),]
              dailyData2$RokMiesiac <- as.Date(endOfMonth(dailyData2$Data))
              d2 <- aggregate(Liczba~RokMiesiac, data=dailyData2, FUN=sum, na.rm=T)
              colnames(d2)[1] <- "Data"
              d2 <- d2[order(d2$Data),]
              d2 <- d2[-c(1,nrow(d2)),]
              d[,2] <- d[,2]/mean(d2[,2])
            } else if(input$time=="rok") {
              dailyData$Rok <- as.Date(endOfYear(dailyData$Data))
              d <- aggregate(Liczba~Rok, data=dailyData, FUN=sum, na.rm=T)
              colnames(d)[1] <- "Data"
              d <- d[order(d$Data),]
              d <- d[-c(1,nrow(d)),]
              dailyData2$Rok <- as.Date(endOfYear(dailyData2$Data))
              d2 <- aggregate(Liczba~Rok, data=dailyData2, FUN=sum, na.rm=T)
              colnames(d2)[1] <- "Data"
              d2 <- d2[order(d2$Data),]
              d2 <- d2[-c(1,nrow(d2)),]
              d[,2] <- d[,2]/mean(d2[,2])
            }
          },
          error=function(e) {d <<- data.frame()}
          )
          return(d)
          dbDisconnect(con)
        },
        silent=T)
        return(data.frame())
      })
      
      output$Szeregczasowy_wzgledny <- renderPlotly({
        d=szeregczasowy_wzgledny()
        if(nrow(d)){
          img <- (
            ggplotly(ggplot(d, aes(x=Data,y=Liczba))
                     + geom_line()
            ))
        }else{
          img <- ggplot(data.frame())
        }
        return(img)
      })
      
      szeregczasowyEU <- reactive({
        try({
          dataDir <- file.path(getwd(),"data")
          dbName <- file.path(dataDir,"GUS_data.db")
          # zapytanie
          con <- dbConnect(
            dbDriver("SQLite"),
            dbname = dbName
          )
          d <- data.frame()
          d <- dbGetQuery(con, paste0("select TIME, Value from EUROSTAT where GEO='", input$geoEU, "' and SEX='", input$sexEU, "' order by TIME", sep=""))
          d$TIME<-ISOweek2date(paste(substr(d$TIME, 1, 4), "-", substr(d$TIME, 5, 7), "-1", sep=""))
          d <- na.omit(d)
          tryCatch({
            d <- d[d$TIME>=input$SzeregczasowyInputEU & d$TIME<=input$SzeregczasowyInputEU2, ]
            endOfYear <- function(X){
              X <- as.character(as.Date(paste0(substr(as.character(as.Date(paste0(substr(as.character(X),1,4),"-01-01")) + 366),1,4),"-01-01"))-1)
              return(X)
            }
            dailyData <- do.call("rbind",lapply(split(d,d$TIME),function(x){
              tmpSeq <- seq(from=x$TIME,to=(as.Date(x$TIME)+6),by="1 day")
              return(data.frame(Data=tmpSeq,Value=x$Value/length(tmpSeq)))
            }))
            if(input$timeEU=="tydzie?"){
              colnames(d)[1] <- "Data"
            } else if(input$timeEU=="rok") {
              dailyData$Rok <- as.Date(endOfYear(dailyData$Data))
              d <- aggregate(Value~Rok, data=dailyData, FUN=sum, na.rm=T)
              colnames(d)[1] <- "Data"
              d <- d[order(d$Data),]
              d <- d[-c(1,nrow(d)),]
            }
          },
          error=function(e) {d <<- data.frame()}
          )
          return(d)
          dbDisconnect(con)
        },
        silent=T)
        return(data.frame())
      })
      
      output$SzeregczasowyEU <- renderPlotly({
        d=szeregczasowyEU()
        if(nrow(d)){
          img <- (
            ggplotly(ggplot(d, aes(x=Data,y=Value))
                     + geom_line()
            ))
        }else{
          img <- ggplot(data.frame())
        }
        return(img)
      })
      
      szeregczasowyEU_wzgledny <- reactive({
        try({
          dataDir <- file.path(getwd(),"data")
          dbName <- file.path(dataDir,"GUS_data.db")
          # zapytanie
          con <- dbConnect(
            dbDriver("SQLite"),
            dbname = dbName
          )
          d <- data.frame()
          d <- dbGetQuery(con, paste0("select TIME, Value from EUROSTAT where GEO='", input$geoEU, "' and SEX='", input$sexEU, "' order by TIME", sep=""))
          d$TIME<-ISOweek2date(paste(substr(d$TIME, 1, 4), "-", substr(d$TIME, 5, 7), "-1", sep=""))
          d <- na.omit(d)
          tryCatch({
            d1 <- d[d$TIME>=input$SzeregczasowyInputEU & d$TIME<=input$SzeregczasowyInputEU2, ]
            d2 <- d[d$TIME>=input$SzeregczasowyInputEU_wzgledny & d$TIME<=input$SzeregczasowyInputEU2_wzgledny, ]
            d <- d1
            endOfYear <- function(X){
              X <- as.character(as.Date(paste0(substr(as.character(as.Date(paste0(substr(as.character(X),1,4),"-01-01")) + 366),1,4),"-01-01"))-1)
              return(X)
            }
            dailyData <- do.call("rbind",lapply(split(d,d$TIME),function(x){
              tmpSeq <- seq(from=x$TIME,to=(as.Date(x$TIME)+6),by="1 day")
              return(data.frame(Data=tmpSeq,Value=x$Value/length(tmpSeq)))
            }))
            dailyData2 <- do.call("rbind",lapply(split(d2,d2$TIME),function(x){
              tmpSeq <- seq(from=x$TIME,to=(as.Date(x$TIME)+6),by="1 day")
              return(data.frame(Data=tmpSeq,Value=x$Value/length(tmpSeq)))
            }))
            if(input$timeEU=="tydzie?"){
              colnames(d)[1] <- "Data"
            } else if(input$timeEU=="rok") {
              dailyData$Rok <- as.Date(endOfYear(dailyData$Data))
              d <- aggregate(Value~Rok, data=dailyData, FUN=sum, na.rm=T)
              colnames(d)[1] <- "Data"
              d <- d[order(d$Data),]
              d <- d[-c(1,nrow(d)),]
              dailyData2$Rok <- as.Date(endOfYear(dailyData2$Data))
              d2 <- aggregate(Value~Rok, data=dailyData2, FUN=sum, na.rm=T)
              colnames(d2)[1] <- "Data"
              d2 <- d2[order(d2$Data),]
              d2 <- d2[-c(1,nrow(d2)),]
            }
            d[,2] <- d[,2]/mean(d2[,2])
          },
          error=function(e) {d <<- data.frame()}
          )
          return(d)
          dbDisconnect(con)
        },
        silent=T)
        return(data.frame())
      })
      
      output$SzeregczasowyEU_wzgledny <- renderPlotly({
        d=szeregczasowyEU_wzgledny()
        if(nrow(d)){
          img <- (
            ggplotly(ggplot(d, aes(x=Data,y=Value))
                     + geom_line()
            ))
        }else{
          img <- ggplot(data.frame())
        }
        return(img)
      })
      
      output$downloadReport <- downloadHandler(
        filename = "report.html",
        
        content = function(file) {
          src <- normalizePath('report.Rmd')
          owd <- setwd(tempdir())
          on.exit(setwd(owd))
          file.copy(src, 'report.Rmd', overwrite = TRUE)
          
          out <- render('report.Rmd', html_document())
          file.rename(out, file)
        }
      )
      
    })
  
  runApp(app)

}