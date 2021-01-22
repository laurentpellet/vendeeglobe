library(data.table)
library(lubridate)
library(xlsx)
library(data.table)
library(reshape2)
library(lubridate)
library(ggplot2)
library(ggrepel)
library(scales)
library(gridExtra)

baseURL <- "https://www.vendeeglobe.org/download-race-data/"

convertKtsToNum <- function(x){
  as.numeric(gsub(" kts", "", x))
}

convertNmToNum <- function(x){
  as.numeric(gsub(" nm", "", x))
}

removeBackslashN <- function(x){
  gsub("\\n", "", x)
}

generateAllFiles <- function(){
  dayMin  <- as.Date('2020-11-09', tz="UTC")
  dayMax  <- as.Date(format(Sys.time(), tz="UTC"))
  
  allDates <- format(seq(dayMin , dayMax, by="day"), "%Y%m%d")
  allHours <- c("04","08","11","14","17","21")
  allDates <-  as.POSIXct(c(outer(allDates, allHours, FUN=paste0)),tryFormats = c("%Y%m%d%H"), tz="UTC")
  allDates <- allDates[allDates<Sys.time()]
  allDates <- allDates[order(allDates)]

    format(allDates, "vendeeglobe_%Y%m%d%_%H0000.xlsx" )
}

getMissingFiles <- function(){
  A <- generateAllFiles()
  B <- list.files("input")
  A[which(!A %in% B)]
}

convertXLStoDTAndSave <- function(file){
  message(sprintf("Ouverture du fichier %s", file))
  dt <- as_datetime(substring(basename(file),13,27), tz = "UTC")
  vg <- read.xlsx(paste0("input/", file), 1, startRow = 5, endRow = 38, encoding = "UTF-8")
  cols <- c("Rang", "Voile", "Skiper", "Heure", "Lat", "Long", 
            "Cap30", "Vit30", "VMG30", "Dist30",
            "CapLast", "VitLast", "VMGLast", "DistLast",
            "Cap24H", "Vit24H", "VMG24H", "Dist24H", "DTF", "DTL")
  
  colnames(vg) <- cols
  vg <- setDT(cbind(dt, colsplit(vg$Skiper, "\n", c("Skipper", "Bateau")), vg[,-3]))
  
  colsKts <- c(grep("VMG", cols, value = T),  grep("Vit", cols, value = T))
  colsNm  <- c(grep("Dist", cols, value = T), grep("DT", cols, value = T))
  colsBN  <- c("Voile", "Heure")
  vg[,(colsKts):=lapply(.SD, convertKtsToNum), .SDcols=colsKts] 
  vg[,(colsNm):=lapply(.SD, convertNmToNum), .SDcols=colsNm]
  vg[,(colsBN):=lapply(.SD, removeBackslashN), .SDcols=colsBN] 
  fwrite(vg, paste0("output/",tools::file_path_sans_ext(file), ".csv"), sep=";")
}




downloadFile <- function(x){
  download.file(paste0(baseURL, x), file.path("input", x), method="curl")
}


createCompleteSet <- function(){
  vg <- rbindlist(lapply(list.files("output", full.names = T),
                         FUN=function(x) fread(x, encoding="UTF-8")))
  
  vg[Voile=="GBR 99", Bateau:="Hugo Boss"]
  vg[,Heure:=format(dt, "%d/%m/%Y %H:00:00")]
  vg[, Place:=as.numeric(Rang)]
  vg
}


ispfPalette <- c("#0071B2", "#A349A4", "#F07D17", "#009FA4", "#999999", "#FF0000", "#D55E00", "#CC79A7")

theme_ispf <- function (base_size = 8, base_family = "Roboto Light") 
{
  bgcolor <- "#FFFFFF"
  ret <- theme(rect = element_rect(fill = bgcolor, 
                                   linetype = 0, 
                                   colour = NA), 
               text = element_text(size = base_size, family = base_family), 
               title = element_text(hjust = 0.5, family = base_family, face="bold"), 
               plot.title = element_text(hjust = 0.5, family = base_family, face="bold"), 
               #axis.title.x = element_blank(),
               #axis.title.x = element_text(hjust = 0.5), 
               axis.title.y = element_text(hjust = 0.5),
               panel.grid.major.y = element_line(colour = "#D8D8D8"), 
               panel.grid.minor.y = element_blank(),
               panel.grid.major.x = element_blank(), 
               panel.grid.minor.x = element_blank(), 
               panel.border = element_blank(), 
               panel.background = element_blank(),
               legend.key = element_rect(fill = "#FFFFFF00"),
               plot.margin=grid::unit(c(0,0,0,0), "mm"),
               legend.position = "right",
               legend.direction = "vertical",
               legend.title = element_blank(),
               legend.key.width=unit(0.2, "cm"))
  ret
}


createGraphe <- function(nbCoureurs, nbJours, variable){
  aSuivre <- head(vg[dt==max(vg$dt), .(Voile, Skipper, DTL)][order(DTL)],nbCoureurs)[,Voile]
  dateMax <- max(vg$dt)
  dateMin <- dateMax %m+% days(-nbJours)
  dateMaxLabel <- dateMax %m+% hours(18)
  if(variable!="DTL"){
    dataPlot <- vg[Voile %in% aSuivre & dt>dateMin]
    dataPlotLabel <- vg[Voile %in% aSuivre & dt==dateMax]
  }
  else{
    dataPlot <- vg[Voile %in% aSuivre & dt>dateMin & DTL!=0]
    dataPlotLabel <- vg[Voile %in% aSuivre & dt==dateMax & DTL!=0]
  }
  
  ggplot(dataPlot)+
    geom_line(aes_string(x="dt", y=variable, group="Skipper", colour="Skipper"))+
    geom_label_repel(data=dataPlotLabel, 
                     aes_string(label = "Skipper", colour = "Skipper", x = "dt", y = variable),size=3)+
    scale_x_datetime(breaks = date_breaks("1 day"), 
                     labels = date_format("%a %d %b", tz="CET"), 
                     limits=c(dateMin, dateMaxLabel),
                     expand = c(0,0))+
    #scale_fill_manual(values=ispfPalette)+
    #scale_colour_manual(values=ispfPalette)+
    xlab("")+
    theme_ispf()+
    theme(legend.position='none')
}
