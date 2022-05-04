library('stringr')

read.bnp.farkas.data = function( extended = FALSE, covid = 'pre' )
{
  
  farkasmappa <- 'SET/DIRECTORY/FOR/CODES/AND/DATA'
  #farkastablazat <- str_c(farkasmappa, '/Nagyragadozo_220403.csv')
  farkastablazat <- str_c(farkasmappa, '/Nagyragadozo_csv_aktualis.csv')
  
  #1. Adatok elokeszitese
  #1.a) tablazat beolvasas
  f.df <- read.csv(farkastablazat, sep = ';', stringsAsFactors = FALSE)
  
  f.df$Felvetel.kezdete[which(sapply(f.df$Felvetel.kezdete, function(a) {length(str_match_all(a,':')[[1]])}) == 1)] =
    paste0(f.df$Felvetel.kezdete[which(sapply(f.df$Felvetel.kezdete, function(a) {length(str_match_all(a,':')[[1]])}) == 1)], ':00')
  f.df$Felvetel.vege[which(sapply(f.df$Felvetel.vege, function(a) {length(str_match_all(a,':')[[1]])}) == 1)] =
    paste0(f.df$Felvetel.vege[which(sapply(f.df$Felvetel.vege, function(a) {length(str_match_all(a,':')[[1]])}) == 1)], ':00')
  
  f.df$Helyszin[f.df$Helyszin == "Istvan sir"] <- "Istvansir"
  f.df$Helyszin[f.df$Helyszin == "Harsas 2"] <- "Harsas02"
  f.df$Helyszin[f.df$Helyszin == "Feketesár"] <- "Feketesar"
  #f.df <- f.df[f.df$Helyszin != "Kajlaberc", ]
  f.df <- f.df[f.df$Helyszin != "Satai_rakodo_utan", ]
  f.df <- f.df[f.df$Helyszin != "Kacs", ]
  
  f.df$Datum = str_replace_all(f.df$Datum, '[.]', '-')
  f.df$Start = str_replace_all(f.df$Start, '[.]', '-')
  f.df$Stop = str_replace_all(f.df$Stop, '[.]', '-')
  
  f.df <- f.df[f.df$Keszitette != "Domboroczky Tibor", ]
  f.df <- f.df[f.df$Keszitette != "Nemes Krisztián", ]
  
  f.df$Felvetel.tartalma[f.df$Felvetel.tartalma == "canis lupus"] <- "Canis lupus"
  f.df$Felvetel.tartalma[f.df$Felvetel.tartalma == "sus scrofa"] <- "Sus scrofa"
  f.df$Felvetel.tartalma[f.df$Felvetel.tartalma == "cervus elaphus"] <- "Cervus elaphus"
  f.df$Felvetel.tartalma[f.df$Felvetel.tartalma == "capreolus capreolus"] <- "Capreolus capreolus"
  f.df$Felvetel.tartalma[f.df$Felvetel.tartalma == "vulpes vulpes"] <- "Vulpes vulpes"
  f.df$Felvetel.tartalma[f.df$Felvetel.tartalma == "Lepus europeus"] <- "Lepus europaeus"
  f.df$Felvetel.tartalma[f.df$Felvetel.tartalma == "Lepus europeaus"] <- "Lepus europaeus"
  f.df$Felvetel.tartalma[f.df$Felvetel.tartalma == "nyúl"] <- "Lepus europaeus"
  f.df$Felvetel.tartalma[f.df$Felvetel.tartalma == "Vadasz"] <- "vadasz"
  f.df$Felvetel.tartalma[f.df$Felvetel.tartalma == "ember"] <- "Ember"
  f.df$Felvetel.tartalma[f.df$Felvetel.tartalma == "Ember"] <- "gyalogos"
  f.df$Felvetel.tartalma[f.df$Felvetel.tartalma == "vadasz"] <- "gyalogos"
  f.df$Felvetel.tartalma[f.df$Felvetel.tartalma == "kamion"] <- "Kamion"
  f.df$Felvetel.tartalma[f.df$Felvetel.tartalma == "Kamion"] <- "MMJ"
  f.df$Felvetel.tartalma[f.df$Felvetel.tartalma == "auto"] <- "MMJ"
  f.df$Felvetel.tartalma[f.df$Felvetel.tartalma == "motoros"] <- "MMJ"
  f.df$Felvetel.tartalma[f.df$Felvetel.tartalma == "traktor"] <- "MMJ"
  f.df$Felvetel.tartalma[f.df$Felvetel.tartalma == "biciklis"] <- "bicikli/lovas"
  f.df$Felvetel.tartalma[f.df$Felvetel.tartalma == "lovaskocsi"] <- "bicikli/lovas"
  f.df$Felvetel.tartalma[f.df$Felvetel.tartalma == "lovas"] <- "bicikli/lovas"
  f.df$Felvetel.tartalma[f.df$Felvetel.tartalma == "fakitermeles"] <- "erdomunka"
  
  if (!extended)
  {
    f.df <- f.df[f.df$Felvetel.tartalma != "Lepus europaeus", ]
    #f.df <- f.df[f.df$Felvetel.tartalma != "Felis silvestris", ]
    #f.df <- f.df[f.df$Felvetel.tartalma != "Meles meles", ]
    f.df <- f.df[f.df$Felvetel.tartalma != "Martes foina", ]
    f.df <- f.df[f.df$Felvetel.tartalma != "Martes", ]
    f.df <- f.df[f.df$Felvetel.tartalma != "Ovis orientalis", ]
    f.df <- f.df[f.df$Felvetel.tartalma != "Sciurus vulgaris", ]
  }
  f.df <- f.df[f.df$Felvetel.tartalma != "kutya", ]
  f.df <- f.df[f.df$Felvetel.tartalma != "indet.", ]
  f.df <- f.df[f.df$Felvetel.tartalma != "Indet.", ]
  f.df <- f.df[f.df$Felvetel.tartalma != "Inder", ]
  f.df <- f.df[f.df$Felvetel.tartalma != "fakitermeles", ]
  f.df <- f.df[f.df$Felvetel.tartalma != "erdomunka", ]
  f.df <- f.df[f.df$Felvetel.tartalma != "Ovis aries", ]
  f.df <- f.df[f.df$Felvetel.tartalma != "Lynx lynx", ]
  f.df <- f.df[f.df$Felvetel.tartalma != "Bos taurus taurus", ]
  f.df <- f.df[f.df$Felvetel.tartalma != "", ]
  
  f.df$Felvetel.tartalma2 = f.df$Felvetel.tartalma
  f.df$Felvetel.tartalma2[f.df$Felvetel.tartalma2 == "gyalogos"] <- "emberi zavarás"
  f.df$Felvetel.tartalma2[f.df$Felvetel.tartalma2 == "MMJ"] <- "emberi zavarás"
  f.df$Felvetel.tartalma2[f.df$Felvetel.tartalma2 == "nyerges"] <- "emberi zavarás"
  f.df$Felvetel.tartalma2[f.df$Felvetel.tartalma2 == "erdomunka"] <- "emberi zavarás"
  
  f.df$Felvetel.tartalma3 = f.df$Felvetel.tartalma2
  f.df$Felvetel.tartalma3[f.df$Felvetel.tartalma3 == "Capreolus capreolus"] <- "nagyvad"
  f.df$Felvetel.tartalma3[f.df$Felvetel.tartalma3 == "Cervus elaphus"] <- "nagyvad"
  f.df$Felvetel.tartalma3[f.df$Felvetel.tartalma3 == "Sus scrofa"] <- "nagyvad"
  
  
  #1.b) datum es ido atalakitasa
  #Következõ a dátumok és idõk átkódolása. Az idõket az éjfélig eltelt másodpercekkel mérjük, a dátumok Date objektumokká lesznek alakítva.
  d <- as.Date(as.character(f.df$Datum))
  f.df$Datum <- d
  d <- as.Date(as.character(f.df$Start))
  f.df$Start <- d
  d <- as.Date(as.character(f.df$Stop))
  f.df$Stop <- d
  
  #Kiszámoljuk, hogy az adott esemény esetén hány napig mûködött a kamera:
  d <- f.df$Stop - f.df$Start
  
  #Covid-19 felosztas
  if (covid == 'pre')
    f.df = f.df[which(f.df$Stop < as.Date("2020-01-18")), ]
  else if (covid == 'post')
    f.df = f.df[which(f.df$Stop > as.Date("2020-01-17")), ]
  else
    f.df = f.df
  
  #Az idõk átalakítása:
  ido <- strsplit(as.character(f.df$Felvetel.kezdete), ":")
  ido <- sapply(ido, function(y) as.numeric(y[1])*3600 + as.numeric(y[2])*60 + as.numeric(y[3]))
  f.df$ErkIdo <- ido
  ido <- strsplit(as.character(f.df$Felvetel.vege), ":")
  ido <- sapply(ido, function(y) as.numeric(y[1])*3600 + as.numeric(y[2])*60 + as.numeric(y[3]))
  f.df$TavIdo <- ido
  f.df$Tart <- f.df$TavIdo - f.df$ErkIdo
  f.df$Felvetel.idotartama <- NULL
  
  #Néhány esetben a tartozkódási idõ átnyúlt az éjfélen, itt korrigálnunk kell.
  i <- !is.na(f.df$Tart) & f.df$Tart < 0
  f.df$Tart[i] <- 24*3600 - f.df$ErkIdo[i] + f.df$TavIdo[i]
  
  #Szintén számolunk egy abszolút idõt is. Ez 2015-01-01 óta eltelt másodpercek száma.
  time.origin <- as.Date("2015-01-01")
  f.df$abs.time <- as.numeric(f.df$Datum - time.origin) * 24*3600 + f.df$ErkIdo
  
  #1.c) Táblázat javítása
  #Két megjegyzés oszlopunk van, összevonom a kettõt.
  m <- paste(f.df$Megjegyz., ":", f.df$Megjegyz..1)
  m[m == " : "] <- NA
  f.df$Megjegyzes <- m
  f.df$Megjegyz. <- NULL
  f.df$Megjegyz..1 <- NULL
  
  #Az adattábla több oszlopot tartalmaz, amit valószínûleg nem fogunk használni. Az egyszerûség kedvéért törlöm ezeket.
  f.df$Megjegyzes <- NULL
  f.df$Kep <- NULL
  f.df$Irta <- NULL
  
  return(f.df)
}


#nincs benne a repository-ban
read.cserkesz.farkas.data = function()
{
  
  farkasmappa <- 'SET/DIRECTORY/FOR/CODES/AND/DATA'
  farkastablazat <- str_c(farkasmappa, '/kamerafelvetelek_filedata2021_BUKK_20211013.csv')
  
  #1. Adatok elokeszitese
  #1.a) tablazat beolvasas
  f.df <- read.csv(farkastablazat, sep = ';', stringsAsFactors = FALSE)
  
  f.df$Date = paste0(f.df$Date, ':00')
  
  f.df$Location.2[f.df$Location.2 == ""] <- "-"
  
  f.df$Species[f.df$Species == "szarvas"] <- "Cervus elaphus"
  f.df$Species[f.df$Species == "oz"] <- "Capreolus capreolus"
  f.df$Species[f.df$Species == "capreolus capreolus"] <- "Capreolus capreolus"
  f.df$Species[f.df$Species == "diszno"] <- "Sus scrofa"
  f.df$Species[f.df$Species == "disznó"] <- "Sus scrofa"
  f.df$Species[f.df$Species == "róka"] <- "Vulpes vulpes"
  f.df$Species[f.df$Species == "roka"] <- "Vulpes vulpes"
  f.df$Species[f.df$Species == "farkas"] <- "Canis lupus"
  f.df$Species[f.df$Species == "vadmacska"] <- "Felis silvestris"
  f.df$Species[f.df$Species == "borz"] <- "Meles meles"
  f.df$Species[f.df$Species == "mi"] <- "Human disturbance"
  f.df$Species[f.df$Species == "human"] <- "Human disturbance"
  f.df$Species[f.df$Species == "én"] <- "Human disturbance"
  f.df$Species[f.df$Species == "ember"] <- "Human disturbance"
  f.df$Species[f.df$Species == "traki"] <- "Human disturbance"
  f.df$Species[f.df$Species == "motor"] <- "Human disturbance"
  
  f.df$Species[f.df$Species == "Autó"] <- "Human disturbance"
  f.df$Species[f.df$Species == "Biciklis"] <- "Human disturbance"
  f.df$Species[f.df$Species == "Ember"] <- "Human disturbance"
  f.df$Species[f.df$Species == "Motoros"] <- "Human disturbance"
  f.df$Species[f.df$Species == "Traktor"] <- "Human disturbance"
  
  f.df <- f.df[f.df$Species %in% c('Sus scrofa', 'Capreolus capreolus', 'Cervus elaphus', 'Vulpes vulpes', 'Canis lupus', 'Human disturbance', 'Felis silvestris', 'Meles meles'), ]
  f.df$Time = str_split_fixed(f.df$Date, pattern = ' ', n = 2)[,2]
  f.df$Datum = str_replace_all(str_split_fixed(f.df$Date, pattern = ' ', n = 2)[,1], '[.]', '-')
  
  f.cs <- data.frame(Helyszin = f.df$Location.2,
                     Date = as.POSIXct(str_replace_all(f.df$Date, '[.]', '-')),
                     Datum = f.df$Datum,
                     Felvetel.kezdete = f.df$Time,
                     Felvetel.tartalma = f.df$Species,
                     stringsAsFactors = FALSE)
  
  f = NULL
  for ( h in unique(f.cs$Helyszin) )
  {
    d = subset(f.cs, Helyszin == h)
    i = c(1)
    m = dim(d)[1]
    n = 1
    k = 1
    while ( k + n <= m )
    {
      if ( d$Felvetel.tartalma[k+n] == d$Felvetel.tartalma[k] )
      {
        if ( as.numeric( difftime(d$Date[k+n], d$Date[k], units = 'min') ) < 15 )
        {
          n = n+1
        }
        else
        {
          i = c(i, k+n)
          k = k+n
          n = 1
        }
      }
      else
      {
        i = c(i, k+n)
        k = k+n
        n = 1
      }
    }
    f = rbind(f, d[i,])
  }
  
  f$Date = NULL
  
  return(f)
  
}


#nincs benne a repository-ban
read.weather.lunar.data = function()
{
  
  farkasmappa <- 'SET/DIRECTORY/FOR/CODES/AND/DATA'
  idojarasmappa <- str_c(farkasmappa, '/weather')
  
  weather <- read.csv(str_c(idojarasmappa, '/52733_20150101_20180930.csv'), stringsAsFactors = FALSE)
  weather2 <- read.csv(str_c(idojarasmappa, '/52744_20150101_20180930.csv'), stringsAsFactors = FALSE)
  weather <- merge(weather, weather2, by = 'Date')
  lunar <- read.csv(str_c(idojarasmappa, '/lunar.csv'), stringsAsFactors = FALSE)
  colnames(lunar)[-1] <- str_c('lunar.', colnames(lunar))[-1]
  weather <- merge(weather, lunar, by = 'Date')
  weather$Site.x <- NULL
  weather$Site.y <- NULL
  weather$Date <- as.Date(weather$Date)
  
  return(weather)
  
}
