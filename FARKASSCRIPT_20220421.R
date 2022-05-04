library(stringr)

# Setting paths
farkasmappa <- 'SET/DIRECTORY/FOR/CODES/AND/DATA'
#farkastablazat <- str_c(farkasmappa, '/Nagyragadozo_220403.csv')
idojarasmappa <- str_c(farkasmappa, '/weather')
figdir <- str_c(farkasmappa, '/figures_manuscript_20220503/')

# Sourcing scripts
source( paste0(farkasmappa, 'FARKASSCRIPT_20220421_datainput.R') )
source( paste0(farkasmappa, 'FARKASSCRIPT_20220421_sectioning.R') )
source( paste0(farkasmappa, 'FARKASSCRIPT_20220421_overlap.R') )
source( paste0(farkasmappa, 'FARKASSCRIPT_20220421_regrtree.R') )
source( paste0(farkasmappa, 'FARKASSCRIPT_20220421_functions.R') )

# Create output dirs
create.figdirs( )

# Input data
extended.data = FALSE # az elemzesek nincsenek beallitva a plusz fajokkal kibovitett tablazatra
covid.data = 'pre' # pre, vagy post, amit a 2020-01-17 datum valaszt el. barmi mas eseten a teljes adathalmazt olvassa be
f.df = read.bnp.farkas.data( extended = extended.data, covid = 'pre' )
##weather = read.weather.lunar.data()
##f.df <- merge(f.df, weather, by.x = 'Datum', by.y = 'Date')

# Survey elemzes - csak a bovitett adathalmazra van tesztelve
if (extended.data) f.summ = summarize.monthly.surveys( f.df )

# Specialis idoszakok kijelolese
f.df$tavasz <- sapply(f.df$Datum, function(y) as.numeric(str_split(as.character(y), '-')[[1]][2]) %in% c(3))
f.df$nyar <- sapply(f.df$Datum, function(y) as.numeric(str_split(as.character(y), '-')[[1]][2]) %in% 3:8)
f.df$vadaszideny <- sapply(f.df$Datum, function(y) as.numeric(str_split(as.character(y), '-')[[1]][2]) %in% c(1, 2, 9, 10, 11, 12))

# Forming continuous SECTIONS
napokat.becsuld = TRUE
becsles.modszer = 'count' # length, vagy count, attol fuggoen, hogy minek a varianciajat minimalizaljuk
napok           = 14 # manualisan megadott szakaszhossz, ignoraljuk, ha a becsles TRUE
legrovidebb     = 7  # ennel rovidebb szakaszokat eldobjuk, ha ez NA, vagy negativ, akkor nincs szures

f.df = sectioning( f.df, autolength = napokat.becsuld, automethod = becsles.modszer, napok = napok, minnapok = legrovidebb )

# Translating to english (currently only used for overlap functions)
f.df.en = english.translation( f.df )

# Trapping Gantt chart
draw.bnpi.gantt.chart( f.df )

# Overlap analysis
overlap.analysis.bnpi( f.df.en )

# Regression tree analysis
measure.method = 'perc' # daily, percentage, count
standard       = FALSE

#tbl = measure.trappings.per.section.and.transform( f.df, method = measure.method, standardize = standard )
#Az alabbi elemzesben a celvaltozo a szakaszokban eszlelt darabszama az egyes fajoknak, a magyarazo valtozok pedig a tobbi faj napi rataja
tbl.target = measure.trappings.per.section.and.transform( f.df, method = 'count', standardize = FALSE )
tbl.variables = measure.trappings.per.section.and.transform( f.df, method = 'daily', standardize = FALSE )

crossmeasure = regression.tree.crossvalidate( tbl.var = tbl.variables, tbl.tar = tbl.target )
regression.tree.analysis( tbl.var = tbl.variables, tbl.tar = tbl.target )
