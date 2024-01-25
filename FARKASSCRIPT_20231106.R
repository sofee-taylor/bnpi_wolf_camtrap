library(stringr)

# Setting paths
farkasmappa <- 'D:/PhD/PROJECTS/TempSpatialOverlap_2015-2019/R/'
#idojarasmappa <- str_c(farkasmappa, '/weather')
figdir <- str_c(farkasmappa, '/figures_manuscript_20231106/')

# Sourcing scripts
source( paste0(farkasmappa, 'FARKASSCRIPT_20230226_datainput.R') )
source( paste0(farkasmappa, 'FARKASSCRIPT_20220421_sectioning.R') )
source( paste0(farkasmappa, 'FARKASSCRIPT_20231106_overlap.R') )
source( paste0(farkasmappa, 'FARKASSCRIPT_20220421_regrtree.R') )
source( paste0(farkasmappa, 'FARKASSCRIPT_20220421_functions.R') )

# Create output dirs
create.figdirs( )

# Specialis idoszakok kijelolese
summer.months = c(4, 5, 6, 7, 8, 9)
winter.months = c(1, 2, 3, 10, 11, 12)
full.year = NULL

# Input data
extended.data = FALSE # az elemzesek nincsenek beallitva a plusz fajokkal kibovitett tablazatra
months.data = full.year #summer.months, winter.months, full.year, or custom
covid.data = 'none' # pre, vagy post, amit a 2020-01-17 datum valaszt el. barmi mas eseten a teljes adathalmazt olvassa be
human.density = 'none' #'none' is for all data, 'high' for high human density and 'low' for low human density areas 
f.df = read.bnp.farkas.data( farkasmappa = farkasmappa, extended = extended.data, hum.den = human.density, data.months = months.data, covid = covid.data )
##weather = read.weather.lunar.data()
##f.df <- merge(f.df, weather, by.x = 'Datum', by.y = 'Date')

# Survey elemzes - csak a bovitett adathalmazra van tesztelve
if (extended.data) f.summ = summarize.monthly.surveys( f.df )

# # Forming continuous SECTIONS
# napokat.becsuld = TRUE
# becsles.modszer = 'count' # length, vagy count, attol fuggoen, hogy minek a varianciajat minimalizaljuk
# napok           = 14 # manualisan megadott szakaszhossz, ignoraljuk, ha a becsles TRUE
# legrovidebb     = 7  # ennel rovidebb szakaszokat eldobjuk, ha ez NA, vagy negativ, akkor nincs szures
#
# f.df = sectioning( f.df, autolength = napokat.becsuld, automethod = becsles.modszer, napok = napok, minnapok = legrovidebb )

# Translating to english (currently only used for overlap functions)
f.df.en = english.translation( f.df )

# # Trapping Gantt chart
# draw.bnpi.gantt.chart( f.df )

# Overlap analysis
outputname.postfix = ""
olap.matrix = overlap.analysis.bnpi( f.df.en, outputname.postfix )

# # Regression tree analysis
# measure.method = 'perc' # daily, percentage, count
# standard       = FALSE
# 
# #tbl = measure.trappings.per.section.and.transform( f.df, method = measure.method, standardize = standard )
# #Az alabbi elemzesben a celvaltozo a szakaszokban eszlelt darabszama az egyes fajoknak, a magyarazo valtozok pedig a tobbi faj napi rataja
# tbl.target = measure.trappings.per.section.and.transform( f.df, method = 'count', standardize = FALSE )
# tbl.variables = measure.trappings.per.section.and.transform( f.df, method = 'daily', standardize = FALSE )
# 
# crossmeasure = regression.tree.crossvalidate( tbl.var = tbl.variables, tbl.tar = tbl.target )
# regression.tree.analysis( tbl.var = tbl.variables, tbl.tar = tbl.target )
