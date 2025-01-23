library(stringr)

# Setting paths
farkasmappa <- 'D:/Work/bnpi_wolf_camtrap/' # Folder of the cloned Git repository
#idojarasmappa <- str_c(farkasmappa, '/weather')
figdir <- str_c(farkasmappa, '/figures_manuscript_20250123/')

# Sourcing scripts
#source( paste0(farkasmappa, 'FARKASSCRIPT_20230226_datainput.R') )
#source( paste0(farkasmappa, 'FARKASSCRIPT_20220421_sectioning.R') )
source( paste0(farkasmappa, 'FARKASSCRIPT_20231106_overlap.R') )
#source( paste0(farkasmappa, 'FARKASSCRIPT_20220421_regrtree.R') )
source( paste0(farkasmappa, 'FARKASSCRIPT_20220421_functions.R') )

# Create output dirs
create.figdirs( )

# Load table and do final filtering (TODO: incorporate this in datainput script)
f.df.en.sample = read.csv(paste0(farkasmappa, '/sample/f_df_en_60_sample.csv'))

# # Trapping Gantt chart
draw.bnpi.gantt.chart( f.df.en.sample, use.sample = TRUE )

# Overlap analysis
#outputname.postfix = ""
outputname.postfix = "merged"
merge.human.disturbance = TRUE
merge.games = TRUE
olap.matrix = overlap.analysis.bnpi( f.df.en.sample, outputname.postfix, merge.human.disturbance, merge.games )

