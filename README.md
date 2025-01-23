# bnpi_wolf_camtrap
(EN) Scripts and data to reproduce camera trapping analysis conducted by the "EvoZool" Department of Evolutionary Zoology, University of Debrecen based on camera trapping recordings by the Directorate of Bükk National Park.

(HU) Kameracsapdas elemzesek a DE "EvoZool" tanszekrol a BNPI felvetelei alapjan


## Publications
Szabó, Zs., Gombkötő, P., Aranyi, S.C., Patkó, L., Patkó, D. & Barta, Z. \
Camera trapping of forest mammals in Bükk Mountain, Hungary. \
COMMUNITY ECOLOGY ([2024](https://doi.org/10.1007/s42974-024-00225-2)).


## Guide

This repository contains cleaned sample data and analysis scripts that produced results shown in <i>Szabó et al. (2024)</i>.

### Data sample
<b>sample/f_df_en_60_sample.csv</b>

Although the complete data processing methods are shared in the FARKASSCRIPT_[yyyymmdd]_datainput.R script, raw tabular data, camera trapping images, and meteorological information are not included in the repository.

### Reproducing temporal overlap analysis
Use the script <b>FARKASSCRIPT_[yyyymmdd]_sample.R</b> as the starting point.

For detailed implementation see the FARKASSCRIPT_[yyyymmdd]_overlap.R script. Function <i>overlap.analysis.bnpi()</i> makes conditional data processing, then calls worker functions <i>overlap.analysis.mx()</i> and <i>nmds.analysis</i>.

### Reproducing Multi-state modeling (MSM) analysis
Look for the code in the "Grouping by 60 minutes" section of the <b>encounter.qmd</b> markdown document. Note, that MSM modeling can take hours, or even days (!!!) for larger datasets.
