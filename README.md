# bnpi_wolf_camtrap
(EN) Scripts and data to reproduce camera trapping analysis conducted by the "EvoZool" Department of Evolutionary Zoology, University of Debrecen based on camera trapping recordings by the Directorate of Bükk National Park.

(HU) Kameracsapdas elemzesek a DE "EvoZool" tanszekrol a BNPI felvetelei alapjan


## English guide

### Data
<b>ver7_Nagyragadozo_csv_220516.csv</b>

### Reproducing temporal overlap analysis
Use the script <b>FARKASSCRIPT_[yyyymmdd].R</b>

### Reproducing co-occurrence analysis
Look for the code in the <b>nagyragadozok-v2.Rmd</b> markdown document

## Hungarian guide
## Hasznalat
### 1. Allitsd be a letoltott repozitorium eleresi utjat
a FARKASSCRIPT_\*.R fajlban, es a FARKASSCRIPT_\*_datainput.R fuggvenyeiben

### 2. Allitsd be a kimeneti mappat
a FARKASSCRIPT_\*.R fajlban a 'figdir' valtozot

### 3. Elemzes futtatasa
Futtasd a FARKASSCRIPT_\*.R szkriptet. A beallitott 'figdir' mappaba keszulnek el az eredmenyek.

## Megjegyzesek
Az adatok csak a BNPI felvetelei alapjan keszitett tablazatot tartalmazzak,
egyeb adathalmazt, valamint meteorologiai adatokat nem.
