# Referenciak
# https://www.statsdirect.com/help/survival_analysis/cox_regression.htm

# Szükséges csomag betöltése
library(dplyr)
library(corrplot)

# Segédfüggvények
zip <- function(...) {
  mapply(list, ..., SIMPLIFY = FALSE)
}

#FÜGGŐ VÁLTOZÓ
# Az "abs.time.diff" (t1-to, time-to-encounter) oszlop hozzáadása a táblázathoz

# f.df.diff1 <- f.df.en %>%
#   mutate(Datum = as.Date(Datum),  # "Datum" oszlop átalakítása dátumokká
#          Felvetel.kezdete = as.POSIXct(paste(Datum, Felvetel.kezdete), format="%Y-%m-%d %H:%M:%S"),
#          Felvetel.vege = as.POSIXct(paste(Datum, Felvetel.vege), format="%Y-%m-%d %H:%M:%S")) %>%
#   group_by(Helyszin, Start) %>%
#   mutate(abs.time.diff = as.numeric(difftime(Felvetel.kezdete, lag(Felvetel.vege), units = "secs")))

  
# f.df.diff2 <- f.df.en %>%
#   mutate(Datum = as.Date(Datum),  # "Datum" oszlop átalakítása dátumokká
#          Felvetel.kezdete = as.POSIXct(paste(Datum, Felvetel.kezdete), format="%Y-%m-%d %H:%M:%S"),
#          Felvetel.vege = as.POSIXct(paste(Datum, Felvetel.vege), format="%Y-%m-%d %H:%M:%S")) %>%
#   group_by(Helyszin, Start) %>%
#   mutate(abs.time.diff = as.numeric(difftime(Felvetel.vege, lag(Felvetel.kezdete), units = "secs") / 3600)) %>%
#   filter(Felvetel.tartalma != lag(Felvetel.tartalma, default = first(Felvetel.tartalma)))

# Adatok betoltese
f.df.en.backup  = read.csv('D:/PhD/PROJECTS/TempSpatialOverlap_2015-2019/R/Újragondolt/f_df_en.csv')
f.df.en = f.df.en.backup
f.df.en <- f.df.en[f.df.en$Felvetel.tartalma != "Meles meles", ]
f.df.en <- f.df.en[f.df.en$Felvetel.tartalma != "rider", ]
f.df.diff3 <- f.df.en %>%
  mutate(Datum = as.Date(Datum),  # "Datum" oszlop átalakítása dátumokká
         Felvetel.kezdete = as.POSIXct(paste(Datum, Felvetel.kezdete), format="%Y-%m-%d %H:%M:%S"),
         Felvetel.vege = as.POSIXct(paste(Datum, Felvetel.vege), format="%Y-%m-%d %H:%M:%S")) %>%
  group_by(Helyszin, Start) %>%
  arrange(Felvetel.kezdete) %>%
  mutate(abs.time.diff = ifelse((Felvetel.kezdete == lag(Felvetel.kezdete)) |
                                (Felvetel.vege == lag(Felvetel.vege)) |
                                ((Felvetel.kezdete > lag(Felvetel.kezdete)) & (Felvetel.kezdete < lag(Felvetel.vege))),
                                0.0,
                                as.numeric(difftime(Felvetel.kezdete, lag(Felvetel.vege), units = "secs") / 3600))) %>%
  filter(ifelse(is.na(abs.time.diff), TRUE, Felvetel.tartalma != lag(Felvetel.tartalma, default = first(Felvetel.tartalma))))

# head(as.data.frame(f.df.diff1), 10)
head(as.data.frame(f.df.diff3), 10)
#View(f.df.diff3[unique(unlist(zip(which(f.df.diff3$abs.time.diff < 0)-1, which(f.df.diff3$abs.time.diff < 0)))),])


#MAGYARÁZÓ VÁLTOZÓK
#Bükki kamerák táblázatának leszűrése
kamera_selected <- Bukk_kamerak_koordinatakGPSEOV[, c("Helyszin", "Elhelyz.Bukk", "Lat.GPS", "Lon.GPS", "Elevation..m.", "Uton")]

# Csak azokat a sorokat tartjuk meg a kamerás táblázatból, amelyek szerepelnek a f.df.diff2-ben a "Helyszin" oszlop alapján
kamera_filtered <- semi_join(kamera_selected, f.df.diff3, by = "Helyszin")

# Kiszűrjük azokat a sorokat, ahol a 'Lat.GPS' nem NA
kamera_filtered <- kamera_filtered %>%
  filter(!is.na(Lat.GPS))

# Az új df2_filtered táblázat megjelenítése
head(kamera_filtered)


# A táblázatok összekapcsolása a "Helyszin" oszlopon
merged_df <- left_join(f.df.diff3, kamera_filtered, by = "Helyszin")

# Az első pár sor megjelenítése
head(as.data.frame(merged_df))

library(survival)

# ÚJ TÁBLÁZAT A SZÜKSÉGESEKKEL
# Kiválogatjuk azokat az oszlopokat, amiket tartalmazni szeretnénk
new_table <- merged_df %>%
  select(Helyszin, Start, Felvetel.kezdete, Felvetel.vege, Felvetel.tartalma, abs.time.diff, Elevation..m., Uton) %>%
  # Új oszlop létrehozása az előző sor 'Felvetel.tartalma' tartalmával
  mutate(prev_Felvetel.tartalma = lag(Felvetel.tartalma),
         prev_Felvetel.vege = lag(Felvetel.vege))


#NA-k szűrése
new_table <- new_table %>% filter(!is.na(abs.time.diff))

#Interakció oszlop elkészítése
new_table <- new_table %>% mutate(Interakcio = paste(Felvetel.tartalma, prev_Felvetel.tartalma, sep = "_"))

# Az új táblázat megjelenítése
head(as.data.frame(new_table))

# Töröljük azokat a sorokat, ahol a t1-t0 (abs.time.diff) több mint 7 nap
# Átszámolás napokra (24 óra = 1 nap)
new_table <- mutate(new_table, abs.time.diff_days = abs.time.diff / 24)

# Szűrés azokra a sorokra, ahol az abs.time.diff_days nagyobb mint 7
new_table_filtered <- filter(new_table, abs.time.diff_days <= 7)

# Az új táblázat megjelenítése
head(as.data.frame(new_table_filtered))


#MEDIAN KINYERÉSE

# median_by_group <- new_table_filtered %>%
#   group_by(Interakcio) %>%
#   summarize(median_abs_time_diff = median(abs.time.diff, na.rm = TRUE))

new_table_filtered <- new_table_filtered %>%
  filter(abs.time.diff >= 0)

summary_by_group <- new_table_filtered %>%
  group_by(Interakcio) %>%
  summarize(
    count = n(),
    median_abs_time_diff = median(abs.time.diff, na.rm = TRUE),
    min_abs_time_diff = min(abs.time.diff, na.rm = TRUE),
    max_abs_time_diff = max(abs.time.diff, na.rm = TRUE),
    mean_abs_time_diff = mean(abs.time.diff, na.rm = TRUE)
  )

# Eredmény kiíratása

#print(median_by_group)
print(summary_by_group)

# Kiválasztás és szűrés az Interakcio oszlop alapján

# filtered_summary <- summary_by_group %>%
#   filter(Interakcio %in% c("Canis lupus_Capreolus capreolus", "Canis lupus_Cervus elaphus", "Canis lupus_Meles meles", "Canis lupus_Sus scrofa", "Canis lupus_Vulpes vulpes", "Sus scrofa_Canis lupus", "Capreolus capreolus_Canis lupus", "Cervus elaphus_Canis lupus", "Vulpes vulpes_Capreolus capreolus", "MV_Sus scrofa", "MV_Capreolus capreolus", "MV_Cervus elaphus", "MV_Canis lupus", "MV_Vulpes vulpes", "MV_Meles meles", "hiker_Sus scrofa", "hiker_Capreolus capreolus", "hiker_Cervus elaphus", "hiker_Canis lupus", "hiker_Vulpes vulpes", "hiker_Meles meles", "rider_Sus scrofa", "rider_Capreolus capreolus", "rider_Cervus elaphus", "rider_Canis lupus", "rider_Vulpes vulpes", "rider_Meles meles")) %>%
#   select(Interakcio, everything())

canis_summary <- summary_by_group %>%
  filter(Interakcio %in% c("Canis lupus_Capreolus capreolus", "Canis lupus_Cervus elaphus", "Canis lupus_Sus scrofa", "Canis lupus_Vulpes vulpes")) %>%
  select(Interakcio, everything())

ungulate_summary <- summary_by_group %>%
  filter(Interakcio %in% c("Sus scrofa_Canis lupus", "Capreolus capreolus_Canis lupus", "Cervus elaphus_Canis lupus")) %>%
  select(Interakcio, everything())

human_summary <- summary_by_group %>%
  filter(Interakcio %in% c("MV_Sus scrofa", "MV_Capreolus capreolus", "MV_Cervus elaphus", "MV_Canis lupus", "MV_Vulpes vulpes", "hiker_Sus scrofa", "hiker_Capreolus capreolus", "hiker_Cervus elaphus", "hiker_Canis lupus", "hiker_Vulpes vulpes")) %>%
  select(Interakcio, everything())

#Medián Ábrázolása
library(ggplot2)

# Adatok rendezése
human_summary$Interakcio<- factor(human_summary$Interakcio, levels = human_summary$Interakcio[order(human_summary$median_abs_time_diff)])
ungulate_summary$Interakcio<- factor(ungulate_summary$Interakcio, levels = ungulate_summary$Interakcio[order(ungulate_summary$median_abs_time_diff)])
canis_summary$Interakcio<- factor(canis_summary$Interakcio, levels = canis_summary$Interakcio[order(canis_summary$median_abs_time_diff)])

# Az ábrázolás
humanabra <- ggplot(human_summary, aes(x = Interakcio, y = median_abs_time_diff, fill = Interakcio)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = count), vjust = -0.5, position = position_dodge(width = 0.9)) +
  labs(x = "Interakcio", y = "Median abs_time_diff") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

humanabra

ungulateabra <- ggplot(ungulate_summary, aes(x = Interakcio, y = median_abs_time_diff, fill = Interakcio)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = count), vjust = -0.5, position = position_dodge(width = 0.9)) +
  labs(x = "Interakcio", y = "Median abs_time_diff") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ungulateabra

canisabra <- ggplot(canis_summary, aes(x = Interakcio, y = median_abs_time_diff, fill = Interakcio)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = count), vjust = -0.5, position = position_dodge(width = 0.9)) +
  labs(x = "Interakcio", y = "Median abs_time_diff") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

canisabra


# Az új táblázat megjelenítése
#print(filtered_summary)
print(canis_summary)


# Median értékek megjelenítése ~corrplot stílusban~
mx_median_abs_time_diff = matrix(0, nrow = 7, ncol = 7)
mx_median_abs_time_diff[lower.tri(mx_median_abs_time_diff) | upper.tri(mx_median_abs_time_diff)] = summary_by_group$median_abs_time_diff
colnames(mx_median_abs_time_diff) = c('Canis lupus', 'Capreolus capreolus', 'Cervus elaphus', 'MV', 'Sus scrofa', 'Vulpes vulpes', 'hiker')
rownames(mx_median_abs_time_diff) = c('Canis lupus', 'Capreolus capreolus', 'Cervus elaphus', 'MV', 'Sus scrofa', 'Vulpes vulpes', 'hiker')
mx_median_abs_time_diff = t(mx_median_abs_time_diff[c(7,4,1,6,2,3,5), c(7,4,1,6,2,3,5)])
diag(mx_median_abs_time_diff) = ceiling(max(mx_median_abs_time_diff))
corrplot(mx_median_abs_time_diff, method = 'square', addCoef.col = "black",
         cl.cex = 1.8, #tl.pos = 'n',
         is.corr = FALSE, col.lim = c(0,ceiling(max(mx_median_abs_time_diff))),
         col = colorRampPalette(c("darkred","white"))(100)) %>% corrRect(c(1,3,5,7), lwd=3)



#DENDROGRAM KÉSZÍTÉSE T1-T0 MEDIAN-RA

# Szükséges csomagok betöltése
library(ecodist)
library(ggplot2)
library(vegan)
library(smerc)

# Felosztjuk az 'Interakció' oszlop elemeit '_' karakterrel
split_interakcio <- strsplit(as.character(summary_by_group$Interakcio), "_")

# Kiválasztjuk az egyedi értékeket és a 'Uton_exp_coef' oszlop értékeit
unique_values <- unique(unlist(split_interakcio))

# Létrehozzuk a mátrixot
matrix_data_median <- matrix(NA, nrow = length(unique_values), ncol = length(unique_values), dimnames = list(unique_values, unique_values))

# Beállítjuk a mátrix elemeit a 7 nap - 'median_abs_time_diff' értékekkel
for (i in seq_along(split_interakcio)) {
  matrix_data_median[split_interakcio[[i]][1], split_interakcio[[i]][2]] <- 7*24 - as.numeric(summary_by_group$median_abs_time_diff[i])
}
diag(matrix_data_median) = 0.0

# Elbow módszerrel határozzuk meg az ideális klaszter számot
wss <- numeric(length = nrow(matrix_data_median))
for (i in 1:(nrow(matrix_data_median)-1)) {
  cat(i)
  kmeans_model <- kmeans(matrix_data_median, centers = i)
  wss[i] <- sum((matrix_data_median - kmeans_model$centers[kmeans_model$cluster,])^2)
}

# Megkeressük a könyök pontot
elbow_point <- elbow_point(x=1:length(wss),y=wss)

# Ábrázoljuk az inercia változását a klaszterek számának függvényében
plot(1:nrow(matrix_data_median), wss, type = "b", pch = 19, frame = FALSE, main = "Elbow Method",
     xlab = "Number of Clusters", ylab = "Within Sum of Squares")
elbow_point
points(elbow_point$x, elbow_point$y, col = "red", cex = 2, pch = 4)
text(elbow_point$x, elbow_point$y, labels = sprintf("   %d", elbow_point$idx), pos = 4)

# Optimális klaszter szám
optimal_clusters <- elbow_point$idx

# Klaszterezési fa létrehozása
distance_matrix <- as.dist(matrix_data_median)
hierarchical_clustering <- hclust(distance_matrix, method = "ward.D2")

# Színes klaszterek megjelenítése a dendrogramon
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
cols = gg_color_hue(optimal_clusters)
plot(hierarchical_clustering, main = "Dendrogram with Colored Clusters", xlab = "Elemek", sub = NULL)
rect.hclust(hierarchical_clustering, k = optimal_clusters, border = cols) #2:optimal_clusters)

# NMDS ábra frissítése az optimális klaszterek számával
nmds_result_optimal <- metaMDS(distance_matrix, k = 2)

# Klaszterek hozzáadása
nmds_result_optimal$clust <- cutree(hierarchical_clustering, k = optimal_clusters)

# Ábra készítése
g <- ggplot(data = data.frame(nmds_result_optimal$points, clust = factor(nmds_result_optimal$clust)),
            aes(x = nmds_result_optimal$points[, 1], y = nmds_result_optimal$points[, 2], color = clust, label = row.names(nmds_result_optimal$points))) +
  geom_point(size = 3) +
  geom_text(size = 3, hjust = 0, vjust = 0) +
  ggtitle("NMDS Plot with Cluster Coloring") +
  theme_minimal()
g








#COX MODELL
library(survival)

new_table_filtered$Uton <- factor(new_table_filtered$Uton)
new_table_filtered$Interakcio <- factor(new_table_filtered$Interakcio)
cox_model <- coxph(Surv(abs.time.diff) ~ Elevation..m. + Uton + cluster(Interakcio), data = new_table_filtered)
#cox_model <- coxph(Surv(abs.time.diff) ~ Elevation..m. + Uton + frailty(Helyszin) + cluster(Interakcio), data = new_table_filtered)
#A coxph nem alkalmas a cluster és frailty funkciók együttes használatára.
zph_test <- cox.zph(cox_model)





#BOOTSTREPPING (new_table_filtered táblázat kell!)




# COX MODELLEKBŐL KÉSZÍTETT KLASZTER ÉS NMDS ÁBRA

# Különböző Interakció értékekkel elkészítjük a Cox modelleket
cox_models <- list()
for (interakcio_level in levels(new_table_filtered$Interakcio)) {
  # Leszűrjük az adatokat az aktuális Interakció értékkel
  filtered_data <- subset(new_table_filtered, Interakcio == interakcio_level)
  
  # Elkészítjük a Cox modellt az aktuális csoportra
  formula_str <- "Surv(abs.time.diff) ~ Elevation..m. + Uton"
  cox_models[[interakcio_level]] <- coxph(as.formula(formula_str), data = filtered_data)
}

# Eredmények tárolására létrehozunk egy üres adatkeretet
result_df <- data.frame(Interakcio = character(), db = numeric(), Uton_exp_coef = numeric(), Elevation_exp_coef = numeric(), stringsAsFactors = FALSE)

interakcio_table = table(new_table_filtered$Interakcio)

# Végigmegyünk az összes modellen
for (interakcio_level in names(cox_models)) {
  # A modell kinyerése
  model <- cox_models[[interakcio_level]]
  
  # Az exp(coef) (HR) értékek kinyerése
  exp_coefs <- exp(coef(model))
  
  # Az eredmények hozzáadása az adatkerethez
  result_df <- rbind(result_df, c(Interakcio = interakcio_level, db = interakcio_table[interakcio_level], Uton_exp_coef = exp_coefs["UtonNem"], Elevation_exp_coef = exp_coefs["Elevation..m."]))
}
colnames(result_df) = c('Interakció', 'db', 'Uton_exp_coef', 'Elevation_exp_coef')
result_df$Uton_exp_coef[which(is.na(result_df$Uton_exp_coef))] = 1.0

# Felosztjuk az 'Interakció' oszlop elemeit '_' karakterrel
split_interakcio <- strsplit(as.character(result_df$Interakció), "_")

# Kiválasztjuk az egyedi értékeket és a 'Uton_exp_coef' oszlop értékeit
unique_values <- unique(unlist(split_interakcio))
uton_exp_coef <- result_df$Uton_exp_coef
elevation_exp_coef <- result_df$Elevation_exp_coef

# Létrehozzuk a mátrixot
matrix_data <- matrix(NA, nrow = length(unique_values), ncol = length(unique_values), dimnames = list(unique_values, unique_values))
matrix_data_elevation <- matrix_data

# Beállítjuk a mátrix elemeit a 'Uton_exp_coef' értékekkel
for (i in seq_along(split_interakcio)) {
  matrix_data[split_interakcio[[i]][1], split_interakcio[[i]][2]] <- as.numeric(uton_exp_coef[i])
  matrix_data_elevation[split_interakcio[[i]][1], split_interakcio[[i]][2]] <- as.numeric(elevation_exp_coef[i])
}
diag(matrix_data) = 0.0
diag(matrix_data_elevation) = 0.0

# Megjelenítjük a mátrixot
print(matrix_data)
print(matrix_data_elevation)

#Dendogram készítése

# Szükséges csomagok betöltése
library(ecodist)
library(ggplot2)
library(vegan)
library(smerc)

# Elbow módszerrel határozzuk meg az ideális klaszter számot
wss <- numeric(length = nrow(matrix_data))
for (i in 1:(nrow(matrix_data)-1)) {
  cat(i)
  kmeans_model <- kmeans(matrix_data, centers = i)
  wss[i] <- sum((matrix_data - kmeans_model$centers[kmeans_model$cluster,])^2)
}

# Megkeressük a könyök pontot
elbow_point <- elbow_point(x=1:length(wss),y=wss)

# Ábrázoljuk az inercia változását a klaszterek számának függvényében
plot(1:nrow(matrix_data), wss, type = "b", pch = 19, frame = FALSE, main = "Elbow Method",
     xlab = "Number of Clusters", ylab = "Within Sum of Squares")
elbow_point
points(elbow_point$x, elbow_point$y, col = "red", cex = 2, pch = 4)
text(elbow_point$x, elbow_point$y, labels = sprintf("   %d", elbow_point$idx), pos = 4)

# Optimális klaszter szám
optimal_clusters <- elbow_point$idx

# Klaszterezési fa létrehozása
distance_matrix <- as.dist(matrix_data)
hierarchical_clustering <- hclust(distance_matrix, method = "ward.D2")

# Színes klaszterek megjelenítése a dendrogramon
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
cols = gg_color_hue(optimal_clusters)
plot(hierarchical_clustering, main = "Dendrogram with Colored Clusters", xlab = "Elemek", sub = NULL)
rect.hclust(hierarchical_clustering, k = optimal_clusters, border = cols) #2:optimal_clusters)

# NMDS ábra frissítése az optimális klaszterek számával
nmds_result_optimal <- metaMDS(distance_matrix, k = 2)

# Klaszterek hozzáadása
nmds_result_optimal$clust <- cutree(hierarchical_clustering, k = optimal_clusters)

# Ábra készítése
g <- ggplot(data = data.frame(nmds_result_optimal$points, clust = factor(nmds_result_optimal$clust)),
            aes(x = nmds_result_optimal$points[, 1], y = nmds_result_optimal$points[, 2], color = clust, label = row.names(nmds_result_optimal$points))) +
  geom_point(size = 3) +
  geom_text(size = 3, hjust = 0, vjust = 0) +
  ggtitle("NMDS Plot with Cluster Coloring") +
  theme_minimal()
g

