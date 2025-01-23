library(stringr)


#figdir.hca   = str_c(figdir, '/hca/')
figdir.gantt = str_c(figdir, '/gantt/')
figdir.olap  = str_c(figdir, '/overlap/')
figdir.tree  = str_c(figdir, '/regr_tree/')
figdir.surv  = str_c(figdir, '/survey/')

create.figdirs = function(  )
{
  #dir.create( figdir.hca, showWarnings = FALSE, recursive = TRUE )
  dir.create( figdir.gantt, showWarnings = FALSE, recursive = TRUE )
  dir.create( figdir.olap, showWarnings = FALSE, recursive = TRUE )
  dir.create( figdir.tree, showWarnings = FALSE, recursive = TRUE )
  dir.create( figdir.surv, showWarnings = FALSE, recursive = TRUE )
}

english.translation = function( f.df )
{
  f.df$Felvetel.tartalma[f.df$Felvetel.tartalma == "bicikli/lovas"] <- "rider"
  f.df$Felvetel.tartalma[f.df$Felvetel.tartalma == "MMJ"] <- "MV"
  f.df$Felvetel.tartalma[f.df$Felvetel.tartalma == "gyalogos"] <- "hiker"
  
  return(f.df)
}

summarize.monthly.surveys = function( f.df )
{
  helyszinek = unique(f.df$Helyszin)
  
  first.month = format(min(f.df$Datum), "%Y-%m")
  last.month  = format(max(f.df$Datum), "%Y-%m")
  all.months  = seq.Date( as.Date(paste0(first.month, "-01")), as.Date(paste0(last.month, "-01")), by = "month" )
  
  origin = "1970-01-01"
  
  same.month = function( d1, d2, origin = "1970-01-01" )
  {
    format(as.Date(d1, origin), "%Y-%m") == format(as.Date(d2, origin), "%Y-%m")
  }
  
  summ = data.frame( Date = character(0), ID = character(0), Survey = integer(0), BTS = integer(0),
                     cap.det.sz = integer(0), cer.det.sz = integer(0), sus.det.sz = integer(0), vul.det.sz = integer(0), ov.det.sz = integer(0),
                     mel.det.sz = integer(0), fel.det.sz = integer(0), lep.det.sz = integer(0), mar.det.sz = integer(0), sci.det.sz = integer(0),
                     all.det.sz = integer(0))
  
  for (h in helyszinek)
  {
    f.h = subset(f.df, Helyszin == h)
    for (m in all.months)
    {
      m = as.Date(m, origin)
      f.m = f.h[which( same.month(f.h$Datum, m) ), ]
      if ( dim(f.m)[1] == 0 )
      {
        summ = rbind(summ,
                     data.frame(Date = format(m, "%Y-%m"), Site = h, Survey = NA, BTS = NA,
                                cap.det.sz = NA,
                                cer.det.sz = NA,
                                sus.det.sz = NA,
                                vul.det.sz = NA,
                                ov.det.sz  = NA,
                                mel.det.sz = NA,
                                fel.det.sz = NA,
                                lep.det.sz = NA,
                                mar.det.sz = NA,
                                sci.det.sz = NA,
                                all.det.sz = NA ) )
      }
      else
      {
        
        surveys = 0
        for (s in unique(f.m$Stop))
        {
          if ( same.month(s, m) )
            surveys = surveys + 1
        }
        cap.det.sz = length(which(f.m$Felvetel.tartalma == "Capreolus capreolus"))
        cer.det.sz = length(which(f.m$Felvetel.tartalma == "Cervus elaphus"))
        sus.det.sz = length(which(f.m$Felvetel.tartalma == "Sus scrofa"))
        vul.det.sz = length(which(f.m$Felvetel.tartalma == "Vulpes vulpes"))
        ov.det.sz  = length(which(f.m$Felvetel.tartalma == "Ovis orientalis"))
        mel.det.sz = length(which(f.m$Felvetel.tartalma == "Meles meles"))
        fel.det.sz = length(which(f.m$Felvetel.tartalma == "Felis silvestris"))
        lep.det.sz = length(which(f.m$Felvetel.tartalma == "Lepus europaeus"))
        mar.det.sz = length(which(f.m$Felvetel.tartalma == "Martes foina")) + length(which(f.m$Felvetel.tartalma == "Martes"))
        sci.det.sz = length(which(f.m$Felvetel.tartalma == "Sciurus vulgaris"))
        all.det.sz = cap.det.sz + cer.det.sz + sus.det.sz + vul.det.sz + ov.det.sz + mel.det.sz + fel.det.sz + lep.det.sz + mar.det.sz + sci.det.sz
        summ = rbind(summ,
                     data.frame(Date = format(m, "%Y-%m"), Site = h, Survey = surveys, BTS = as.numeric(as.logical( length(which(f.m$Felvetel.tartalma == "Canis lupus")) )),
                                cap.det.sz = cap.det.sz,
                                cer.det.sz = cer.det.sz,
                                sus.det.sz = sus.det.sz,
                                vul.det.sz = vul.det.sz,
                                ov.det.sz  = ov.det.sz,
                                mel.det.sz = mel.det.sz,
                                fel.det.sz = fel.det.sz,
                                lep.det.sz = lep.det.sz,
                                mar.det.sz = mar.det.sz,
                                sci.det.sz = sci.det.sz,
                                all.det.sz = all.det.sz ) )
      }
    }
  }
  
  write.csv2(summ, paste0(figdir.surv, '/monthly_survey_info.csv'), row.names = FALSE)
  
  return(summ)
}

draw.bnpi.gantt.chart = function( f.df, use.sample=TRUE )
{
  library(ggplot2)
  
  f.df1 = f.df[,-which(colnames(f.df) =='Stop')]
  f.df1$state = 'Start'
  colnames(f.df1)[which(colnames(f.df1)=='Start')] = 'date'
  
  f.df2 = f.df[,-which(colnames(f.df) =='Start')]
  f.df2$state = 'Stop'
  colnames(f.df2)[which(colnames(f.df2)=='Stop')] = 'date'
  
  f.df.gantt = rbind(f.df1, f.df2)
  f.df.gantt = f.df.gantt[order(f.df.gantt$abs.time),]
  
  f.df.gantt$date = as.Date(f.df.gantt$date)
  
  if (use.sample){
    camorder = c('Site_1', 'Site_2', 'Site_3')
  } else{
    camorder = unique(f.df.gantt$Helyszin)
    camorder = camorder[!(camorder %in% c('Komlei_kanyar', 'Satai'))]
    camorder = c(camorder[1:which(camorder == 'Komlei')], 'Komlei_kanyar', camorder[(which(camorder == 'Komlei')+1):length(camorder)] )
    camorder = c(camorder[1:which(camorder == 'Satai_dagonya')], 'Satai', camorder[(which(camorder == 'Satai_dagonya')+1):length(camorder)] )
  }
  
  f.df.gantt$Helyszin = factor(f.df.gantt$Helyszin, levels = rev(camorder))
  
  #, color=Helyszin, group=szakaszID
  g.gantt = ggplot(f.df.gantt, aes(date, Helyszin)) + geom_line(linewidth=6) +
    #labs(x=NULL, y=NULL) + xlim(c(as.Date("2015-01-01"), as.Date("2020-01-01"))) +
    labs(x=NULL, y=NULL) + xlim(c(as.Date(min(f.df$Start)), as.Date(max(f.df$Stop)))) +
    scale_y_discrete(labels = str_c('Site', rev(1:length(camorder)), sep = '_')) +
    theme(legend.position = "none", axis.text.x = element_text(size=12, face = "bold"), axis.text.y = element_text(size=12, face = "bold"))
  
  ggsave( paste0(figdir.gantt, 'bnpi_site_activity_gantt.tiff'), width = 8, height = 6, dpi = 300, units = 'in', compression='lzw')
  
  g.gantt = ggplot(f.df.gantt, aes(date, Helyszin)) + geom_line(linewidth=6) +
    #labs(x=NULL, y=NULL) + xlim(c(as.Date("2015-01-01"), as.Date("2020-01-01"))) +
    labs(x=NULL, y=NULL) + xlim(c(as.Date(min(f.df$Start)), as.Date(max(f.df$Stop)))) +
    #scale_y_discrete(labels = str_c('Site', rev(1:length(camorder)), sep = '_')) +
    theme(legend.position = "none", axis.text.x = element_text(size=12, face = "bold"), axis.text.y = element_text(size=12, face = "bold"))

  ggsave( paste0(figdir.gantt, 'bnpi_site_activity_gantt_sites.tiff'), width = 8, height = 6, dpi = 300, units = 'in', compression='lzw')

  rm(f.df1)
  rm(f.df2)
  rm(f.df.gantt)
}

cooccurency_HCA <- function(tablazat, kepmappa, nev){
  
  dir.create(kepmappa, showWarnings = FALSE, recursive = TRUE)

  tbl <- as.data.frame(tablazat)
  
  library(ggplot2)
  library(scales) # for muted function
  g <- ggplot(tbl, aes(Var1, Var2)) + # x and y axes => Var1 and Var2
    geom_tile(aes(fill = Freq)) + # background colours are mapped according to the value column
    geom_text(aes(fill = tbl$Freq, label = round(tbl$Freq,3))) + # write the values
    scale_fill_gradient2(low = "white", 
                         #mid = "yellow", 
                         high = muted("green")) + #, 
    #midpoint = mean(tbl$Freq)) + # determine the colour
    theme(panel.grid.major.x=element_blank(), #no gridlines
          panel.grid.minor.x=element_blank(), 
          panel.grid.major.y=element_blank(), 
          panel.grid.minor.y=element_blank(),
          panel.background=element_rect(fill="white"), # background=white
          axis.text.x = element_text(angle=90, hjust = 1,vjust=1,size = 12,face = "bold"),
          plot.title = element_text(size=20,face="bold"),
          axis.text.y = element_text(size = 12,face = "bold")) + 
    #ggtitle("eszlelesek szama") + 
    theme(legend.title=element_text(face="bold", size=14)) + 
    guides(fill=guide_legend(title="eszleles")) +
    scale_x_discrete(name="") +
    scale_y_discrete(name="")
  #labs(fill="Corr. Coef.")
  g
  ggsave(str_c(kepmappa, '/hca_eredeti_', nev, '.pdf'), width = 12, height = 8)
  
  
  library(reshape2)
  tbl.df <- as.data.frame.matrix(tablazat)
  #tbl.df <- ifelse(tbl.df[,] > 0, 1, 0) #esetszam -> detektalas
  tbl.clust <- hclust(dist(tbl.df), method = 'ward.D')
  ord <- tbl.clust$order
  tbl.df.ord <- melt(tbl.df[ord,])
  tbl.df.ord$Var1 = row.names(tbl.df)[ord]
  tbl.df.ord$Var1 = factor(tbl.df.ord$Var1, levels = levels(factor(tbl.df.ord$Var1))[ord])
  colnames(tbl.df.ord)[1:2] = c('Var2', 'Freq')
  #colnames(tbl.df.ord)[colnames(tbl.df.ord) == 'value'] <- 'Freq'
  g2 <- ggplot(tbl.df.ord, aes(Var1, Var2)) + # x and y axes => Var1 and Var2
    geom_tile(aes(fill = Freq)) + # background colours are mapped according to the value column
    geom_text(aes(fill = tbl.df.ord$Freq, label = round(tbl.df.ord$Freq,3))) + # write the values
    scale_fill_gradient2(low = "white", 
                         #mid = "yellow", 
                         high = muted("green")) + #, 
    #midpoint = mean(tbl$Freq)) + # determine the colour
    theme(panel.grid.major.x=element_blank(), #no gridlines
          panel.grid.minor.x=element_blank(), 
          panel.grid.major.y=element_blank(), 
          panel.grid.minor.y=element_blank(),
          panel.background=element_rect(fill="white"), # background=white
          axis.text.x = element_text(angle=90, hjust = 1,vjust=1,size = 12,face = "bold"),
          plot.title = element_text(size=20,face="bold"),
          axis.text.y = element_text(size = 12,face = "bold")) + 
    #ggtitle("eszlelesek szama") + 
    theme(legend.title=element_text(face="bold", size=14)) + 
    scale_x_discrete(name="") +
    scale_y_discrete(name="")
  #labs(fill="Corr. Coef.")
  g2
  ggsave(str_c(kepmappa, '/hca_clustered_', nev, '.pdf'), width = 12, height = 8)

  
  tablazat = tablazat / max(tablazat)
  tbl.df <- as.data.frame.matrix(tablazat)
  #tbl.df <- ifelse(tbl.df[,] > 0, 1, 0) #esetszam -> detektalas
  tbl.clust <- hclust(dist(tbl.df), method = 'ward.D')
  ord <- tbl.clust$order
  tbl.df.ord <- melt(tbl.df[ord,])
  tbl.df.ord$Var1 = row.names(tbl.df)[ord]
  tbl.df.ord$Var1 = factor(tbl.df.ord$Var1, levels = levels(factor(tbl.df.ord$Var1))[ord])
  colnames(tbl.df.ord)[1:2] = c('Var2', 'Freq')
  #colnames(tbl.df.ord)[colnames(tbl.df.ord) == 'value'] <- 'Freq'
  g2 <- ggplot(tbl.df.ord, aes(Var1, Var2)) + # x and y axes => Var1 and Var2
    geom_tile(aes(fill = Freq)) + # background colours are mapped according to the value column
    geom_text(aes(fill = tbl.df.ord$Freq, label = round(tbl.df.ord$Freq, 3))) + # write the values
    scale_fill_gradient2(low = "white", 
                         #mid = "yellow", 
                         high = muted("green")) + #, 
    #midpoint = mean(tbl$Freq)) + # determine the colour
    theme(panel.grid.major.x=element_blank(), #no gridlines
          panel.grid.minor.x=element_blank(), 
          panel.grid.major.y=element_blank(), 
          panel.grid.minor.y=element_blank(),
          panel.background=element_rect(fill="white"), # background=white
          axis.text.x = element_text(angle=90, hjust = 1,vjust=1,size = 12,face = "bold"),
          plot.title = element_text(size=20,face="bold"),
          axis.text.y = element_text(size = 12,face = "bold")) + 
    #ggtitle("eszlelesek szama") + 
    theme(legend.title=element_text(face="bold", size=14)) + 
    scale_x_discrete(name="") +
    scale_y_discrete(name="")
  #labs(fill="Corr. Coef.")
  g2
  ggsave(str_c(kepmappa, '/hca_clustered_normalized_', nev, '.pdf'), width = 12, height = 8)
  
  
  library(RColorBrewer)
  pdf(str_c(kepmappa, '/hca_heatmap_', nev, '.pdf'), width = 8, height = 8)
  margin_height = max(strwidth(rownames(tbl.df), units = 'inches')) * par('fin')[1]
  margin_width = max(strwidth(colnames(tbl.df), units = 'inches')) * par('fin')[2]
  heatmap(t(as.matrix(tbl.df)), Colv = as.dendrogram(tbl.clust), Rowv = NA, col=RColorBrewer::brewer.pal(255, 'Greens'), margins = c(margin_height, margin_width))
  dev.off()
  
  
  library(dendsort)
  library(ggdendro)
  tbl.clust = dendsort(tbl.clust, isReverse = TRUE)
  ggdendrogram(tbl.clust, rotate = TRUE) #+ 
    #ggtitle(str_c('Dendrogram ', str_split_fixed(nev, '_', 2)[1]))
    #ggtitle('HCA egesz eves atmeneti valoszinusegek alapjan')
  ggsave(str_c(kepmappa, '/hca_dendrogram_', nev, '.pdf'), width = 8, height = 4)
  
}



transition_matrix <- function(f.df)
{
  library(reshape2)
  
  #2.b)1. Az ?tmeneti m?trix ?rt?keit minden szakaszra k?l?n sz?moljuk, elker?lend? a szakaszhat?rokon ?tny?l? virtu?lis esem?nyek okozta hib?t. Tov?bb? azokat az esem?ny p?rokat nem vesz?k figyelembe, ahol valamelyik esem?ny Indet..
  #El?sz?r l?trehozzuk a m?trixot, ami tartalmazza az ?tmeneteket:
  esemenyek <- unique(f.df$Felvetel.tartalma)
  m.trans <- matrix(0, nrow=length(esemenyek), ncol=length(esemenyek),
                    dimnames=list(Var1=esemenyek, Var2=esemenyek))
  #Ut?na k?vetkezik a m?trix felt?lt?se:
  #library(corrplot)
  for (sz in unique(f.df$szakaszID)) {
    szakasz <- f.df[f.df$szakaszID == sz,]
    for (i in 1:(nrow(szakasz)-1)) {
      start <- szakasz$Felvetel.tartalma[i]
      stop <- szakasz$Felvetel.tartalma[i+1]
      if(start != "Indet." && stop != "Indet.") {
        m.trans[start, stop] <- m.trans[start, stop] + 1
      }
    }
  }
  #corrplot(m.trans, is.corr=FALSE)
  #Az ?bra azt mutatja, hogy sok olyan esem?ny?nk van, ami ritk?n fordul el?, ez?rt nem igaz?n ?rdemes vel?k foglalkozni.
  
  # #2.b)2. A k?vetkez? pr?b?lkoz?s sor?n kiz?rom a 100-n?l kevesebbszer el?fordul? esem?nyeket.
  # esetek <- table(f.df$Felvetel.tartalma)
  # esemenyek <- names(esetek[esetek >= 100])
  # ff <- f.df[f.df$Felvetel.tartalma %in% esemenyek,]
  # sz.l <- tapply(ff$szakaszID, ff$szakaszID, length)
  # lh <- 2
  # keves.esemeny <- names(sz.l[sz.l < lh])
  # ff <- ff[!(ff$szakaszID %in% keves.esemeny),]
  # 
  # #Ezut?n felt?ltj?k az ?tmeneti m?trixot.
  # m.trans <- matrix(0, nrow=length(esemenyek), ncol=length(esemenyek),
  #                   dimnames=list(Var1=esemenyek, Var2=esemenyek))
  # for (sz in unique(ff$szakaszID)) {
  #   szakasz <- ff[ff$szakaszID == sz,]
  #   for (i in 1:(nrow(szakasz)-1)) {
  #     start <- szakasz$Felvetel.tartalma[i]
  #     stop <- szakasz$Felvetel.tartalma[i+1]
  #     if(start != "Indet." && stop != "Indet.") {
  #       m.trans[start, stop] <- m.trans[start, stop] + 1
  #     }
  #   }
  # }
  rs <- rowSums(m.trans)
  m.trans <- m.trans[names(sort(rs)), names(sort(rs))]
  #m.trans <- m.trans[-1, -1] # remove "Indet."
  #corrplot(m.trans, is.corr=FALSE)
  #Az ?tmeneti esem?nyek m?trixa nehezen ?rtelmezhet?, mert a k?l?nb?z? esem?nyek nagyon elt?r? sz?mban fordulnak el?.
  
  #2.b)3. Ez?rt az esem?nyek sz?m?ra norm?lnunk kell. Ez megadja az ?tmeneti val?sz?n?s?geket.
  #K?vetkezik az ?tmeneti val?sz?n?s?gek sz?mol?sa.
  p.trans <- m.trans/rowSums(m.trans)
  #corrplot(p.trans, is.corr=FALSE)
  #Ez m?r sokkal haszn?lhat?bb, de m?g mindig kiss? f?lrevezet?, mivel az egyes esem?nyek el?fordul?si val?sz?n?s?ge nem egyenletes.
  
  #2.b)4. Az erre val? korrekci?k?nt kisz?moljuk az esem?nyek el?fordul?si val?sz?n?s?g?t, vagyis azt a val?sz?n?s?get, ami megadja,
  #hogy egy adott esem?ny milyen val?sz?n?s?ggel fordul el?, ha az esem?nyek v?letlenszer?en k?vetkeznek egym?s ut?n.
  #layout(1:2)
  #corrplot(p.trans, is.corr=FALSE)
  #matplot(t(p.trans), type="b")
  exp.tr <- colSums(m.trans)/sum(m.trans)
  #matlines(exp.tr, lw=2)
  #plot of chunk calc-expected
  #layout(1)
  #Az als? ?br?n a vastag folytonos vonal mutatja a v?letlen el?fordul?s eset?n v?rhat? ?tmeneti val?sz?n?s?get.
  #Erre ?rdemes korrig?lni, hogy l?ssuk, az adott ?tmeneti val?sz?n?s?g kisebb vagy nagyobb enn?l a v?letlen v?rakoz?sn?l.
  pp <- apply(p.trans, 1, function(y) y/exp.tr)
  pp[which(pp == 0.0)] = 1.0
  #layout(1:2)
  #corrplot(log10(pp), is.corr=FALSE)
  
  return( list( pp=as.table(log10(pp)), m.trans=as.table(m.trans)) )
  
}
