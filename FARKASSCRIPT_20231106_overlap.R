
overlap.analysis = function ( f.df.o, table.postfix='' )
{
  
  library('circular')
  library('overlap')  
  library('stringr')
  
  caller.str = deparse(sys.calls()[[sys.nframe()-1]])
  if ( str_detect(caller.str, 'bnpi') )
    subdir = 'bnpi'
  else if ( str_detect(caller.str, 'cserkesz') )
    subdir = 'cserkesz'
  else if ( str_detect(caller.str, 'full') )
    subdir = 'full'
  else
    subdir = 'other'
  
  graphics.off()
  par(mar=c(1,1,1,1))
  
  summtable = data.frame(database=character(0), period=character(0),
                         overlap=character(0), dhat4=numeric(0),
                         human=integer(0), canis=integer(0), vulpes=integer(0),
                         cervus=integer(0), capreolus=integer(0), sus=integer(0))
  
  
  #Define periods
  
  gen.summer = sapply(f.df.o$Datum, function(y) as.numeric(str_split(as.character(y), '-')[[1]][2]) %in% c(4, 5, 6, 7, 8, 9))
  #gen.winter = sapply(f.df.o$Datum, function(y) as.numeric(str_split(as.character(y), '-')[[1]][2]) %in% c(1, 2, 11, 12))
  gen.winter = sapply(f.df.o$Datum, function(y) as.numeric(str_split(as.character(y), '-')[[1]][2]) %in% c(1, 2, 3, 10, 11, 12))
  gen.wlfpup = sapply(f.df.o$Datum, function(y) as.numeric(str_split(as.character(y), '-')[[1]][2]) %in% c(4, 5, 6))
  
  cap.birth  = sapply(f.df.o$Datum, function(y) as.numeric(str_split(as.character(y), '-')[[1]][2]) %in% c(5, 6))
  cap.rut    = sapply(f.df.o$Datum, function(y) as.numeric(str_split(as.character(y), '-')[[1]][2]) %in% c(7, 8))
  
  cer.birth  = sapply(f.df.o$Datum, function(y) as.numeric(str_split(as.character(y), '-')[[1]][2]) %in% c(5))
  cer.prerut = sapply(f.df.o$Datum, function(y) as.numeric(str_split(as.character(y), '-')[[1]][2]) %in% c(7, 8))
  cer.rut    = sapply(f.df.o$Datum, function(y) as.numeric(str_split(as.character(y), '-')[[1]][2]) %in% c(9, 10))
  
  sus.gest   = sapply(f.df.o$Datum, function(y) as.numeric(str_split(as.character(y), '-')[[1]][2]) %in% c(1, 2))
  sus.birth  = sapply(f.df.o$Datum, function(y) as.numeric(str_split(as.character(y), '-')[[1]][2]) %in% c(2, 3, 4))
  sus.esumm  = sapply(f.df.o$Datum, function(y) as.numeric(str_split(as.character(y), '-')[[1]][2]) %in% c(5, 6, 7))
  sus.male   = sapply(f.df.o$Datum, function(y) as.numeric(str_split(as.character(y), '-')[[1]][2]) %in% c(8, 9, 10))
  sus.breed  = sapply(f.df.o$Datum, function(y) as.numeric(str_split(as.character(y), '-')[[1]][2]) %in% c(11, 12))
  
  # yearly.periods = data.frame(full_year = TRUE, winter = gen.winter, wolf_pup = gen.wlfpup,
  #                             cap_birth = cap.birth, cap_rut = cap.rut,
  #                             cer_birth = cer.birth, cer_prerut = cer.prerut, cer_rut = cer.rut,
  #                             sus_gest = sus.gest, sus_birth = sus.birth, sus_earlysumm = sus.esumm, sus_solemale = sus.male, sus_breed = sus.breed)
  
  yearly.periods = data.frame(full_year = TRUE, winter = gen.winter, summer = gen.summer)
  
  
  for( p in colnames(yearly.periods) )
  {
    
    overlap.dir = str_c( figdir.olap, subdir, paste0(p, table.postfix), '/', sep = '/')
    dir.create(overlap.dir, showWarnings = FALSE, recursive = TRUE)
    
    f.df.overlap = f.df.o[yearly.periods[,p],]
    
    # circ.human = circular( sapply(f.df.overlap$Felvetel.kezdete[which(f.df.overlap$Felvetel.tartalma == "Human disturbance")],
    #                               function(x){ts = str_split_fixed(x, ':', 3);
    #                                           t = as.numeric(ts[,1]) + as.numeric(ts[,2])/60 + as.numeric(ts[,3])/3600}),
    #                        units = "hours", template = "clock24" )
    # #rose.diag(circ.human, bin=24, col = "lightblue", prop = 3)
    # bw.human = 40*bw.nrd0(circ.human)
    # dens.human = density.circular(circ.human, bw=bw.human)
    # plot(dens.human, plot.type = 'line', join = FALSE, xlab = "Hours", ylab = "Activity density")
    #
    # circ.canis = circular( sapply(f.df.overlap$Felvetel.kezdete[which(f.df.overlap$Felvetel.tartalma == "Canis lupus")],
    #                               function(x){ts = str_split_fixed(x, ':', 3);
    #                               t = as.numeric(ts[,1]) + as.numeric(ts[,2])/60 + as.numeric(ts[,3])/3600}),
    #                        units = "hours", template = "clock24" )
    # #rose.diag(circ.canis, bin=24, col = "lightblue", prop = 3)
    # bw.canis = 40*bw.nrd0(circ.canis)
    # dens.canis = density.circular(circ.canis, bw=bw.canis)
    # plot(dens.canis, plot.type = 'line', join = FALSE, xlab = "Hours", ylab = "Activity density")
    #
    # circ.cervus = circular( sapply(f.df.overlap$Felvetel.kezdete[which(f.df.overlap$Felvetel.tartalma == "Cervus elaphus")],
    #                               function(x){ts = str_split_fixed(x, ':', 3);
    #                               t = as.numeric(ts[,1]) + as.numeric(ts[,2])/60 + as.numeric(ts[,3])/3600}),
    #                        units = "hours", template = "clock24" )
    # #rose.diag(circ.cervus, bin=24, col = "lightblue", prop = 3)
    # bw.cervus = 40*bw.nrd0(circ.cervus)
    # dens.cervus = density.circular(circ.cervus, bw=bw.cervus)
    # plot(dens.cervus, plot.type = 'line', join = FALSE, xlab = "Hours", ylab = "Activity density")
    #
    # circ.capreolus = circular( sapply(f.df.overlap$Felvetel.kezdete[which(f.df.overlap$Felvetel.tartalma == "Capreolus capreolus")],
    #                               function(x){ts = str_split_fixed(x, ':', 3);
    #                               t = as.numeric(ts[,1]) + as.numeric(ts[,2])/60 + as.numeric(ts[,3])/3600}),
    #                        units = "hours", template = "clock24" )
    # #rose.diag(circ.capreolus, bin=24, col = "lightblue", prop = 3)
    # bw.capreolus = 40*bw.nrd0(circ.capreolus)
    # dens.capreolus = density.circular(circ.capreolus, bw=bw.capreolus)
    # plot(dens.capreolus, plot.type = 'line', join = FALSE, xlab = "Hours", ylab = "Activity density")
    #
    # circ.sus = circular( sapply(f.df.overlap$Felvetel.kezdete[which(f.df.overlap$Felvetel.tartalma == "Sus scrofa")],
    #                               function(x){ts = str_split_fixed(x, ':', 3);
    #                               t = as.numeric(ts[,1]) + as.numeric(ts[,2])/60 + as.numeric(ts[,3])/3600}),
    #                        units = "hours", template = "clock24" )
    # #rose.diag(circ.sus, bin=24, col = "lightblue", prop = 3)
    # bw.sus = 40*bw.nrd0(circ.sus)
    # dens.sus = density.circular(circ.sus, bw=bw.sus)
    # plot(dens.sus, plot.type = 'line', join = FALSE, xlab = "Hours", ylab = "Activity density")
    
    
    #overlap
    circ.human = circular( sapply(f.df.overlap$Felvetel.kezdete[which(f.df.overlap$Felvetel.tartalma == "Human disturbance")],
                                  function(x){ts = str_split_fixed(x, ':', 3);
                                  t = (as.numeric(ts[,1]) + as.numeric(ts[,2])/60 + as.numeric(ts[,3])/3600) / (24 / (2*pi)) }),
                           units = "radians", modulo = "2pi", template = "none" )
    
    circ.canis = circular( sapply(f.df.overlap$Felvetel.kezdete[which(f.df.overlap$Felvetel.tartalma == "Canis lupus")],
                                  function(x){ts = str_split_fixed(x, ':', 3);
                                  t = (as.numeric(ts[,1]) + as.numeric(ts[,2])/60 + as.numeric(ts[,3])/3600) / (24 / (2*pi)) }),
                           units = "radians", modulo = "2pi", template = "none" )
    
    circ.cervus = circular( sapply(f.df.overlap$Felvetel.kezdete[which(f.df.overlap$Felvetel.tartalma == "Cervus elaphus")],
                                   function(x){ts = str_split_fixed(x, ':', 3);
                                   t = (as.numeric(ts[,1]) + as.numeric(ts[,2])/60 + as.numeric(ts[,3])/3600) / (24 / (2*pi)) }),
                            units = "radians", modulo = "2pi", template = "none" )
    
    circ.capreolus = circular( sapply(f.df.overlap$Felvetel.kezdete[which(f.df.overlap$Felvetel.tartalma == "Capreolus capreolus")],
                                      function(x){ts = str_split_fixed(x, ':', 3);
                                      t = (as.numeric(ts[,1]) + as.numeric(ts[,2])/60 + as.numeric(ts[,3])/3600) / (24 / (2*pi)) }),
                               units = "radians", modulo = "2pi", template = "none" )
    
    circ.sus = circular( sapply(f.df.overlap$Felvetel.kezdete[which(f.df.overlap$Felvetel.tartalma == "Sus scrofa")],
                                function(x){ts = str_split_fixed(x, ':', 3);
                                t = (as.numeric(ts[,1]) + as.numeric(ts[,2])/60 + as.numeric(ts[,3])/3600) / (24 / (2*pi)) }),
                         units = "radians", modulo = "2pi", template = "none" )
    
    circ.vulpes = circular( sapply(f.df.overlap$Felvetel.kezdete[which(f.df.overlap$Felvetel.tartalma == "Vulpes vulpes")],
                                   function(x){ts = str_split_fixed(x, ':', 3);
                                   t = (as.numeric(ts[,1]) + as.numeric(ts[,2])/60 + as.numeric(ts[,3])/3600) / (24 / (2*pi)) }),
                            units = "radians", modulo = "2pi", template = "none" )
    
    circ.game = circular( sapply(f.df.overlap$Felvetel.kezdete[which(f.df.overlap$Felvetel.tartalma == "Cervus elaphus" |
                                                                       f.df.overlap$Felvetel.tartalma == "Capreolus capreolus" |
                                                                       f.df.overlap$Felvetel.tartalma == "Sus scrofa")],
                                 function(x){ts = str_split_fixed(x, ':', 3);
                                 t = (as.numeric(ts[,1]) + as.numeric(ts[,2])/60 + as.numeric(ts[,3])/3600) / (24 / (2*pi)) }),
                          units = "radians", modulo = "2pi", template = "none" )
    
    circ.felis = circular( sapply(f.df.overlap$Felvetel.kezdete[which(f.df.overlap$Felvetel.tartalma == "Felis silvestris")],
                                  function(x){ts = str_split_fixed(x, ':', 3);
                                  t = (as.numeric(ts[,1]) + as.numeric(ts[,2])/60 + as.numeric(ts[,3])/3600) / (24 / (2*pi)) }),
                           units = "radians", modulo = "2pi", template = "none" )
    
    circ.meles = circular( sapply(f.df.overlap$Felvetel.kezdete[which(f.df.overlap$Felvetel.tartalma == "Meles meles")],
                                  function(x){ts = str_split_fixed(x, ':', 3);
                                  t = (as.numeric(ts[,1]) + as.numeric(ts[,2])/60 + as.numeric(ts[,3])/3600) / (24 / (2*pi)) }),
                           units = "radians", modulo = "2pi", template = "none" )
    
    
    #pdf(str_c(overlap.dir, 'olap_hum_can.pdf'), width = 6, height = 5)
    tiff(str_c(overlap.dir, 'olap_hum_can.tiff'), width = 6, height = 5, res = 300, units = 'in', compression = c('lzw'))
    overlapPlot(as.numeric(circ.human), as.numeric(circ.canis), kmax = 5, linewidth = c(2,2), olapcol = "#d9f2d9",
                main = NULL, ylab = "Activity density", cex.lab = 1.25, ylim = c(0.0,0.145))
    legend("topleft", legend = c("Human disturbance", "Canis lupus"), col = c("black", "blue"), lty = c(1,2), cex = 0.8)
    d4.hum.can = round(overlapEst(as.numeric(circ.human), as.numeric(circ.canis), type = "Dhat4"), 2)
    legend("topright", as.expression(bquote(Delta['4'] == .(d4.hum.can))), bty = "n", text.width = 4)
    dev.off()
    
    #pdf(str_c(overlap.dir, 'olap_hum_cer.pdf'), width = 6, height = 5)
    tiff(str_c(overlap.dir, 'olap_hum_cer.tiff'), width = 6, height = 5, res = 300, units = 'in', compression = c('lzw'))
    overlapPlot(as.numeric(circ.human), as.numeric(circ.cervus), kmax = 5, linewidth = c(2,2), olapcol = "#d9f2d9",
                main = NULL, ylab = "Activity density", cex.lab = 1.25, ylim = c(0.0,0.145))
    legend("topleft", legend = c("Human disturbance", "Cervus elaphus"), col = c("black", "blue"), lty = c(1,2), cex = 0.8)
    d4.hum.cer = round(overlapEst(as.numeric(circ.human), as.numeric(circ.cervus), type = "Dhat4"), 2)
    legend("topright", as.expression(bquote(Delta['4'] == .(d4.hum.cer))), bty = "n", text.width = 4)
    dev.off()
    
    #pdf(str_c(overlap.dir, 'olap_hum_cap.pdf'), width = 6, height = 5)
    tiff(str_c(overlap.dir, 'olap_hum_cap.tiff'), width = 6, height = 5, res = 300, units = 'in', compression = c('lzw'))
    overlapPlot(as.numeric(circ.human), as.numeric(circ.capreolus), kmax = 5, linewidth = c(2,2), olapcol = "#d9f2d9",
                main = NULL, ylab = "Activity density", cex.lab = 1.25, ylim = c(0.0,0.145))
    legend("topleft", legend = c("Human disturbance", "Capreolus capreolus"), col = c("black", "blue"), lty = c(1,2), cex = 0.8)
    d4.hum.cap = round(overlapEst(as.numeric(circ.human), as.numeric(circ.capreolus), type = "Dhat4"), 2)
    legend("topright", as.expression(bquote(Delta['4'] == .(d4.hum.cap))), bty = "n", text.width = 4)
    dev.off()
    
    #pdf(str_c(overlap.dir, 'olap_hum_sus.pdf'), width = 6, height = 5)
    tiff(str_c(overlap.dir, 'olap_hum_sus.tiff'), width = 6, height = 5, res = 300, units = 'in', compression = c('lzw'))
    overlapPlot(as.numeric(circ.human), as.numeric(circ.sus), kmax = 5, linewidth = c(2,2), olapcol = "#d9f2d9",
                main = NULL, ylab = "Activity density", cex.lab = 1.25, ylim = c(0.0,0.145))
    legend("topleft", legend = c("Human disturbance", "Sus scrofa"), col = c("black", "blue"), lty = c(1,2), cex = 0.8)
    d4.hum.sus = round(overlapEst(as.numeric(circ.human), as.numeric(circ.sus), type = "Dhat4"), 2)
    legend("topright", as.expression(bquote(Delta['4'] == .(d4.hum.sus))), bty = "n", text.width = 4)
    dev.off()
    
    #pdf(str_c(overlap.dir, 'olap_can_cer.pdf'), width = 6, height = 5)
    tiff(str_c(overlap.dir, 'olap_can_cer.tiff'), width = 6, height = 5, res = 300, units = 'in', compression = c('lzw'))
    overlapPlot(as.numeric(circ.canis), as.numeric(circ.cervus), kmax = 5, linewidth = c(2,2), olapcol = "#d9f2d9",
                main = NULL, ylab = "Activity density", cex.lab = 1.25, ylim = c(0.0,0.145))
    legend("topleft", legend = c("Canis lupus", "Cervus elaphus"), col = c("black", "blue"), lty = c(1,2), cex = 0.8)
    d4.can.cer = round(overlapEst(as.numeric(circ.canis), as.numeric(circ.cervus), type = "Dhat4"), 2)
    legend("topright", as.expression(bquote(Delta['4'] == .(d4.can.cer))), bty = "n", text.width = 4)
    dev.off()
    
    
    #pdf(str_c(overlap.dir, 'olap_can_cap.pdf'), width = 6, height = 5)
    tiff(str_c(overlap.dir, 'olap_can_cap.tiff'), width = 6, height = 5, res = 300, units = 'in', compression = c('lzw'))
    overlapPlot(as.numeric(circ.canis), as.numeric(circ.capreolus), kmax = 5, linewidth = c(2,2), olapcol = "#d9f2d9",
                main = NULL, ylab = "Activity density", cex.lab = 1.25, ylim = c(0.0,0.145))
    legend("topleft", legend = c("Canis lupus", "Capreolus capreolus"), col = c("black", "blue"), lty = c(1,2), cex = 0.8)
    d4.can.cap = round(overlapEst(as.numeric(circ.canis), as.numeric(circ.capreolus), type = "Dhat4"), 2)
    legend("topright", as.expression(bquote(Delta['4'] == .(d4.can.cap))), bty = "n", text.width = 4)
    dev.off()
    
    
    #pdf(str_c(overlap.dir, 'olap_can_sus.pdf'), width = 6, height = 5)
    tiff(str_c(overlap.dir, 'olap_can_sus.tiff'), width = 6, height = 5, res = 300, units = 'in', compression = c('lzw'))
    overlapPlot(as.numeric(circ.canis), as.numeric(circ.sus), kmax = 5, linewidth = c(2,2), olapcol = "#d9f2d9",
                main = NULL, ylab = "Activity density", cex.lab = 1.25, ylim = c(0.0,0.145))
    legend("topleft", legend = c("Canis lupus", "Sus scrofa"), col = c("black", "blue"), lty = c(1,2), cex = 0.8)
    d4.can.sus = round(overlapEst(as.numeric(circ.canis), as.numeric(circ.sus), type = "Dhat4"), 2)
    legend("topright", as.expression(bquote(Delta['4'] == .(d4.can.sus))), bty = "n", text.width = 4)
    dev.off()
    
    
    #pdf(str_c(overlap.dir, 'olap_can_vul.pdf'), width = 6, height = 5)
    tiff(str_c(overlap.dir, 'olap_can_vul.tiff'), width = 6, height = 5, res = 300, units = 'in', compression = c('lzw'))
    overlapPlot(as.numeric(circ.canis), as.numeric(circ.vulpes), kmax = 5, linewidth = c(2,2), olapcol = "#d9f2d9",
                main = NULL, ylab = "Activity density", cex.lab = 1.25, ylim = c(0.0,0.145))
    legend("topleft", legend = c("Canis lupus", "Vulpes vulpes"), col = c("black", "blue"), lty = c(1,2), cex = 0.8)
    d4.can.vul = round(overlapEst(as.numeric(circ.canis), as.numeric(circ.vulpes), type = "Dhat4"), 2)
    legend("topright", as.expression(bquote(Delta['4'] == .(d4.can.vul))), bty = "n", text.width = 4)
    dev.off()
    
    
    #pdf(str_c(overlap.dir, 'olap_can_fel.pdf'), width = 6, height = 5)
    tiff(str_c(overlap.dir, 'olap_can_fel.tiff'), width = 6, height = 5, res = 300, units = 'in', compression = c('lzw'))
    overlapPlot(as.numeric(circ.canis), as.numeric(circ.felis), kmax = 5, linewidth = c(2,2), olapcol = "#d9f2d9",
                main = NULL, ylab = "Activity density", cex.lab = 1.25, ylim = c(0.0,0.145))
    legend("topleft", legend = c("Canis lupus", "Felis silvestris"), col = c("black", "blue"), lty = c(1,2), cex = 0.8)
    d4.can.fel = round(overlapEst(as.numeric(circ.canis), as.numeric(circ.felis), type = "Dhat4"), 2)
    legend("topright", as.expression(bquote(Delta['4'] == .(d4.can.fel))), bty = "n", text.width = 4)
    dev.off()
    
    
    #pdf(str_c(overlap.dir, 'olap_can_mel.pdf'), width = 6, height = 5)
    tiff(str_c(overlap.dir, 'olap_can_mel.tiff'), width = 6, height = 5, res = 300, units = 'in', compression = c('lzw'))
    overlapPlot(as.numeric(circ.canis), as.numeric(circ.meles), kmax = 5, linewidth = c(2,2), olapcol = "#d9f2d9",
                main = NULL, ylab = "Activity density", cex.lab = 1.25, ylim = c(0.0,0.145))
    legend("topleft", legend = c("Canis lupus", "Meles meles"), col = c("black", "blue"), lty = c(1,2), cex = 0.8)
    d4.can.mel = round(overlapEst(as.numeric(circ.canis), as.numeric(circ.meles), type = "Dhat4"), 2)
    legend("topright", as.expression(bquote(Delta['4'] == .(d4.can.mel))), bty = "n", text.width = 4)
    dev.off()
    
    
    #pdf(str_c(overlap.dir, 'olap_vul_cap.pdf'), width = 6, height = 5)
    tiff(str_c(overlap.dir, 'olap_vul_cap.tiff'), width = 6, height = 5, res = 300, units = 'in', compression = c('lzw'))
    overlapPlot(as.numeric(circ.vulpes), as.numeric(circ.capreolus), kmax = 5, linewidth = c(2,2), olapcol = "#d9f2d9",
                main = NULL, ylab = "Activity density", cex.lab = 1.25, ylim = c(0.0,0.145))
    legend("topleft", legend = c("Vulpes vulpes", "Capreolus capreolus"), col = c("black", "blue"), lty = c(1,2), cex = 0.8)
    d4.vul.cap = round(overlapEst(as.numeric(circ.vulpes), as.numeric(circ.capreolus), type = "Dhat4"), 2)
    legend("topright", as.expression(bquote(Delta['4'] == .(d4.vul.cap))), bty = "n", text.width = 4)
    dev.off()
    
    
    #pdf(str_c(overlap.dir, 'olap_hum_vul.pdf'), width = 6, height = 5)
    tiff(str_c(overlap.dir, 'olap_hum_vul.tiff'), width = 6, height = 5, res = 300, units = 'in', compression = c('lzw'))
    overlapPlot(as.numeric(circ.human), as.numeric(circ.vulpes), kmax = 5, linewidth = c(2,2), olapcol = "#d9f2d9",
                main = NULL, ylab = "Activity density", cex.lab = 1.25, ylim = c(0.0,0.145))
    legend("topleft", legend = c("Human disturbance", "Vulpes vulpes"), col = c("black", "blue"), lty = c(1,2), cex = 0.8)
    d4.hum.vul = round(overlapEst(as.numeric(circ.human), as.numeric(circ.vulpes), type = "Dhat4"), 2)
    legend("topright", as.expression(bquote(Delta['4'] == .(d4.hum.vul))), bty = "n", text.width = 4)
    dev.off()
    
    #pdf(str_c(overlap.dir, 'olap_can_gam.pdf'), width = 6, height = 5)
    tiff(str_c(overlap.dir, 'olap_can_gam.tiff'), width = 6, height = 5, res = 300, units = 'in', compression = c('lzw'))
    overlapPlot(as.numeric(circ.canis), as.numeric(circ.game), kmax = 5, linewidth = c(2,2), olapcol = "#d9f2d9",
                main = NULL, ylab = "Activity density", cex.lab = 1.25, ylim = c(0.0,0.145))
    legend("topleft", legend = c("Canis lupus", "Big game"), col = c("black", "blue"), lty = c(1,2), cex = 0.8)
    d4.can.gam = round(overlapEst(as.numeric(circ.canis), as.numeric(circ.game), type = "Dhat4"), 2)
    legend("topright", as.expression(bquote(Delta['4'] == .(d4.can.gam))), bty = "n", text.width = 4)
    dev.off()
    
    #pdf(str_c(overlap.dir, 'olap_hum_gam.pdf'), width = 6, height = 5)
    tiff(str_c(overlap.dir, 'olap_hum_gam.tiff'), width = 6, height = 5, res = 300, units = 'in', compression = c('lzw'))
    overlapPlot(as.numeric(circ.human), as.numeric(circ.game), kmax = 5, linewidth = c(2,2), olapcol = "#d9f2d9",
                main = NULL, ylab = "Activity density", cex.lab = 1.25, ylim = c(0.0,0.145))
    legend("topleft", legend = c("Human disturbance", "Big game"), col = c("black", "blue"), lty = c(1,2), cex = 0.8)
    d4.hum.gam = round(overlapEst(as.numeric(circ.human), as.numeric(circ.game), type = "Dhat4"), 2)
    legend("topright", as.expression(bquote(Delta['4'] == .(d4.hum.gam))), bty = "n", text.width = 4)
    dev.off()
    
    cat(p, '\n')
    ol.hum.can = overlapEst(as.numeric(circ.human), as.numeric(circ.canis), type = "Dhat4")
    ol.hum.cap = overlapEst(as.numeric(circ.human), as.numeric(circ.capreolus), type = "Dhat4")
    ol.hum.cer = overlapEst(as.numeric(circ.human), as.numeric(circ.cervus), type = "Dhat4")
    ol.hum.sus = overlapEst(as.numeric(circ.human), as.numeric(circ.sus), type = "Dhat4")
    ol.can.cap = overlapEst(as.numeric(circ.canis), as.numeric(circ.capreolus), type = "Dhat4")
    ol.can.cer = overlapEst(as.numeric(circ.canis), as.numeric(circ.cervus), type = "Dhat4")
    ol.can.sus = overlapEst(as.numeric(circ.canis), as.numeric(circ.sus), type = "Dhat4")
    ol.vul.cap = overlapEst(as.numeric(circ.vulpes), as.numeric(circ.capreolus), type = "Dhat4")
    
    bs.hum.can = bootstrap(as.numeric(circ.human), as.numeric(circ.canis), nb = 100, type = "Dhat4"); cat('.');
    bs.hum.cap = bootstrap(as.numeric(circ.human), as.numeric(circ.capreolus), nb = 100, type = "Dhat4"); cat('.');
    bs.hum.cer = bootstrap(as.numeric(circ.human), as.numeric(circ.cervus), nb = 100, type = "Dhat4"); cat('.');
    bs.hum.sus = bootstrap(as.numeric(circ.human), as.numeric(circ.sus), nb = 100, type = "Dhat4"); cat('.');
    bs.can.cap = bootstrap(as.numeric(circ.canis), as.numeric(circ.capreolus), nb = 100, type = "Dhat4"); cat('.');
    bs.can.cer = bootstrap(as.numeric(circ.canis), as.numeric(circ.cervus), nb = 100, type = "Dhat4"); cat('.');
    bs.can.sus = bootstrap(as.numeric(circ.canis), as.numeric(circ.sus), nb = 100, type = "Dhat4"); cat('.');
    bs.vul.cap = bootstrap(as.numeric(circ.vulpes), as.numeric(circ.capreolus), nb = 100, type = "Dhat4"); cat('.\n');
    
    ci.hum.can = bootCI(ol.hum.can, bs.hum.can, conf = 0.95)
    ci.hum.cap = bootCI(ol.hum.cap, bs.hum.cap, conf = 0.95)
    ci.hum.cer = bootCI(ol.hum.cer, bs.hum.cer, conf = 0.95)
    ci.hum.sus = bootCI(ol.hum.sus, bs.hum.sus, conf = 0.95)
    ci.can.cap = bootCI(ol.can.cap, bs.can.cap, conf = 0.95)
    ci.can.cer = bootCI(ol.can.cer, bs.can.cer, conf = 0.95)
    ci.can.sus = bootCI(ol.can.sus, bs.can.sus, conf = 0.95)
    ci.vul.cap = bootCI(ol.vul.cap, bs.vul.cap, conf = 0.95)
    
    overlaps = c('hum_can', 'hum_cer', 'hum_cap', 'hum_sus', 'can_cer', 'can_cap', 'can_sus', 'vul_cap')
    dhats = c(d4.hum.can, d4.hum.cer, d4.hum.cap, d4.hum.sus, d4.can.cer, d4.can.cap, d4.can.sus, d4.vul.cap)
    CImin = c(ci.hum.can['norm0','lower'], ci.hum.cer['norm0','lower'], ci.hum.cap['norm0','lower'], ci.hum.sus['norm0','lower'],
              ci.can.cer['norm0','lower'], ci.can.cap['norm0','lower'], ci.can.sus['norm0','lower'], ci.vul.cap['norm0','lower'])
    CImax = c(ci.hum.can['norm0','upper'], ci.hum.cer['norm0','upper'], ci.hum.cap['norm0','upper'], ci.hum.sus['norm0','upper'],
              ci.can.cer['norm0','upper'], ci.can.cap['norm0','upper'], ci.can.sus['norm0','upper'], ci.vul.cap['norm0','upper'])
    
    summtable = rbind( summtable,
                       data.frame(database='bnpi', period=p,
                                  overlap=overlaps, dhat4=dhats, CImin=CImin, CImax=CImax,
                                  human=length(circ.human), canis=length(circ.canis), vulpes=length(circ.vulpes),
                                  cervus=length(circ.cervus), capreolus=length(circ.capreolus), sus=length(circ.sus))
    )
    
  }
  
  write.table(summtable, str_c(overlap.dir, '/../overlap_summary', table.postfix, '.csv'), sep = ';', row.names = FALSE, col.names = TRUE, dec = ',')
  
  return (summtable)
}


overlap.analysis.mx = function ( f.df.o, table.postfix='' )
{
  
  library('circular')
  library('overlap')  
  library('stringr')
  
  caller.str = deparse(sys.calls()[[sys.nframe()-1]], nlines = 1)
  if ( str_detect(caller.str, 'bnpi') )
    subdir = 'bnpi'
  else if ( str_detect(caller.str, 'cserkesz') )
    subdir = 'cserkesz'
  else if ( str_detect(caller.str, 'full') )
    subdir = 'full'
  else
    subdir = 'other'
  
  graphics.off()
  par(mar=c(1,1,1,1))
  
  summtable = data.frame(database=character(0), period=character(0),
                         i=character(0), j=character(0),
                         dhat4=numeric(0), CImin=numeric(0), CImax=numeric(0),
                         Ni=integer(0), Nj=integer(0))
  
  #Define periods
  
  full.year = rep(TRUE, dim(f.df.o)[1])
  
  gen.summer = sapply(f.df.o$Datum, function(y) as.numeric(str_split(as.character(y), '-')[[1]][2]) %in% c(4, 5, 6, 7, 8, 9))
  #gen.winter = sapply(f.df.o$Datum, function(y) as.numeric(str_split(as.character(y), '-')[[1]][2]) %in% c(1, 2, 11, 12))
  gen.winter = sapply(f.df.o$Datum, function(y) as.numeric(str_split(as.character(y), '-')[[1]][2]) %in% c(1, 2, 3, 10, 11, 12))
  gen.wlfpup = sapply(f.df.o$Datum, function(y) as.numeric(str_split(as.character(y), '-')[[1]][2]) %in% c(4, 5, 6))
  
  cap.birth  = sapply(f.df.o$Datum, function(y) as.numeric(str_split(as.character(y), '-')[[1]][2]) %in% c(5, 6))
  cap.rut    = sapply(f.df.o$Datum, function(y) as.numeric(str_split(as.character(y), '-')[[1]][2]) %in% c(7, 8))
  
  cer.birth  = sapply(f.df.o$Datum, function(y) as.numeric(str_split(as.character(y), '-')[[1]][2]) %in% c(5))
  cer.prerut = sapply(f.df.o$Datum, function(y) as.numeric(str_split(as.character(y), '-')[[1]][2]) %in% c(7, 8))
  cer.rut    = sapply(f.df.o$Datum, function(y) as.numeric(str_split(as.character(y), '-')[[1]][2]) %in% c(9, 10))
  
  sus.gest   = sapply(f.df.o$Datum, function(y) as.numeric(str_split(as.character(y), '-')[[1]][2]) %in% c(1, 2))
  sus.birth  = sapply(f.df.o$Datum, function(y) as.numeric(str_split(as.character(y), '-')[[1]][2]) %in% c(2, 3, 4))
  sus.esumm  = sapply(f.df.o$Datum, function(y) as.numeric(str_split(as.character(y), '-')[[1]][2]) %in% c(5, 6, 7))
  sus.male   = sapply(f.df.o$Datum, function(y) as.numeric(str_split(as.character(y), '-')[[1]][2]) %in% c(8, 9, 10))
  sus.breed  = sapply(f.df.o$Datum, function(y) as.numeric(str_split(as.character(y), '-')[[1]][2]) %in% c(11, 12))
  
  # yearly.periods = data.frame(full_year = full.year, winter = gen.winter, wolf_pup = gen.wlfpup,
  #                             cap_birth = cap.birth, cap_rut = cap.rut,
  #                             cer_birth = cer.birth, cer_prerut = cer.prerut, cer_rut = cer.rut,
  #                             sus_gest = sus.gest, sus_birth = sus.birth, sus_earlysumm = sus.esumm, sus_solemale = sus.male, sus_breed = sus.breed)
  
  #yearly.periods = data.frame(full_year = full.year, winter = gen.winter, summer = gen.summer)
  
  yearly.periods = data.frame(full_year = full.year)
  
  return_fullyear_matrix = NULL
  species = unique(f.df.o$Felvetel.tartalma)
  #Temporal modification: change species order only for this run
  #species = c('Human disturbance', 'Canis lupus', 'Vulpes vulpes',
  #            'Cervus elaphus', 'Capreolus capreolus', 'Sus scrofa')
  #species = c('Human disturbance', 'Canis lupus', 'Vulpes vulpes', 'Ungulates')
  
  
  for( p in colnames(yearly.periods) )
  {
    cat(p, ':\n')
    
    graphics.off()
    par(mar=c(1,1,1,1))
    
    overlap.dir = str_c( figdir.olap, subdir, paste0(p, table.postfix), '/', sep = '/')
    dir.create(overlap.dir, showWarnings = FALSE, recursive = TRUE)
    
    f.df.overlap = f.df.o[yearly.periods[,p],]
    
    summtable.p = data.frame(database=character(0), period=character(0),
                             i=character(0), j=character(0),
                             dhat4=numeric(0), CImin=numeric(0), CImax=numeric(0),
                             Ni=integer(0), Nj=integer(0))
    
    for( i in 1:(length(species)-1) )
    {
      for( j in (i+1):length(species) )
      {
        cat(species[i], ' X ', species[j])
        
        # Make daily data circular
        circ.i = circular( sapply(f.df.overlap$Felvetel.kezdete[which(f.df.overlap$Felvetel.tartalma == species[i])],
                                  function(x){ts = str_split_fixed(x, ':', 3);
                                  t = (as.numeric(ts[,1]) + as.numeric(ts[,2])/60 + as.numeric(ts[,3])/3600) / (24 / (2*pi)) }),
                           units = "radians", modulo = "2pi", template = "none" )
        circ.j = circular( sapply(f.df.overlap$Felvetel.kezdete[which(f.df.overlap$Felvetel.tartalma == species[j])],
                                  function(x){ts = str_split_fixed(x, ':', 3);
                                  t = (as.numeric(ts[,1]) + as.numeric(ts[,2])/60 + as.numeric(ts[,3])/3600) / (24 / (2*pi)) }),
                           units = "radians", modulo = "2pi", template = "none" )
        
        # Create overlap image
        tiff(str_c(overlap.dir, paste0('olap_', species[i], '_', species[j], '.tiff')), width = 6, height = 5, res = 300, units = 'in', compression = c('lzw'))
        overlapPlot(as.numeric(circ.i), as.numeric(circ.j), kmax = 5, linewidth = c(2,2), olapcol = "#d9f2d9",
                    main = NULL, ylab = "Activity density", cex.lab = 1.25, ylim = c(0.0,0.145))
        legend("topleft", legend = c(species[i], species[j]), col = c("black", "blue"), lty = c(1,2), cex = 0.8)
        d4 = round(overlapEst(as.numeric(circ.i), as.numeric(circ.j), type = "Dhat4"), 2)
        legend("topright", as.expression(bquote(Delta['4'] == .(d4))), bty = "n", text.width = 4)
        dev.off()
        
        # Compute bootstrapped confidence interval
        ol = overlapEst(as.numeric(circ.i), as.numeric(circ.j), type = "Dhat4")
        bs = bootstrap(as.numeric(circ.i), as.numeric(circ.j), nb = 100, type = "Dhat4"); cat('.\n');
        ci = bootCI(ol, bs, conf = 0.95)
        
        # Collect summary data
        #overlaps = c('hum_can', 'hum_cer', 'hum_cap', 'hum_sus', 'can_cer', 'can_cap', 'can_sus', 'vul_cap')
        #dhats = c(d4.hum.can, d4.hum.cer, d4.hum.cap, d4.hum.sus, d4.can.cer, d4.can.cap, d4.can.sus, d4.vul.cap)
        #CImin = c(ci.hum.can['norm0','lower'], ci.hum.cer['norm0','lower'], ci.hum.cap['norm0','lower'], ci.hum.sus['norm0','lower'],
        #          ci.can.cer['norm0','lower'], ci.can.cap['norm0','lower'], ci.can.sus['norm0','lower'], ci.vul.cap['norm0','lower'])
        #CImax = c(ci.hum.can['norm0','upper'], ci.hum.cer['norm0','upper'], ci.hum.cap['norm0','upper'], ci.hum.sus['norm0','upper'],
        #          ci.can.cer['norm0','upper'], ci.can.cap['norm0','upper'], ci.can.sus['norm0','upper'], ci.vul.cap['norm0','upper'])
        
        summtable.p = rbind( summtable.p,
                             data.frame(database='bnpi', period=p,
                                        i=species[i], j=species[j],
                                        dhat4=d4, CImin=ci['norm0', 'lower'], CImax=ci['norm0', 'upper'],
                                        Ni=length(circ.i), Nj=length(circ.j))
        )
      }
    }
    
    summtable = rbind( summtable, summtable.p )
    
    # Create matrix of overlap coefficients (dhat4 matrix)
    d4.mat = matrix(0.0, nrow = length(species), ncol = length(species))
    d4.mat[lower.tri(d4.mat, diag = FALSE)] = summtable$dhat4
    colnames(d4.mat) = species
    rownames(d4.mat) = species
    write.table(d4.mat, str_c(overlap.dir, '/dhat4_matrix.csv'), sep = ';', row.names = FALSE, col.names = TRUE, dec = ',')
    
    if (p == 'full_year')
      return_fullyear_matrix = d4.mat
    
    hcd = as.dendrogram( hclust(as.dist(d4.mat), method = 'average') )
    nodePar <- list(lab.cex = 0.6, pch = c(NA, 19), cex = 0.7, col = "blue")
    graphics.off()
    tiff(str_c(overlap.dir, paste0('dhat4_matrix.tiff')), width = 6, height = 6, res = 300, units = 'in', compression = c('lzw'))
    plot(hcd,  ylab = "Height", nodePar = nodePar, edgePar = list(col = 2:3, lwd = 2:1))
    dev.off()
    
    # Create matrix of overlap coefficients (1-dhat4 matrix)
    d4.mat = matrix(1.0, nrow = length(species), ncol = length(species))
    d4.mat[lower.tri(d4.mat, diag = FALSE)] = summtable$dhat4
    d4.mat = 1.0 - d4.mat
    colnames(d4.mat) = species
    rownames(d4.mat) = species
    write.table(d4.mat, str_c(overlap.dir, '/dhat4_inv_matrix.csv'), sep = ';', row.names = FALSE, col.names = TRUE, dec = ',')
    
    if (p == 'full_year')
      return_fullyear_matrix = d4.mat
    
    hcd = as.dendrogram( hclust(as.dist(d4.mat), method = 'average') )
    nodePar <- list(lab.cex = 0.6, pch = c(NA, 19), cex = 0.7, col = "blue")
    graphics.off()
    tiff(str_c(overlap.dir, paste0('dhat4_inv_matrix.tiff')), width = 6, height = 6, res = 300, units = 'in', compression = c('lzw'))
    plot(hcd,  ylab = "Height", nodePar = nodePar, edgePar = list(col = 2:3, lwd = 2:1))
    dev.off()
  }
  
  write.table(summtable, str_c(overlap.dir, '/../overlap_summary', table.postfix, '.csv'), sep = ';', row.names = FALSE, col.names = TRUE, dec = ',')
  
  return( return_fullyear_matrix )
}


overlap.analysis.bnpi = function ( f.df.en, table.postfix='', merge.hd=FALSE, merge.game=FALSE )
{
  f.df.o = f.df.en
  #f.df.o$Felvetel.tartalma[f.df.o$Felvetel.tartalma == "hiker"] <- "Human disturbance"
  #f.df.o$Felvetel.tartalma[f.df.o$Felvetel.tartalma == "rider"] <- "Human disturbance"
  #f.df.o$Felvetel.tartalma[f.df.o$Felvetel.tartalma == "MV"] <- "Human disturbance"
  f.df.o$Felvetel.tartalma[f.df.o$Felvetel.tartalma == "hiker"] <- "Hiker"
  f.df.o$Felvetel.tartalma[f.df.o$Felvetel.tartalma == "rider"] <- "Rider"
  #f.df.o$Felvetel.tartalma[f.df.o$Felvetel.tartalma == "MV"] <- "Human disturbance"
  #f.df.overlap <- f.df.overlap[f.df.overlap$Felvetel.tartalma != "Vulpes vulpes", ]
  f.df.o <- f.df.o[f.df.o$Felvetel.tartalma != "Felis silvestris", ]
  f.df.o <- f.df.o[f.df.o$Felvetel.tartalma != "Rider", ]
  
  if (merge.hd)
  {
    f.df.o$Felvetel.tartalma[f.df.o$Felvetel.tartalma == "MV"] <- "Human disturbance"
    f.df.o$Felvetel.tartalma[f.df.o$Felvetel.tartalma == "Hiker"] <- "Human disturbance"
    f.df.o$Felvetel.tartalma[f.df.o$Felvetel.tartalma == "Rider"] <- "Human disturbance"
  }
  
  if (merge.game)
  {
    f.df.o$Felvetel.tartalma[f.df.o$Felvetel.tartalma == "Cervus elaphus"] <- "Ungulates"
    f.df.o$Felvetel.tartalma[f.df.o$Felvetel.tartalma == "Capreolus capreolus"] <- "Ungulates"
    f.df.o$Felvetel.tartalma[f.df.o$Felvetel.tartalma == "Sus scrofa"] <- "Ungulates"
  }
  
  f.df.o = f.df.o[,c('Helyszin', 'Datum', 'Felvetel.kezdete', 'Felvetel.tartalma')]
  
  cat("Compute overlap (Dhat4) matrix\n")
  olap.matrix = overlap.analysis.mx( f.df.o, table.postfix )

  cat("Plot NMDS and clustering\n")
  nmds.analysis(olap.matrix)
  
  return (olap.matrix)
}


overlap.analysis.cserkesz = function ( f.cs, table.postfix='' )
{
  overlap.analysis( f.cs, table.postfix )
}


overlap.analysis.full = function ( f.df.en, f.cs, table.postfix='' )
{
  f.df.o = f.df.en
  f.df.o$Felvetel.tartalma[f.df.o$Felvetel.tartalma == "hiker"] <- "Human disturbance"
  f.df.o$Felvetel.tartalma[f.df.o$Felvetel.tartalma == "rider"] <- "Human disturbance"
  f.df.o$Felvetel.tartalma[f.df.o$Felvetel.tartalma == "MV"] <- "Human disturbance"
  #f.df.overlap <- f.df.overlap[f.df.overlap$Felvetel.tartalma != "Vulpes vulpes", ]

  f.df.o = f.df.o[,c('Helyszin', 'Datum', 'Felvetel.kezdete', 'Felvetel.tartalma')]

  f.df.o = rbind(f.df.o, f.cs)
  
  overlap.analysis( f.df.o, table.postfix )
}


nmds.analysis = function ( olap.matrix )
{
  library(ecodist)
  library(ggplot2)
  library(vegan)
  library(smerc)
  
  if (length(sys.nframe()) > 1)
  {
    caller.str = deparse(sys.calls()[[sys.nframe()-1]], nlines = 1)
    if ( str_detect(caller.str, 'bnpi') )
      subdir = 'bnpi'
    else if ( str_detect(caller.str, 'cserkesz') )
      subdir = 'cserkesz'
    else if ( str_detect(caller.str, 'full') )
      subdir = 'full'
    else
      subdir = 'other'
  } else
  {
    subdir = 'other'
  }
      
  
  overlap.dir = str_c( figdir.olap, subdir, "full_year", '/', sep = '/')
  dir.create(overlap.dir, showWarnings = FALSE, recursive = TRUE)
  
  # Compute ideal number of clusters with elbow method
  wss <- numeric(length = nrow(olap.matrix))
  for (i in 1:(nrow(olap.matrix)-1)) {
    cat(i)
    kmeans_model <- kmeans(olap.matrix, centers = i)
    wss[i] <- sum((olap.matrix - kmeans_model$centers[kmeans_model$cluster,])^2)
  }
  
  # Find elbow point
  elbow_point <- elbow_point(x=1:length(wss),y=wss)
  
  # Plot inertia changes by number of clusters
  tiff(str_c(overlap.dir, paste0('nmds_elbow.tiff')), width = 6, height = 4, res = 300, units = 'in', compression = c('lzw'))
  plot(1:nrow(olap.matrix), wss, type = "b", pch = 19, frame = FALSE, main = "Elbow Method",
       xlab = "Number of Clusters", ylab = "Within Sum of Squares")
  elbow_point
  points(elbow_point$x, elbow_point$y, col = "red", cex = 2, pch = 4)
  text(elbow_point$x, elbow_point$y, labels = sprintf("   %d", elbow_point$idx), pos = 4)
  dev.off()
  
  # Optimal number of clusters
  optimal_clusters <- elbow_point$idx
  cat("Optimal number of clusters: ", optimal_clusters, "\n")
  
  # Create clustering tree
  distance_matrix <- as.dist(olap.matrix)
  hierarchical_clustering <- hclust(distance_matrix, method = "ward.D2")

  # Set cluster colors on dendrogram
  gg_color_hue <- function(n) {
    hues = seq(15, 375, length = n + 1)
    hcl(h = hues, l = 65, c = 100)[1:n]
  }
  cols = gg_color_hue(optimal_clusters)
  nodePar <- list(lab.cex = 0.6, pch = c(NA, 19), cex = 0.7, col = "blue")
  tiff(str_c(overlap.dir, paste0('nmds_dendrogram.tiff')), width = 6, height = 6, res = 300, units = 'in', compression = c('lzw'))
  plot(as.dendrogram(hierarchical_clustering, hang = -1), main = "Dhat4 distances (Ward.D2)", xlab = "", sub = NULL,
       nodePar = nodePar, edgePar = list(col = 2:3, lwd = 2:1))
  rect.hclust(hierarchical_clustering, k = optimal_clusters, border = cols) #2:optimal_clusters)
  dev.off()
  
  # Update NMDS plot with optimal number of clusters
  nmds_result_optimal <- metaMDS(distance_matrix, k = 2)
  
  # Adding clusters
  nmds_result_optimal$clust <- cutree(hierarchical_clustering, k = optimal_clusters)
  
  # Plotting
  g <- ggplot(data = data.frame(nmds_result_optimal$points, clust = factor(nmds_result_optimal$clust)),
              aes(x = nmds_result_optimal$points[, 1], y = nmds_result_optimal$points[, 2], color = clust, label = row.names(nmds_result_optimal$points))) +
    geom_point(aes(shape = clust), size = 3) +
    geom_text(size = 3, hjust = -0.1, vjust = 0) +
    xlim(1.1*min(nmds_result_optimal$points[,'MDS1']), 1.1*max(nmds_result_optimal$points[,'MDS1'])) +
    ylim(1.1*min(nmds_result_optimal$points[,'MDS2']), 1.1*max(nmds_result_optimal$points[,'MDS2'])) +
    ggtitle("") + xlab('NMDS1') + ylab('NMDS2') +
    theme_bw() + theme(legend.position="none")
  ggsave(str_c(overlap.dir, paste0('nmds_plot.tiff')), width = 6, height = 5, dpi=300, units = "in", compression = "lzw")
  return(g)
}
