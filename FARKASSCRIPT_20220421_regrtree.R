
measure.trappings.per.section.and.transform = function( f.df, method = 'daily', standardize = TRUE )
{
  
  tbl = as.data.frame.matrix(table(f.df$section.ID, f.df$Felvetel.tartalma))

  if ( startsWith( 'daily', method ) )
  {
    tbl.hosszak = as.data.frame.matrix(table(f.df$section.ID, f.df$section.length))
    szakasz.hosszak = as.numeric( apply(tbl.hosszak, 1, function(x){colnames(tbl.hosszak)[which(x>0)]}) )
    tbl = tbl / szakasz.hosszak
  }
  else if ( startsWith( 'percentage', method ) )
    tbl = tbl / rowSums(tbl) * 100
  else if ( startsWith( 'count', method ) )
    tbl = tbl
  else
    cat("A method parameter daily, percentage, vagy count lehet.\n")
  
  if (standardize)
  {
    tbl = t(tbl)
    tbl = as.data.frame( t(( tbl - rowMeans(tbl) ) / apply(tbl, 1, sd)) )
  }

  tbl.nyar = as.data.frame.matrix(table(f.df$section.ID, f.df$nyar))
  tbl$summer = as.numeric( ((tbl.nyar[,2]-tbl.nyar[,1]) > 0) )
    
  #colnames(tbl) = c("rider", "Canis", "Capreolus", "Cervus", "hiker", "MV", "Sus", "Vulpes", "summer")
  colnames(tbl) = c("rider", "Canis", "Capreolus", "Cervus", "Felis", "hiker", "Meles", "MV", "Sus", "Vulpes", "summer")
  
  return(tbl)
  
}


mixing.variables = function( tbl.variables, tbl.target, target )
{
  tbl.variables[,target] = tbl.target[,target]
  return( tbl.variables )
}


regression.tree.crossvalidate = function( tbl.var, tbl.tar = NULL, kfolds = 10 )
{
  #Mivel az ertekek nem kategoria valtozok, a modszer pontossagat root-mean-square-error ertekkel jellemezzuk
  # cross validation k-fold choice - https://stats.stackexchange.com/questions/27730/choice-of-k-in-k-fold-cross-validation
  #RMSE - https://towardsdatascience.com/what-does-rmse-really-mean-806b65f2e48e
  #R2 - https://blog.minitab.com/blog/adventures-in-statistics-2/regression-analysis-how-do-i-interpret-r-squared-and-assess-the-goodness-of-fit
  #R2 criticism - https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4530125/
  library(partykit)
  library(caret)
  library(e1071)
  
  crossmeasures = NULL
  
  for ( i in 1:kfolds )
  {
    testidx = sample(1:dim(tbl.var)[1], round(dim(tbl.var)[1]/10), replace = FALSE)
    #tbl.train = tbl[-testidx, ]
    #tbl.test  = tbl[testidx, ]
    
    # SEPARATE
    if (!is.null(tbl.tar)) tbl.mixed = mixing.variables(tbl.variables = tbl.var, tbl.target = tbl.tar, target = 'Cervus') else tbl.mixed = tbl.var
    tbl.train.cer = tbl.mixed[-testidx, ]
    tbl.test.cer = tbl.mixed[testidx, ]
    
    if (!is.null(tbl.tar)) tbl.mixed = mixing.variables(tbl.variables = tbl.var, tbl.target = tbl.tar, target = 'Sus') else tbl.mixed = tbl.var
    tbl.train.sus = tbl.mixed[-testidx, ]
    tbl.test.sus = tbl.mixed[testidx, ]
    
    if (!is.null(tbl.tar)) tbl.mixed = mixing.variables(tbl.variables = tbl.var, tbl.target = tbl.tar, target = 'Capreolus') else tbl.mixed = tbl.var
    tbl.train.cap = tbl.mixed[-testidx, ]
    tbl.test.cap = tbl.mixed[testidx, ]
    
    if (!is.null(tbl.tar)) tbl.mixed = mixing.variables(tbl.variables = tbl.var, tbl.target = tbl.tar, target = 'Canis') else tbl.mixed = tbl.var
    tbl.train.can = tbl.mixed[-testidx, ]
    tbl.test.can = tbl.mixed[testidx, ]
    
    if (!is.null(tbl.tar)) tbl.mixed = mixing.variables(tbl.variables = tbl.var, tbl.target = tbl.tar, target = 'Vulpes') else tbl.mixed = tbl.var
    tbl.train.vul = tbl.mixed[-testidx, ]
    tbl.test.vul = tbl.mixed[testidx, ]
    
    # TRAINING
    #nyarat kiszedtem a modellbol (megj.: lathatoan megsem...)
    m4b.cervus = partykit::ctree(Cervus    ~ Canis + hiker +  MV + rider + summer,                   data = tbl.train.cer, control = partykit::ctree_control(minsplit = 6, minbucket = 12, mincriterion = 0.95))
    m4b.sus    = partykit::ctree(Sus       ~ Canis + hiker +  MV + rider,                            data = tbl.train.sus, control = partykit::ctree_control(minsplit = 6, minbucket = 12, mincriterion = 0.95))
    m4b.cap    = partykit::ctree(Capreolus ~ Canis + Vulpes + hiker +  MV + rider,                   data = tbl.train.cap, control = partykit::ctree_control(minsplit = 6, minbucket = 12, mincriterion = 0.95))
    m4b.canis  = partykit::ctree(Canis     ~ Cervus + Capreolus + Sus + hiker + MV + rider + summer, data = tbl.train.can, control = partykit::ctree_control(minsplit = 6, minbucket = 12, mincriterion = 0.95))
    m4b.vulpes = partykit::ctree(Vulpes    ~ Canis + Capreolus + hiker + MV + rider,                 data = tbl.train.vul, control = partykit::ctree_control(minsplit = 6, minbucket = 12, mincriterion = 0.95))
    
    # PREDICTING
    predict.cervus = predict(m4b.cervus, newdata=tbl.test.cer)
    predict.sus    = predict(m4b.sus,    newdata=tbl.test.sus)
    predict.cap    = predict(m4b.cap,    newdata=tbl.test.cap)
    predict.canis  = predict(m4b.canis,  newdata=tbl.test.can)
    predict.vulpes = predict(m4b.vulpes, newdata=tbl.test.vul)
    
    crossmeasures = rbind( crossmeasures, data.frame(subject='Cervus', k=i, R2=R2(predict.cervus, tbl.test.cer$Cervus), cor.R=cor(predict.cervus, tbl.test.cer$Cervus), cor.p=cor.test(predict.cervus, tbl.test.cer$Cervus)$p.value) )
    crossmeasures = rbind( crossmeasures, data.frame(subject='Sus', k=i, R2=R2(predict.sus, tbl.test.sus$Sus), cor.R=cor(predict.sus, tbl.test.sus$Sus), cor.p=cor.test(predict.sus, tbl.test.sus$Sus)$p.value) )
    crossmeasures = rbind( crossmeasures, data.frame(subject='Capreolus', k=i, R2=R2(predict.cap, tbl.test.cap$Capreolus), cor.R=cor(predict.cap, tbl.test.cap$Capreolus), cor.p=cor.test(predict.cap, tbl.test.cap$Capreolus)$p.value) )
    crossmeasures = rbind( crossmeasures, data.frame(subject='Canis', k=i, R2=R2(predict.canis, tbl.test.can$Canis), cor.R=cor(predict.canis, tbl.test.can$Canis), cor.p=cor.test(predict.canis, tbl.test.can$Canis)$p.value) )
    crossmeasures = rbind( crossmeasures, data.frame(subject='Vulpes', k=i, R2=R2(predict.vulpes, tbl.test.vul$Vulpes), cor.R=cor(predict.vulpes, tbl.test.vul$Vulpes), cor.p=cor.test(predict.vulpes, tbl.test.vul$Vulpes)$p.value) )
    
    # crosseval.cervus = table(tbl.test$Cervus, predict.cervus)
    # crosseval.sus    = table(tbl.test$Sus, predict.sus)
    # crosseval.cap    = table(tbl.test$Capreolus, predict.cap)
    # crosseval.canis  = table(tbl.test$Canis, predict.canis)
    # crosseval.vulpes = table(tbl.test$Vulpes, predict.vulpes)
    # 
    # conmat.cervus = prop.table(crosseval.cervus, 1)
    # conmat.sus    = prop.table(crosseval.sus, 1)
    # conmat.cap    = prop.table(crosseval.cap, 1)
    # conmat.canis  = prop.table(crosseval.canis, 1)
    # conmat.vulpes = prop.table(crosseval.vulpes, 1)
    
    # TODO create n equal groups of actual values where n is the different prediction values, then recompute confusion matrix
    # TODO compute precision, recall, f1 score https://chatbotsmagazine.com/automatically-generate-a-precision-recall-and-a-confusion-matrix-for-your-nlp-chatbot-training-9c2a8dc37752
    
  }
  
  colnames(crossmeasures) = c('subject', 'k', 'R2', 'cor.R', 'cor.p')
  cat("overall cross-validation measures:\n")
  for (i in unique(crossmeasures$subject))
  {
    cms = subset(crossmeasures, subject == i)
    cat("\n", i, ":\tR2 = ", sqrt(sum(cms$R2^2)/kfolds), '\tcor = ', sqrt(sum(cms$cor.R^2)/kfolds), '\tcor.p = ', sqrt(sum(cms$cor.p^2)/kfolds))
  }
  
  write.table( crossmeasures, paste0(figdir.tree, 'crossvalidate.csv'), sep = ';', dec = ',', col.names = TRUE, row.names = TRUE )
  
  return(crossmeasures)
  
}


regression.tree.analysis = function( tbl.var, tbl.tar = NULL )
{
  
  library(partykit)
  library(caret)
  library(e1071)
  
  myterminallabel <- function(i) c(
    as.character(round(i$prediction, 2)),
    paste("n =", i$n),
    format(round(i$distribution/i$n, digits = 3), nsmall = 3)
  )
  mynodelabel <- function(i) c(
    as.character(round(i$prediction, 2))
  )
  
  if (!is.null(tbl.tar)) tbl = mixing.variables(tbl.variables = tbl.var, tbl.target = tbl.tar, target = 'Cervus') else tbl = tbl.var
  m3 = partykit::ctree(Cervus ~ Canis + hiker +  MV + rider + summer, data = tbl, control = partykit::ctree_control(minsplit = 6, minbucket = 12, mincriterion = 0.95))
  m3s = as.simpleparty(m3)
  tiff(paste0(figdir.tree, 'ctree_cervus_en.tiff'), width = 10, height = 8, res = 300, units = 'in', compression='lzw')
  plot(m3s, main = "Cervus elaphus", tp_args = list(FUN = myterminallabel), ep_args = list(justmin = 20, digits = 2))
  dev.off()
  
  if (!is.null(tbl.tar)) tbl = mixing.variables(tbl.variables = tbl.var, tbl.target = tbl.tar, target = 'Sus') else tbl = tbl.var
  m3 = partykit::ctree(Sus ~ Canis + hiker +  MV + rider, data = tbl, control = partykit::ctree_control(minsplit = 6, minbucket = 12, mincriterion = 0.95))
  m3s = as.simpleparty(m3)
  tiff(paste0(figdir.tree, 'ctree_sus_en.tiff'), width = 10, height = 8, res = 300, units = 'in', compression='lzw')
  plot(m3s, main = "Sus scrofa", tp_args = list(FUN = myterminallabel), ep_args = list(justmin = 20, digits = 2))
  dev.off()
  
  if (!is.null(tbl.tar)) tbl = mixing.variables(tbl.variables = tbl.var, tbl.target = tbl.tar, target = 'Capreolus') else tbl = tbl.var
  m3 = partykit::ctree(Capreolus ~ Canis + Vulpes + hiker +  MV + rider, data = tbl, control = partykit::ctree_control(minsplit = 6, minbucket = 12, mincriterion = 0.95))
  m3s = as.simpleparty(m3)
  tiff(paste0(figdir.tree, 'ctree_capreolus_en.tiff'), width = 10, height = 8, res = 300, units = 'in', compression='lzw')
  plot(m3s, main = "Capreolus capreolus", tp_args = list(FUN = myterminallabel), ep_args = list(justmin = 20, digits = 2))
  dev.off()
  
  if (!is.null(tbl.tar)) tbl = mixing.variables(tbl.variables = tbl.var, tbl.target = tbl.tar, target = 'Canis') else tbl = tbl.var
  m3 = partykit::ctree(Canis ~ Cervus + Capreolus + Sus + hiker + MV + rider + summer, data = tbl, control = partykit::ctree_control(minsplit = 6, minbucket = 12, mincriterion = 0.95))
  m3s = as.simpleparty(m3)
  tiff(paste0(figdir.tree, 'ctree_canis_en.tiff'), width = 10, height = 8, res = 300, units = 'in', compression='lzw')
  plot(m3s, main = "Canis lupus", tp_args = list(FUN = myterminallabel), ep_args = list(justmin = 20, digits = 2))
  dev.off()
  
  if (!is.null(tbl.tar)) tbl = mixing.variables(tbl.variables = tbl.var, tbl.target = tbl.tar, target = 'Vulpes') else tbl = tbl.var
  m3 = partykit::ctree(Vulpes ~ Canis + Capreolus + hiker + MV + rider, data = tbl, control = partykit::ctree_control(minsplit = 6, minbucket = 12, mincriterion = 0.95))
  m3s = as.simpleparty(m3)
  tiff(paste0(figdir.tree, 'ctree_vulpes_en.tiff'), width = 10, height = 8, res = 300, units = 'in', compression='lzw')
  plot(m3s, main = "Vulpes vulpes", tp_args = list(FUN = myterminallabel), ep_args = list(justmin = 20, digits = 2))
  dev.off()
  
}
