
generate.section.ID = function( f.df, maximum.szunet.ket.mappa.osszavonasahoz = 7 )
{
  
  szakaszID <- paste(as.character(f.df$Helyszin), as.character(f.df$Start), sep=":")
  maxszun = maximum.szunet.ket.mappa.osszavonasahoz
  
  leghosszabb.szunet <- tapply(f.df$Datum, szakaszID, function(d) as.numeric(max(diff(d))))
  f.df2 <- NULL
  for( h in unique(f.df$Helyszin) )
  {
    f.sub <- subset(f.df, Helyszin == h)
    f.sub$section.ID <- ''
    folytatas <- tapply(f.sub$Stop, f.sub$Start, function(s) any(abs(as.numeric(difftime(s, f.sub$Start))) < maxszun) )
    #break
    elozo.szakasz <- FALSE
    for( f in names(folytatas) )
    {
      if ( elozo.szakasz == FALSE && any(abs(as.numeric(difftime(f.sub$Stop[which(f.sub$Start == as.Date(f))], f.sub$Stop))) < maxszun) )
        trapping.nev <- paste(h, f, sep = ':')
      f.sub$section.ID[which(f.sub$Start == as.Date(f))] <- trapping.nev
      elozo.szakasz <- folytatas[f]
    }
    f.df2 = rbind(f.df2, f.sub)
  }
  f.df = f.df2
  rm(f.df2)
  rm(f.sub)
  
  return(f.df)
}

estimate.optimal.subsection.length = function( f.df, method, mindays = 7, maxdays = 28 )
{
  library(NMOF)
  
  egyenletes.szakaszolas = function(x, data)
  {
    naps = tapply(data$abs.time, data$section.ID, function(y) 1 + floor(((y-min(y))/(x*86400))))
    naps2 = naps[ unique(data$section.ID) ]
    napsa = unlist(naps2)
    data$section.ID = str_c(data$section.ID, napsa)
    coefficient.of.variance = sd(table(data$section.ID)) / mean(table(data$section.ID))
    return( coefficient.of.variance )
  }
  egyenletes.szakaszolas.eszlelesszam = function(x, data)
  {
    naps = tapply(data$abs.time, data$section.ID, function(y) 1 + floor(((y-min(y))/(x*86400))))
    trappings = unlist(lapply(naps, table))
    coefficient.of.variance = sd(trappings) / mean(trappings)
    return( coefficient.of.variance )
  }
  
  if ( startsWith( 'length', method ) )
    napok.optimalis = gridSearch( egyenletes.szakaszolas, list(seq(mindays,maxdays)), data = f.df )
  else if ( startsWith( 'count', method ) )
    napok.optimalis = gridSearch( egyenletes.szakaszolas.eszlelesszam, list(seq(mindays,maxdays)), data = f.df )
  else
    cat("A method parameter length, vagy count lehet.\n")
  
  cat("Optimal section length: ", napok.optimalis$minlevels[1], "\n")
  
  return(napok.optimalis$minlevels[1])
}

split.to.subsections = function( f.df, napok )
{
  if (napok <= 0)
  {
    cat("napok parameter legyen pozitiv egesz szam!\n")
    return(-1)
  }
  
  f.df2 = NULL
  for (s in unique(f.df$section.ID))
  {
    f.sub = subset(f.df, section.ID == s)
    f.sub$section.length = napok
    
    rng = as.numeric( difftime(max(f.sub$Stop), min(f.sub$Start), units = 'days') )
    whole = floor(rng / napok) * napok
    fragm = rng - whole
    
    dates = as.numeric( difftime(f.sub$Datum, min(f.sub$Start), units = 'days') )
    
    f.sub$section.length[which( dates >= whole )] = fragm
    f.sub$section.ID = paste( f.sub$section.ID, floor(dates / napok) + 1, sep = ':')
    
    f.df2 = rbind(f.df2, f.sub)
  }
  rm(f.sub)
  
  return(f.df2)
}

remove.short.subsections = function( f.df, minnapok = 7 )
{
  return (f.df[which(f.df$section.length >= minnapok),])
}

sectioning = function( f.df, autolength = TRUE, automethod = 'count', napok = 14, minnapok = NA )
{
  # Forming continuous SECTIONS
  f.df = generate.section.ID( f.df )
  
  # Split SECTIONS to uniform length SUBSECTIONS
  if (autolength)
    napok = estimate.optimal.subsection.length( f.df, method = automethod )
  else
    cat("Selected section length: ", napok, "\n")
  
  f.df = split.to.subsections( f.df, napok )
  if (!is.na(minnapok))
    if (minnapok < napok)
      f.df = remove.short.subsections( f.df, minnapok )
  
  return(f.df)
}
