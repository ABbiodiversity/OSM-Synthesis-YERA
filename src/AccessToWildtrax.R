
AccessToWildtrax=function(Station) {
  OUT=Station
  for(i in 1:length(unique(Station))) {
    S=unique(Station)[i]
    x=strsplit(S, '-')[[1]]
    if(length(x)==3) {
      #Remove leading zeroes from Site Name
      x[2]=as.character(as.numeric(x[2]))
      #Combine with separator of :
      Converted=paste(x[1], x[2], x[3], sep=":")
    } else {
      if(length(x)==4) {
        #Remove leading zeroes from Site Name
        x[2]=as.character(as.numeric(x[2]))
        x[3]=as.character(as.numeric(x[3]))
        #Combine with : and @
        Converted=paste0(x[1], ':', x[3], '@', x[2], ':', x[4])
      } else {
        Converted=NA
      }
    }
    OUT[OUT==S]=Converted
  }
  return(OUT)
}




