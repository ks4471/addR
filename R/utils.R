
lcount<-function(k,length){
    cat('\t',k,'\t',round(k/length, digits=2), "\r");flush.console()
    return(k + 1)
}


lprogr<-function(xvar,xful,bars=F){
  dummy=which(xful==xvar)
	if(!bars){(cat('\t',xvar,'\t',dummy,' of ',length(xful),'\n'))}
	if(bars){cat('\n\t========================    ',xvar,dummy,'of',length(xful),'    ========================\n')}
  return(invisible(dummy))
}


frac<-function(subset,full,num=T,perc=F,sig_fig=2){
  if(!num){return(round(length(subset)/length(full),digits=sig_fig))}
  if(num&perc){return(round(subset/full,digits=sig_fig)*100)}
  if(num&!perc){return(round(subset/full,digits=sig_fig))}
}


trim<-function(dat){
##  returns string w/o leading or trailing whitespace
##  https://stackoverflow.com/questions/2261079/how-to-trim-leading-and-trailing-whitespace-in-r
  return(invisible(gsub("^\\s+|\\s+$", "", dat)))
}


cats<-function(indent=1,...){
  cat(rep('\t',times=indent),...,'\n')
}
