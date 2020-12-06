
pwd<-function(...){(getwd(...))}
cd<-function(...){setwd(...)}
ll<-function(...){
"pattern=NULL,all.files=FALSE,full.names=FALSE,recursive=FALSE,ignore.case=FALSE,include.dirs=FALSE,no..=FALSE"
  list.files(...)
}


###•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•###
###•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•###



Load<-function(obj_path,...){
##   verbose=T works better in a way but loads the obj to parent frame
#  obj=ls()
#  obj=ls()
#  all_obj=load(obj_path)
 # print(as.matrix(sort(ls()[!(ls() %in% obj)]),decreasing=T))
#  print(parent.frame())
#  if(exists("readme")){cat(readme)}  # needs to be the one read-in
 return(as.matrix(sort(load(obj_path, parent.frame(),...),decreasing=F)))

#  return(load(obj_path, parent.frame()))
}


naln<-function(dat,otr=' .,!;-',replace='_'){
##  replace all non alpha-numeric chars with "replace"
##  otr - optional add other punctuation eg ' _.,!'
  return(invisible(gsub(paste0('[^A-Za-z0-9',otr,']'),replace,dat)))
}


read.file<-function(file,fix_colnames=T,...){
    holder=read.table(file=file,quote="",colClasses="character",check.names=F,comment.char="",...)
    if(fix_colnames){
      colnames(holder)=naln(colnames(holder))
    }
    return(holder)
}



write.file<-function(mat,file,row.names=T,col.names=T,missing.value.char="NA",sep="\t",...){
    if(col.names==T & row.names==T){col.names=NA}
    write.table(mat,file,row.names=row.names,col.names=col.names,sep=sep,quote=F,na=missing.value.char,...)
}



read.zip <- function(file,verbose=T,...){
## SOURCE :   http://stackoverflow.com/questions/8986818/automate-zip-file-reading-in-r
    zipFileInfo <- unzip(file, list=TRUE)
    if(nrow(zipFileInfo) > 1){
        stop(paste0("more than one data file inside ",file))
    }
    else{
        if(verbose){cat(' ',file,'\tcontains 1 file : ',as.character(zipFileInfo$Name),'\n')}
        read.csv(unz(file, as.character(zipFileInfo$Name)), ...)
    }
}

