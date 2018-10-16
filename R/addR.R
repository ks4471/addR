#https://github.com/RamsinghLab/ozymandias
# People rarely do what they know to be right; they do what is convenient, then repent.. Therefore, if you want people (yourself, for example) to do the right thing, make it the most convenient thing

# ####============================================================================================================
# ##  wgcna clustering  ----------------------
# #╔═╗╔═╦╗╔═╦═╦╦╦╦╗╔═╗╔╗═╦╗╔═╦╗╗╔╦╗╔═╗╔═╦╗╔═╦═╦╦╦╦╗╔═╗╔╗═╦╗╔═╦╗╗╔╦╗╔═╗╔═╦╗╔═╦═╦╦╦╦╗╔═╗╔╗═╦╗╔═╦╗╔╦╦╗
# options(stringsAsFactors=F);library(colorout);rm(list=ls());ls()#╚═╣║ ╚╣║¯\_(•_•)_/¯║╚╣╔╣╔╣║║║║╚╣
# options(menu.graphics=F)  #╦═╩╬╝╚╬╬╚╗═╚╩╗═╩╠╬╝╔═╗╬╚║╣══╣╦╬╬╗╠╔╗╔╣╣╝╝╣╠╔╠╚╔╔═╦╩╬╣╦╣╔╚╬╦╣╩╬╚╩╗╣╝╚╠╣
# source('~/Dropbox/PROJ/adds/adds.R') #╗╣╠═╩╠╣╠╬═╩╬╗╩╩║═╚╣╠╣╠╗╗╠╔║╩╬╠╝╣╬╔╬╬╚╦╝╔╗╩╠╚╝╠═╝╝╦╔═╚╠╝╣║ ║
# #╚═╝╩═╩╝╚═╩══╩═╩═╩═╩╝╩═╩╝╚═╩═╩═╩╝╚═╝╩═╩╝╚══╩═╩╩══╩╩═╝╩═╩╝╚═╩═╩═╩╝╚═╝╩═╩╝ ╚═╩══╩═╩══╩╝╩═╩╝╚═╩═╩╩═╝

####============================================================================================================
##  signed v unsigned GTEX7 beta10 ----------------------
#╔═╗╔═╦╗╔═╦═╦╦╦╦╗╔═╗╔╗═╦╗╔═╦╗╗╔╦╗╔═╗╔═╦╗╔═╦═╦╦╦╦╗╔═╗╔╗═╦╗╔═╦╗╗╔╦╗╔═╗╔═╦╗╔═╦═╦╦╦╦╗╔═╗╔╗═╦╗╔═╦╗╔╦╦╗
# options(stringsAsFactors=F);library(colorout);rm(list=ls());ls()#╚═╣║ ╚╣║¯\_(•_•)_/¯║╚╣╔╣╔╣║║║║╚╣
# options(menu.graphics=F)  #═╦╩╬╝╚╬╬╚╗═╚╩╗═╩╠╬╝╔╚╗╬╚║╣══╣╦╬╬╗╠╔╗╔╣╣╝╝╣╠╔╠╚╔╔═╦╩╬╣╦╣╔╚╬╦╣╩╬╚╩╗╣╝╚╠╣
# library('adds') #╔╦╣═╩╚╣║╔╔╣╦═║║╔╗║╔╚╔╣╩╚╚╦╣║╩╔╦║║ ╚╩╣╚╚╣║╣╚╩╔╦╩╚╦╚╩╣╬╝╚╗╔╝╬╚╝ ╔╣═╦╦╦╩╠╔╠╗╔╝╚═╗╩║
# devtools::install_github("ks471/addR") #╗╣╠═╩╠╣╠╬═╚╬╗╩╩═║╚╝╝╣╠╗╗╠╔║╩╬╠╝╣╬╔╬╬╚╦╝╔╗╩╠╚╝╠═╝╝╦╔═╚╠╝╣║
#╚═╝╩═╩╝╚═╩══╩═╩═╩═╩╝╩═╩╝╚═╩═╩═╩╝╚═╝╩═╩╝╚═╩══╩═╩═╩═╩╝╩═╩╝╚═╩═╩═╩╝╚═╝╩═╩╝╚═╩══╩═╩═╩═╩╝╩═╩╝╚═╩═╩╩═╝


#### <*=-=*> ####

####======================================================================================================
##  adds functions : helper v2 --------------------------------
##   + standard vocabulary (attempt at anyhow)
##   + semblance of order
#╔═╗╔═╦╗╔═╦═╦╦╦╦╗╔═╗╔╗═╦╗╔═╦╗╗╔╦╗╔═╗╔═╦╗╔═╦═╦╦╦╦╗╔═╗╔╗═╦╗╔═╦╗╗╔╦╗╔═╗╔═╦╗╔═╦═╦╦╦╦╗╔═╗╔╗═╦╗╔═╦╗╔╦╦╗
# options(stringsAsFactors=F);library(colorout);rm(list=ls());ls()#╚═╣║ ╚╣║¯\_(•_•)_/¯║╚╣╔╣╔╣║║║║╚╣
# options(menu.graphics=FALSE);library(adds)#╣═╩╚╣║╔╔╣╦═║║╔╚║╔╚╔╣╩╚╚╦╣║╩╔╦║║ ╚╩╣╚╚╣║╣╚╩╔╦╩╚╦╚╩╣
#╚═╝╩═╩╝╚═╩══╩═╩═╩═╩╝╩═╩╝╚═╩═╩═╩╝╚═╝╩═╩╝╚═╩══╩═╩═╩═╩╝╩═╩╝╚═╩═╩═╩╝╚═╝╩═╩╝╚═╩══╩═╩═╩═╩╝╩═╩╝╚═╩═╩╩═╝

####============================================================================================================
##  signed v unsigned GTEX7 beta10 ----------------------
#╔═╗╔═╦╗╔═╦═╦╦╦╦╗╔═╗╔╗═╦╗╔═╦╗╗╔╦╗╔═╗╔═╦╗╔═╦═╦╦╦╦╗╔═╗╔╗═╦╗╔═╦╗╗╔╦╗╔═╗╔═╦╗╔═╦═╦╦╦╦╗╔═╗╔╗═╦╗╔═╦╗╔╦╦╗
# options(stringsAsFactors=F);library(colorout);rm(list=ls());ls()#╚═╣║ ╚╣║¯\_(•_•)_/¯║╚╣╔╣╔╣║║║║╚╣
# options(menu.graphics=F)  #═╦╩╬╝╚╬╬╚╗═╚╩╗═╩╠╬╝╔╚╗╬╚║╣══╣╦╬╬╗╠╔╗╔╣╣╝╝╣╠╔╠╚╔╔═╦╩╬╣╦╣╔╚╬╦╣╩╬╚╩╗╣╝╚╠╣
# library(R.helper) #╣═╩╚╣║╔╔╣╦═║║╔╗║╔╚╔╣╩╚╚╦╣║╩╔╦║║ ╚╩╣╚╚╣║╣╚╩╔╦╩╚╦╚╩╣╬╝╚╗╔╝╬╚╝ ╔╣═╦╦╦╩╠╔╠╗╔╝╚═╗╩║
# source('/Data/ks/adds.R')   # ╠╚╚╝═╝ ╚╬╗╦═╠║═╔╣═╬╠╝═║╚╣╦═╚╦╦╣╣╝╝╦╣╦╦╔║║╝╝═║═║╗╣╗╔╠╩╝═╗╝║╬═╔╔╔╣╚╩╣
# source('~/Dropbox/PROJ/adds/adds.R') #╗╣╠═╩╠╣╠╬═╚╬╗╩╩═║╚╝╝╣╠╗╗╠╔║╩╬╠╝╣╬╔╬╬╚╦╝╔╗╩╠╚╝╠═╝╝╦╔═╚╠╝╣║ ║
#╚═╝╩═╩╝╚═╩══╩═╩═╩═╩╝╩═╩╝╚═╩═╩═╩╝╚═╝╩═╩╝╚═╩══╩═╩═╩═╩╝╩═╩╝╚═╩═╩═╩╝╚═╝╩═╩╝╚═╩══╩═╩═╩═╩╝╩═╩╝╚═╩═╩╩═╝


####■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
##
#devtools::install_github("ks471/R.helper")
#

## devtools appears to be deprecated..
# usethis::create_package('~/Dropbox/PROJ/adds/github/spatiotemp/')
# Load('~/Dropbox/PROJ/adds/github/spatiotemp/data/spatiotemp_db.GSE25219_rma_pli_dabg.sampid.lm_sva_clust.Rdata')
# cd('~/Dropbox/PROJ/adds/github/spatiotemp/')
# usethis::use_data(spatiotemp_db)    ## save to wd, create_package() will change the directory


##
#/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■

# Format number as fixed width, with leading zeros [duplicate]
# There are several solutions to this.
# One of them is to use sprintf. This uses C style formatting codes embedded in a character string to indicate the format of any other arguments passed to it. For example, the formatting code %3d means format a number as integer of width 3:

# a <- seq(1,101,25)
# sprintf("name_%03d", a)
# [1] "name_001" "name_026" "name_051" "name_076" "name_101"
# Another is formatC and paste:

# paste("name", formatC(a, width=3, flag="0"), sep="_")
# [1] "name_001" "name_026" "name_051" "name_076" "name_101"



#╬═╬═╬═╬═╬═╬═╬═╬═╬═╬═╬═╬═╬═╬═╬═╬═╬═╬═╬═╬═╬═╬═╬═╬═╬═╬═╬═╬═╬═╬═╬═╬═╬═╬═╬═╬═╬═╬═╬═╬═╬═╬═╬═╬═╬═╬═╬═╬═╬═╬═╬═╬═╬═╬═╬═╬═╬═╬═╬═╬═

#gsub("[^A-Za-z0-9 _.,!]", "", humpty$title)  ## anything other than alnum & selected punctuation

# mtext("(c)",side=3,line=-1.5, at=par("usr")[1]+0.05*diff(par("usr")[1:2]),cex=1.2)      #  places text one-twentieth of the way across the "x-axis"

# R smallest number 9.88131291682493e-324
###•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•###
###•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•###
# radiant power - controlled, loving, compassionate, unyielding

colmix=c(
"#0072B2"
,"#E69F00"
,"#009E73"
,"#56B4E9"
,"#D55E00"
,"#66A61E"
,"#7570B3"
,"#a50f15"
,"#A6761D"
,"#117733"
,"#332288"
,"#b15928"
,"#882255"
,"#999933"
,"#AA4499"
,"#1f78b4"
,"#F0E442"
,"#e31a1c"
,"#6a3d9a"
,"#b2df8a"
,"#08519c"
,"#ff7f00"
,"#fdbf6f"
,"#33a02c"
,"#b15928"
,"#f16913"
,"#238b45"
,"#807dba"
,"#d94801"
,"#41ab5d"
,"#fd8d3c"
,"#4292c6"
)

pastelcolmix=c(
"#cab2d6"
,"#ffff99"
,"#8dd3c7"
,"#ffffb3"
,"#bebada"
,"#fb8072"
,"#80b1d3"
,"#fdb462"
,"#b3de69"
,"#fccde5"
,"#d9d9d9"
,"#bc80bd"
,"#ccebc5"
,"#ffed6f"
,"#a6cee3"
,"#fb9a99"
)


colmixrb=c(colorRampPalette(c(
"#3f007d"
,"#313695"
,"#053061"
,"#08306b"
,"#045a8d"
,"#08519c"
,"#2171b5"
,"#6baed6"
))(8),rev(colorRampPalette(c(
"#67001f"
,"#67000d"
,"#a50f15"
,"#bd0026"
,"#cb181d"
,"#e31a1c"
,"#ef3b2c"
,"#f4a582"
))(8)))

colmixb=c(colorRampPalette(c(
"#3f007d"
,"#313695"
,"#053061"
,"#08306b"
,"#045a8d"
,"#08519c"
,"#2171b5"
,"#6baed6"
))(8))

colmixr=rev(colorRampPalette(c(
"#67000d"
,"#a50f15"
,"#bd0026"
,"#cb181d"
,"#e31a1c"
,"#ef3b2c"
,"#f4a582"
))(7))


colred=c('#67001f','#a50026','#f46d43','#fb9a99')
colblu=c('#2d004b','#053061','#2166ac','#4393c3')
colblus=c('#08306b','#08519c','#2171b5','#4292c6','#6baed6','#9ecae1','#c6dbef','#deebf7','#f7fbff')
colreds=c('#67000d','#a50f15','#cb181d','#ef3b2c','#fb6a4a','#fc9272','#fcbba1','#fee0d2','#fff5f0')
colrbd  =c('#320000',"#800026","#bd0026","#e31a1c","#fc4e2a","#fd8d3c","#feb24c","#fed976"  # reds
         ,"#c7e9b4","#7fcdbb","#41b6c4","#1d91c0","#225ea8","#253494","#081d58",'#49006a')  # blues # dark purple , -- need a corresponding smth for the darkest red


colrb  =c(
"#67001f"
,"#b2182b"
,"#d6604d"
,"#f4a582"
,"#fddbc7"
,"#f7f7f7"
,"#d1e5f0"
,"#92c5de"
,"#4393c3"
,"#2166ac"
,"#053061"
)

colrbyg  =c("#800026","#bd0026","#e31a1c","#fc4e2a","#fd8d3c","#feb24c","#fed976",'white'
         ,"#c7e9b4","#7fcdbb","#41b6c4","#1d91c0","#225ea8","#253494","#081d58")  # blues # dark purple , -- need a corresponding smth for the darkest red



colbw=c("#ffffff","#f0f0f0","#d9d9d9","#bdbdbd","#969696","#737373","#525252","#252525","#000000")


Library<-function(pkg_name='',cran=F){
##  check for package existence, if not install
##  only for packages on cran/bioconductor
	options(menu.graphics=FALSE)

	if(pkg_name==''){stop('no package name specified')}
	if(length(pkg_name)>1){stop('one package name only')}
	
	if(!(pkg_name %in% rownames(installed.packages()))){
		if(!cran){
			cat('\tbioconductor install :  ',pkg_name,'\n')
      source("https://bioconductor.org/biocLite.R")
			biocLite(pkg_name,suppressAutoUpdate=T,suppressUpdates=T)
		}
		if(cran){
			cat('\tcran install :  ',pkg_name,'\n')
			install.packages(pkg_name)
		}
	}
	library(pkg_name,character.only=T,quietly=T)
}



install.dependencies<-function(){
##  USE : re-install packages commonly used by adds.R functions, particularly important for github ones. Library() can handle bioconductor / cran
##  NOTE: example error below means the mirror used is not functional/unavailable, try another..

	options(menu.graphics=FALSE)
	system('git clone https://github.com/jalvesaq/colorout.git')
	system('sudo R CMD INSTALL colorout')
	library(colorout)

  devtools::install_github("hadley/devtools")
	Library('devtools')
	Library('gplots')

##  installing WGCNA - requires dependencies first - simplest is to use instruction as per website :
##  http://labs.genetics.ucla.edu/horvath/CoexpressionNetwork/Rpackages/WGCNA/#cranInstall
	source("http://bioconductor.org/biocLite.R") 
	biocLite(c("AnnotationDbi", "impute", "GO.db", "preprocessCore")) 
	Library("WGCNA")
  library(devtools)
  install_github(repo="juanbot/km2gcn/km2gcn")    ## re-clustering WGCNA modules package 

	Library('MetaDE')
	Library('mixtools')
	Library('pamr')
	Library("psych")
	Library('corrplot')

	Library('gplots')
	Library('ggplot2')
	Library('pvclust')
	Library('dendextend')

	Library('minet')
	Library('limma')
	Library('biomaRt')


  install_github("vqv/ggbiplot")
  devtools::install_github("slowkow/ggrepel")

}



###•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•###
###•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•###

pwd<-function(...){(getwd(...))}
cd<-function(...){setwd(...)}
ll<-function(...){
##  pattern=NULL,all.files=FALSE,full.names=FALSE,recursive=FALSE,ignore.case=FALSE,include.dirs=FALSE,no..=FALSE
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



read.file<-function(file,...){
  read.table(file=file,quote = "",colClasses = "character",check.names=F,comment.char="",...)
}



write.file<-function(mat,file,row.names=T,col.names=T,missing.value.char="NA",sep="\t",...){
  if(col.names==T & row.names==T){
    col.names=NA
  }
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



lcount<-function(k,length){
    cat('\t',k,'\t',round(k/length, digits = 2), "\r");flush.console()
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


naln<-function(dat,otr=' _.,!',replace=''){
##  replace all non alpha-numeric chars with "replace"
##  otr - optional add other punctuation eg ' _.,!'
  return(invisible(gsub(paste0('[^A-Za-z0-9',otr,']'),replace,dat)))
}


cats<-function(indent=1,...){
  cat(rep('\t',times=indent),...,'\n')
}

###•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•###
###•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•###



get.duplicates<-function(dat_mat,col_dup,...){
  ndup=sort(table(dat_mat[,col_dup]),decreasing=T)
# ndup=sum(duplicated(dat_mat[,col_dup]))
#   Head(ndup)
  ndup=ndup[ndup>1]

  if(length(ndup)<1){
    cat("\tno duplicates found in column :",col_dup,"\n")
    return(invisible(list(n.duplicated="",duplicates="",unique=dat_mat)))
  }

  if(length(ndup)>=1){
  #   Head(ndup)
    cat("\t",length(ndup),"duplicates found, top duplicates:\n")
    print(as.matrix(ndup[1:min(15,length(ndup))]))
  
    dat_dup=dat_mat[(dat_mat[,col_dup]%in%names(ndup)),]
    dat_dup=unique(dat_dup[order(dat_dup[,col_dup]),])
  
    dat_unq=unique(dat_mat[!(dat_mat[,col_dup]%in%names(ndup)),])
  
  return(invisible(list(n.duplicated=ndup,duplicates=dat_dup,unique=dat_unq)))
  }
}



overlap<-function(A,B,n=5){
##  modified to run only for unique A & B, otherwise numbers can be misleading
    unA=unique(A)
    unB=unique(B)
	    cat('\n\tlength(A) :  ',length(A),'\t unique(A)  :  ',length(unA),' \t',round(length(unA)/length(A),digits=3)*100,'%\n')
	    cat('\tlength(B) :  ',length(B),'\t unique(B)  :  ',length(unB),' \t',round(length(unB)/length(B),digits=3)*100,'%','\n')


    both=union(unA, unB)
    intr=intersect(unA,unB)
    inA=both %in% unA
    inB=both %in% unB
    cat('\n')
    print(table(inA, inB))

    exA=unA[!(unA%in%intr)]
    exB=unB[!(unB%in%intr)]

    cat('\n\t',length(intr),'\tinA & inB :\t',paste(sort(intr[1:n])			  ,collapse=',  '),'\n')
    cat('\t',length(exA),'\tinA  notB :\t',	paste(sort(exA[1:n]),collapse=',  '),'\n')
    cat('\t',length(exB),'\tinB  notA :\t',	paste(sort(exB[1:n]),collapse=',  '),'\n')
    return(invisible(list(inter=intr,union=both,ina=exA,inb=exB)))

}



Table<-function(dat_mat,thresh=NA,sort=T,decreasing=T){
  if(sort){
    dummy=as.data.frame(sort(table(dat_mat),decreasing=decreasing))
  }
  if(!sort){
    dummy=as.data.frame((table(dat_mat)))
  }

  if(ncol(dummy)==1){
  	holder=as.data.frame(rownames(dummy))
  	holder$count=dummy[,1]
  	dummy=holder
  }
  colnames(dummy)=c('entry','count')
  dummy$fraction=round(dummy$count/sum(as.numeric(dummy$count)),digits=3)
  if(!is.na(thresh)){dummy=dummy[dummy$count>=thresh,]}
  return(dummy)
}



Head<-function(data_obj,nlist=1,ncol=1:5,nrow=1:10){
  cat("\n\tobject class : ",class(data_obj),"\n\n")
  prev=""

  if(class(data_obj)=="list"){
  	if(length(names(data_obj))<50){
	    cat("\t\tlist contains",length(names(data_obj)),"objects:\n")
	    cat("\t\t\t",as.matrix(names(data_obj)),"\n\n",sep="   ")
	    cat("\t\tlist[[",nlist,"]] contains ",class((data_obj[[1]]))," :",sep="")

	    prev=paste("list[[",nlist,"]] contains :",sep="")
	    data_obj=data_obj[[nlist]]
    }

  	if(length(names(data_obj))>50){
	    cat("\t\tlist contains",length(names(data_obj)),"objects, first 50:\n")
	    cat("\t\t\t",as.matrix(names(data_obj)[1:min(50,length(names(data_obj)))]),"...\n\n",sep="   ")
	    cat("\t\tlist[[",nlist,"]] contains ",class((data_obj[[1]]))," :",sep="")

	    prev=paste("list[[",nlist,"]] contains :",sep="")
	    data_obj=data_obj[[nlist]]
    }
  }

  if(class(data_obj)=="list"){
    cat("",length(names(data_obj)),"objects:\n")
    cat("\t\t\t",as.matrix(names(data_obj)),"\n\n",sep="   ")
  }

  if(class(data_obj)=="data.frame" | class(data_obj)=="matrix"){
    cat("\n\n")
    print(data_obj[min(1,min(nrow)):min(max(nrow),nrow(data_obj)),min(1,min(ncol)):min(max(ncol),ncol(data_obj)),drop=F])
    cat("\n\t",prev,class(data_obj),"dimensions : ",dim(data_obj),"\n")
    cat("\t\tis.numeric :",is.numeric(data_obj))
 
    if(is.numeric(data_obj)){
    	cat('\tmin=',min(data_obj,na.rm=T),'max=',max(data_obj,na.rm=T),'\n')
    }
    cat('\n')
  }

  if(class(data_obj)=="vector"|class(data_obj)=="character"){
    cat("\n\n")
    print(data_obj[nrow])
    cat("\n\t",prev,class(data_obj),"length : ",length(data_obj),"\n")
    cat("\t\tis.numeric :",is.numeric(data_obj))

    if(is.numeric(data_obj)){
    	cat('\tmin=',min(data_obj,na.rm=T),'max=',max(data_obj,na.rm=T),'\n')
    }
    cat('\n')
  }

  if(!(class(data_obj)%in%c("list","data.frame","matrix","vector","character"))){
    cat("\n\n")
    str(data_obj)
    cat("\n\tis.numeric :",is.numeric(data_obj))
    if(is.numeric(data_obj)){
    	cat('\tmin=',min(data_obj,na.rm=T),'max=',max(data_obj,na.rm=T),'\n')
    }
    cat('\n')
  }
# cat("\t",class(data_obj),"dimensions : ",dim(data_obj),"\n")
}



dat.class<-function(dat_mat,verbose=F){
  if(is.vector(dat_mat)){dat_class='vector'}
	if(is.matrix(dat_mat)){dat_class=apply(dat_mat,2,class)}
	if(is.data.frame(dat_mat)){dat_class=unlist(lapply(dat_mat,class))}
	if(is.list(dat_mat)&!is.data.frame(dat_mat)){stop('input is a list')}
	if(verbose){print(Table(dat_class))}
	return((dat_class))
}



Intersect <- function(a,b,...){
##  full credit to Abhishek K Baikady at http://stackoverflow.com/questions/3695677/how-to-find-common-elements-from-multiple-vectors
  Reduce(intersect, list(a,b,...))
}



Union <- function(a,b,...){
##  full credit to Abhishek K Baikady at http://stackoverflow.com/questions/3695677/how-to-find-common-elements-from-multiple-vectors
  Reduce(union, list(a,b,...))
}



distpc <- function(x,perc) ecdf(x)(perc)
##  estimate quantile of a value in a distribution
#    SOURCE:  http://stats.stackexchange.com/questions/50080/estimate-quantile-of-value-in-a-vector 
#    USE:   ecdf_fun(1:10,8)
#    NOTES:   this function looks really 'creepy', no brackets i get, but where is return?? how does (perc) work?? spooky..



make.numeric<-function(dat_mat,col_factor="",char_as_factor=F,verbose=F,help=F){
##  generate a new matrix, same dimensioins/order as dat_mat and all columns are as.numeric
## for any correlation analysis like linear modeling categorical variables eg 'sex' should always be converted to factor, numeric 1=male, 2=female is considered 2>1 vs 1!=2 if as.factor
##  col_factor  - column names to transform as factor instead, returns a data_frame
  # if(verbose==T){cat('\n')}
  cat('\n')
  if(sum(col_factor=="")!=1&char_as_factor){
    stop('if "col_factor" specified, set "char_as_factor=F"  (mutually exclusive)')
  }


  num_mat=matrix(NA,ncol=ncol(dat_mat),nrow=nrow(dat_mat))
    colnames(num_mat)=colnames(dat_mat)
    rownames(num_mat)=rownames(dat_mat)

  col_numeric=colnames(dat_mat)
  col_numeric=col_numeric[!col_numeric%in%col_factor]

  if(char_as_factor & sum(col_factor=="")==1){
    holder=dat.class(dat_mat)
    col_factor=names(holder[holder%in%c('factor','character')])
    col_numeric=col_numeric[!col_numeric%in%col_factor]

    if(verbose&char_as_factor){cat('\tall columns with character variables will be converted to factors\n')}
  }

  for(inum in col_numeric){
    # if(verbose==T){cat("\t\t - ",inum,"coverted as.numeric\n")}
      num_mat[,inum]=as.numeric(dat_mat[,inum])
  }
  if(sum(col_factor=="")!=1){
    # if(verbose==T){cat("\n\tNOTE:\tmatrix can only hold 1 kind of object eg numeric, character, etc..\n\t\t\tdata.frame will be used instead\n")}
    num_mat=as.data.frame(num_mat)

    for(ifac in col_factor){
      # if(verbose==T){cat("\t\t - column",ifac,"coverted as.factor\n")}
      num_mat[,ifac]=droplevels(as.factor(dat_mat[,ifac]))
    }
  }
  return(num_mat)
      # if(verbose==T){cat("\t---------------------   make.numeric - finished   ---------------------\n\n")}
}


# second.min<-function(dat_vec){
#   dat.class()
# }


is.even<-function(numvec){
  return(numvec %% 2 == 0)
}



spval<-function(dat_mat,value=2,mode='min'){
  if(mode=='min'){return(sort(unique(as.vector(unlist(dat_mat))))[value])}
  if(mode=='max'){return(sort(unique(as.vector(unlist(dat_mat))),decreasing=T)[value])}

}




###•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•###
###•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•###



colmix=c("#0072B2","#E69F00","#009E73","#56B4E9","#D55E00","#66A61E","#7570B3","#a50f15","#A6761D","#117733","#332288","#b15928","#882255","#999933","#AA4499","#1f78b4","#F0E442","#e31a1c","#6a3d9a","#b2df8a","#08519c","#ff7f00","#fdbf6f","#33a02c","#b15928","#f16913","#238b45","#807dba","#d94801","#41ab5d","#fd8d3c","#4292c6")
pchmix=rep(c(21:22),length=32)
rminval=9.88131291682493e-324   ## .Machine$double.xmin   (while this is ideal, does not seem to work..)


qqplot<-function(dat_mat){
  qqnorm(exp_mat,pch=20,col=rgb(0, 0, 0,alpha=0.2))
  qqline(exp_mat,col='dodgerblue')
  holder=shapiro.test(exp_mat)
    legend(x="bottomright",box.lwd=0,box.col="white",legend=paste0('shapiro.test  W=',round(holder$statistic,digits=2),'  P=',formatC(signif(holder$p.value),digits=2)))
}



hist.norm<-function(x,normCurv=T,col='lightgray',points=T,density=F,prob=F,...){  #density=F, # need to add some sort of axis scaling is not very compatible
#  breaks=max(10,sqrt(length(x)+100))
  y=hist(x,prob=prob,...)


  if(density){  # requires prob=TRUE for probabilities not counts
     if(!prob){warning("\tWARNING: density plot is currently optimised for prob=T\n")}
    lines(density(x), col="darkred", lwd=2)                                 # add a density estimate with defaults
    lines(density(x, adjust=2), lty="dotted", col="darkgreen", lwd=2)    # add another "smoother" density


  }
  if(points){rug(x,col=rgb(0, 0, 0,alpha=0.2))}   # add tick marks below histogram

  if(normCurv){
     if(prob){warning("\tWARNING: normal curve fit is currently optimised for prob=F\n")}
    a=min(x,na.rm=T)
    b=max(x,na.rm=T)
    xx=seq(a-(b-a)/10,b+(b-a)/10,length=100)
    lines(sort(xx),dnorm(sort(xx),median(x,na.rm=T),sd(x,na.rm=T))*sum(y$counts*diff(y$breaks)),col='dodgerblue')
    }
    return(invisible(xx))

# to work need to construct the plot_cols and corresponding plot_lines & add a layout param dependent on legend=T to provide space for it on the left
#    legend(x="topright",pch=16,box.lwd=0,box.col="white",col=plot_cols,legend=plot_lines)
}



hist.dens<-function(x,points=T,col='lightgray',density=T,prob=T,breaks=50,...){  #density=F, # need to add some sort of axis scaling is not very compatible
  y=hist(x,prob=prob,...)


  if(density){  # requires prob=TRUE for probabilities not counts
     if(!prob){warning("\tWARNING: density plot is currently optimised for prob=T\n")}
    lines(density(x), col="darkred", lwd=2)                                 # add a density estimate with defaults
    lines(density(x, adjust=2), lty="dotted", col="darkgreen", lwd=2)       # add another "smoother" density


  }
  if(points){rug(x,)}   # add tick marks below histogram

    return(invisible(y))

# to work need to construct the plot_cols and corresponding plot_lines & add a layout param dependent on legend=T to provide space for it on the left
#    legend(x="topright",pch=16,box.lwd=0,box.col="white",col=plot_cols,legend=plot_lines)
}




Boxplot<-function(dat_mat,points=T,text=T,border='grey60',pch=16,cex=0.5,las=2,frame=F,varwidth=T,col='cornflowerblue',...){
##  standard R boxplot wrapper
##  USE: data frame - single box per column
##  USE: vector/single.column ~ vector/single.column == (values ~ groups)
par(mai=c(3.02,1.82,0.82,0.42))
  boxplot(dat_mat,border=border,pch=pch,cex=cex,las=las,frame=frame,varwidth=varwidth,...)
  if(points){
    # print(col)
    # print(Head(dat_mat))
    # if(length(col)!=nrow(dat_mat) ){col='corflowerblue'}
    stripchart(dat_mat, vertical = TRUE,  method = "jitter", add = TRUE, pch = 20, col = col)
  }
  # if(points & text){
  #   text(dat_mat, 1.1, labels=dat_mat)
  # }
}





boxplot.outlier<-function(y,label_name,...,spread_text=T,data,plot=T,range=4,label.col="darkred",push_text_right=0.8     ##  enlarge push_text_right in order to push the text labels further from their point
,segement_width_as_percent_of_label_dist=.3     ##  Change this if you want to have the line closer to the label (range should be between 0 to 1
  ,jitter_if_duplicate=T,jitter_only_positive_duplicates=F){  
##  SOURCE https://www.r-statistics.com/2011/01/how-to-label-all-the-outliers-in-a-boxplot/

  # jitter_if_duplicate - will jitter (Actually just add a bit of numbers) so to be able to decide on which location to plot the label when having identical variables...
  Library('plyr') # for is.formula and ddply
  Library('TeachingDemos')

  # a function to jitter data in case of ties in Y's
  jitter.duplicate=function(x,only_positive=F){
    if(only_positive){
      ss=x > 0
    } else {
      ss=T
    } 
    ss_dup=duplicated(x[ss])
    # ss=ss & ss_dup
    temp_length=length(x[ss][ss_dup]) 
    x[ss][ss_dup]=x[ss][ss_dup] + seq(from=0.00001,to=0.00002,length.out=temp_length)
    x
  }
  # jitter.duplicate(c(1:5))
  # jitter.duplicate(c(1:5,5,2))
  # duplicated(jitter.duplicate(c(1:5,5,2)))
  # jitter.duplicate(c(0,0,1:5,5,2))
  # duplicated(jitter.duplicate(c(0,0,1:5,5,2)))


  
  # handle cases where 
  if(jitter_if_duplicate){
    # warning("duplicate jutter of values in y is ON")
    if(!missing(data)){ #e.g: we DO have data
      # if(exists("y") && is.formula(y)){   # F && NULL # F & NULL
      y_name=as.character(substitute(y))  # I could have also used as.list(match.call())
                        # credit to Uwe Ligges and Marc Schwartz for the help
                        # https://mail.google.com/mail/?shva=1#inbox/12dd7ca2f9bfbc39
      if(length(y_name) > 1){ # then it is a formula (for example: "~","y","x"
        model_frame_y=model.frame(y,data=data)
        temp_y=model_frame_y[,1]
        temp_y =jitter.duplicate(temp_y,jitter_only_positive_duplicates)  # notice that the default of the function is to work only with positive values...
        # the_txt=paste(names(model_frame_y)[1],"temp_y",sep="<<-") # wrong...
        the_txt=paste("data['",names(model_frame_y)[1],"']=temp_y",sep="")      
        eval(parse(text=the_txt)) # jutter out y var so to be able to handle identical values.
      } else {  # this isn't a formula
        data[,y_name]=jitter.duplicate(data[,y_name],jitter_only_positive_duplicates)
        y=data[,y_name] # this will make it possible for boxplot(y,data) to work later (since it is not supposed to work with data when it's not a formula,but now it does :))
      }   
    } else {  # there is no "data"     
      if(is.formula(y)){ # if(exists("y") && is.formula(y)){    # F && NULL # F & NULL
        temp_y=model.frame(y)[,1]
        temp_y =jitter.duplicate(temp_y,jitter_only_positive_duplicates)  # notice that the default of the function is to work only with positive values...
        temp_y_name=names(model.frame(y))[1]  # we must extract the "names" before introducing a new enbironment (or there will be an error)
        environment(y)=new.env()
        assign(temp_y_name,temp_y,environment(y))
          # Credit and thanks for doing this goes to Niels Richard Hansen (2 Jan 30,2011)
          # http://r.789695.n4.nabble.com/environment-question-changing-variables-from-a-formula-through-model-frame-td3246608.html
        # warning("Your original variable (in the global environemnt) was just jittered.")  # maybe I should add a user input before doing this....
        # the_txt=paste(names(model_frame_y)[1],"temp_y",sep="<<-")
        # eval(parse(text=the_txt)) # jutter out y var so to be able to handle identical values.
      } else {
        y=jitter.duplicate(y,jitter_only_positive_duplicates)
      }   
    }
  }
  # the_txt=paste("print(",names(model_frame_y)[1],")")
  # eval(parse(text=the_txt)) # jutter out y var so to be able to handle identical values.
  # print(ls())

  
  # y should be a formula of the type: y~x,y~a*b
  # or it could be simply y
  if(missing(data)){
      boxdata=boxplot(y,plot=plot,range=range ,...)
    } else {
      boxdata=boxplot(y,plot=plot,data=data,range=range ,...)
    }
  if(length(boxdata$names) == 1 && boxdata$names =="") boxdata$names=1  # this is for cases of type: boxplot(y) (when there is no dependent group)
  if(length(boxdata$out) == 0 ){
    warning("No outliers detected for this boxplot")
    return(invisible())
    }

  stripchart(y, vertical = TRUE,  method = "jitter", add = TRUE, pch = 20, col = 'dodgerblue')
  if(!missing(data)) attach(data) # this might lead to problams I should check out for alternatives for using attach here...
  

  # creating a data.frame with information from the boxplot output about the outliers (location and group)
  boxdata_group_name=factor(boxdata$group)
  levels(boxdata_group_name)=boxdata$names[as.numeric(levels(boxdata_group_name))]  # the subseting is for cases where we have some sub groups with no outliers
  if(!is.null(list(...)$at)){ # if the user chose to use the "at" parameter,then we would like the function to still function (added on 19.04.2011)
    boxdata$group=list(...)$at[boxdata$group]   
    }
  boxdata_outlier_df=data.frame(group=boxdata_group_name,y=boxdata$out,x=boxdata$group)
  

  # Let's extract the x,y variables from the formula:
  if(is.formula(y)){
    model_frame_y=model.frame(y)
      # old solution: (which caused problems if we used the names parameter when using a 2 way formula... (since the order of the names is different then the levels order we get from using factor)
      # y=model_frame_y[,1]
      # x=model_frame_y[,-1]

    y=model_frame_y[,1]
    x=model_frame_y[,-1]
    if(!is.null(dim(x))){ # then x is a matrix/data.frame of the type x1*x2*..and so on - and we should merge all the variations...
      x=apply(x,1,paste,collapse=".")
    }
  } else {
    # if(missing(x)) x=rep(1,length(y))
    x=rep(1,length(y))  # we do this in case y comes as a vector and without x
  } 
  
  # and put all the variables (x,y,and outlier label name) into one data.frame
  DATA=data.frame(label_name,x ,y)
  
  if(!is.null(list(...)$names)){ # if the user chose to use the names parameter,then we would like the function to still function (added on 19.04.2011)
    DATA$x=factor(DATA$x,levels=unique(DATA$x))
    levels(DATA$x)=list(...)$names  # enable us to handle when the user adds the "names" parameter # fixed on 19.04.11  # notice that DATA$x must be of the "correct" order (that's why I used split above
    # warning("Careful,the use of the 'names' parameter is experimental.  If you notice any errors please e-mail me at: tal.galili@gmail.com")
    }

  if(!missing(data)){detach(data)}  # we don't need to have "data" attached anymore.

  # let's only keep the rows with our outliers 
  boxplot.outlier.data<-function(xx,y_name="y"){  #<<••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••  change 'outliers' to all values
    # y=xx[,y_name]
    # boxplot_range=range(boxplot.stats(y,coef=range)$stats)
    # ss=(y < boxplot_range[1]) | (y > boxplot_range[2])
    # return(xx[ss,])
    return(xx)
  }
  outlier_df=ddply(DATA,.(x),boxplot.outlier.data)
  

  # create proper x/y locations to handle overlaping dots...
  if(spread_text){
    # credit: Greg Snow
    require(TeachingDemos)    
    temp_x=boxdata_outlier_df[,"x"]
    temp_y1=boxdata_outlier_df[,"y"]
    temp_y2=temp_y1
    for(i in unique(temp_x)){
      tmp=temp_x == i
      temp_y2[ tmp ]=spread.labs( temp_y2[ tmp ],1.3*strheight('A'),maxiter=6000,stepsize=0.05) #,min=0 )
    }
    
  }
  

  
  # max(strwidth(c("asa","a"))
  # move_text_right=max(strwidth(outlier_df[,"label_name"]))  
  
  # plotting the outlier labels :)  (I wish there was a non-loop wise way for doing this)
  for(i in seq_len(dim(boxdata_outlier_df)[1])){
    # ss=(outlier_df[,"x"]  %in% boxdata_outlier_df[i,]$group) & (outlier_df[,"y"] %in% boxdata_outlier_df[i,]$y)

    # if(jitter_if_duplicate){
      # ss=(outlier_df[,"x"]  %in% boxdata_outlier_df[i,]$group) & closest.number(outlier_df[,"y"]  boxdata_outlier_df[i,]$y)
    # } else {
    ss=(outlier_df[,"x"]  %in% boxdata_outlier_df[i,]$group) & (outlier_df[,"y"] %in% boxdata_outlier_df[i,]$y)
    # }

    current_label=outlier_df[ss,"label_name"]
    temp_x=boxdata_outlier_df[i,"x"]
    temp_y=boxdata_outlier_df[i,"y"]    
    # cbind(boxdata_outlier_df,   temp_y2)
    # outlier_df

    
    
    if(spread_text){
      temp_y_new=temp_y2[i] # not ss      
      move_text_right=strwidth(current_label) * push_text_right
      text( temp_x+move_text_right,temp_y_new,current_label,col=label.col)      
      # strwidth
      segments( temp_x+(move_text_right/6),temp_y,temp_x+(move_text_right*segement_width_as_percent_of_label_dist),temp_y_new )
    } else {
      text(temp_x,temp_y,current_label,pos=4,col=label.col)
    }   
  }

  # outputing some of the information we collected
  invisible(list(boxdata=boxdata,boxdata_outlier_df=boxdata_outlier_df,outlier_df=outlier_df))
}



##  mark extreme outlier points - boxplot
# datos=iris[[2]]^5           ##  construimos unha variable con valores extremos
# boxplot(datos)            ##  representamos o diagrama de caixa

# dc=boxplot(datos,plot=F)      ##  garda en dc o diagrama, pero non o volve a representar
# attach(dc)
    
# if(length(out)>0){          ##  separa os distintos elementos, por comodidade
#   for(i in 1:length(out)){    ##  iniciase un bucle, que fai o mesmo para cada valor anomalo
#                   ##  o que fai vai entre chaves

#     if(out[i]>4*stats[4,group[i]]-3*stats[2,group[i]] | out[i]<4*stats[2,group[i]]-3*stats[4,group[i]]){    ##  unha condición, se se cumpre realiza o que está entre chaves
#       points(group[i],out[i],col="white") #borra o punto anterior
#       points(group[i],out[i],pch=4) #escribe o punto novo
#     }
#   }
#   rm(i)
# } #do if

# detach(dc)              ##  elimina a separacion dos elementos de dc
# rm(dc)                ##  borra dc
#                   ##  rematou o debuxo de valores extremos









##  add abline() using lm() /glm()? as an option
Plot<-function(xdat,ydat='',pch=20,col=rgb(0, 0, 0,alpha=0.2),abline=F,frame.plot=F,line45deg=F,labels=NULL,...){#xlim='',ylim='',
##  standard R plot wrapper
plot.new()
start_par=par()
  par(oma=c(0, 0, 0, 0), mar=c(4, 4, 1, 0), new=TRUE)
	options(warn=-1)
	if(is.character(ydat)&ydat==''){yplot=F}
	if(!is.character(ydat)){yplot=T}
	# if(xlim==''){xlim_dat=c(floor(min(xdat)),ceiling(max(xdat)))}
	# if(yplot){if(ylim==''){ylim_dat=c(floor(min(xdat)),ceiling(max(xdat)))}}
	options(warn=0)

# cat('\t',yplot,col,xlim,ylim,'\n')
# cat('\t',yplot,col,pch,frame.plot,'\n')

	if(!yplot){plot(x=xdat,pch=16,col=rgb(0, 0, 0,alpha=0.2),frame.plot=F,...)}
	if(yplot){plot(x=xdat,y=ydat,pch=16,col=rgb(0, 0, 0,alpha=0.2),frame.plot=F,...)}
	if(line45deg){abline(coef=c(0,1),lty=2,col='grey60')}
  if(length(labels)>1){text(xdat,ydat,labels=rownames(xdat))}
  options(warn=-1)
      par(start_par)
  options(warn=0)

}



plot.mat<-function(dat_mat,method='spearman',lm=T,...){
  Library('psych')
    pairs.panels(dat_mat,method=method,lm=lm,...)
}



Heat<-function(dat_mat,values='',values.rm='',margin=c(10,10),Rowv=T,Colv=T,mingrey=F,values.cex=1,ncols=101,cexrow=0.7,cexcol=0.7,dendrogram="both",verbose=F,...){
# plot.new()
start_par=par()
  par(oma=c(0, 0, 0, 0), mar=c(0, 0, 0, 0))
  if(class(Rowv)=='logical'&class(Colv)=='logical'){
    if(Rowv & Colv){dendrogram="both"}
    if(Rowv & !Colv){dendrogram="row"}
    if(!Rowv & Colv){dendrogram="column"}
    if(!Rowv & !Colv){dendrogram="none"}
  }
    if(values=='pval'){dat_mat=-log10(dat_mat)}
# can feasibly include a parameter for dendrogram as well, for more plottting flexibility

  Library('gplots')
  min=floor(min(dat_mat,na.rm=T))
  max=ceiling(max(dat_mat,na.rm=T))
  if(verbose){cat('\tmin=',min,', max=',max,'\n') }
  if(min<0 & max>=0){heat_colors =c(colorRampPalette(c("#0072B2","#56B4E9","white","#F0E442","darkred"))(ncols)); symmkey=T;ncols=ncols-1}
  if(mingrey & min>=0 & max>=0){heat_colors=c('grey60',colorRampPalette(c("white","#F0E442","darkred"))(ncols));symmkey=F}
  if(mingrey & min<=0 & max<=0){heat_colors=c('grey60',colorRampPalette(c("white","#56B4E9","#0072B2"))(ncols));symmkey=F}

  if(!mingrey & min==0 & max>=0){heat_colors=c(colorRampPalette(c("white","#F0E442","darkred"))(ncols+1));symmkey=F}
  if(!mingrey & min==0 & max<=0){heat_colors=c(colorRampPalette(c("white","#56B4E9","#0072B2"))(ncols+1));symmkey=F}

  options(warn=-1)
  if(values==''){
    cor_heat=heatmap.2((dat_mat),breaks=seq(min,max,length=(ncols+2)),col=heat_colors,trace="none",dendrogram=dendrogram,Rowv=Rowv,Colv=Colv,density.info="none",keysize=1,margins=margin,cexCol=cexcol,cexRow=cexrow,symkey=symmkey,hclustfun=function(x) hclust(x, method="ward.D2"),...)#,hclustfun=function(x) hclust(x, method="ward.D2"))
  }
  if(values=='cor'){
    celdat=round(dat_mat,digits=2)
    if(values.rm!=''){celdat[celdat%in%values.rm]=''}
    cor_heat=heatmap.2((dat_mat),cellnote=celdat,notecex=values.cex,notecol="black",breaks=seq(min,max,length=(ncols+2)),col=heat_colors,trace="none",dendrogram=dendrogram,Rowv=Rowv,Colv=Colv,margins=margin,density.info="none",keysize=1,cexCol=cexcol,cexRow=cexrow,symkey=T,hclustfun=function(x) hclust(x, method="ward.D2"),...)
  }
  if(values=='pval'){
      celdat=as.data.frame(dat_mat)
      celdat[dat_mat>=5]="****"
      celdat[dat_mat>=4 & dat_mat<5]="***"
      celdat[dat_mat>=3 & dat_mat<4]="**"
      celdat[dat_mat>=2 & dat_mat<3]="*"
      celdat[dat_mat>= -log10(0.05) & dat_mat<2]="+"
      celdat[dat_mat< -log10(0.05)]=""
    cor_heat=heatmap.2((dat_mat),cellnote=celdat,notecex=values.cex,notecol="black",breaks=seq(min,max,length=(ncols+2)),col=heat_colors,trace="none",dendrogram=dendrogram,Rowv=Rowv,Colv=Colv,margins=margin,density.info="none",keysize=1,cexCol=cexcol,cexRow=cexrow,symkey=F,hclustfun=function(x) hclust(x, method="ward.D2"),...)
  }
  if(values==''){
    celdat=as.data.frame(matrix('',nrow=nrow(dat_mat),ncol=ncol(dat_mat)))

  }
    #celdat[celdat==min(celdat)]=""

      par(start_par)
  options(warn=0)
   return(invisible(list(cor_heat=cor_heat,celdat=celdat,dat_mat=dat_mat)))
}


# listov<-function(dat_lis,union=T,intersect=F){
# ##  USE : get union or intersect of all elements (vector) in list  ~~  bgcommon for list of char vectors

# if(union & intersect){stop('both union & intersect == T, expect 1 only')}
# if(!union & !intersect){stop('both union & intersect == F, expect 1 only')}

#   udat=dat_lis[[1]]
#   for(ilis in 2:length(dat_lis)){
#     if(union){udat=union(udat,dat_lis[[ilis]])}
#     if(intersect){udat=intersect(udat,dat_lis[[ilis]])}
#   }
#   return(udat)
# }

Venn4<-function(dat_lis,main='',...){
 library(gplots)
  venn(dat_lis)
  mtext(main,side=3,...)
  lisun=length(listov(dat_lis,T,F))
  lisin=length(listov(dat_lis,F,T))

  mtext(paste0('\n\nintersect/union=',round(lisin/lisun,digits=3)*100,'%'),side=1)
}


Heatp<-function(pval_mat,sig=T,dat_descr=''){
  Library('gplots')
#  if(breaks=="default"){breaks=seq(0,max(-log10(pval_mat)),length=(102))}
#  if(col=="default"){col=colorRampPalette(c("white","#ffffbf","#fee090","#fdae61","#f46d43","#d73027","#a50026","darkred"))(length(breaks)-1)}

margin=c(max(13,(125-nrow(pval_mat))),12)
lheight=c(0.06,0.94)

  if(sig){
      dat_sig=as.data.frame(pval_mat)
      dummy=pval_mat<=1e-5
      dat_sig[dummy]="****"
      dummy=pval_mat<=1e-4 & pval_mat>1e-5
      dat_sig[dummy]="***"
      dummy=pval_mat<=1e-3 & pval_mat>1e-4
      dat_sig[dummy]="**"
      dummy=pval_mat<=1e-2 & pval_mat>1e-3
      dat_sig[dummy]="*"
      dummy=pval_mat<=0.05 & pval_mat>1e-2
      dat_sig[dummy]="+"
      dummy=pval_mat>0.05
      dat_sig[dummy]=""
    heatmap.2(-log10(pval_mat),margins=margin,lhei=lheight,cellnote=(dat_sig),col=colorRampPalette(c("white","#ffffbf","#fee090","#fdae61","#f46d43","#d73027","#a50026","darkred"))(101),breaks=seq(0,max(-log10(pval_mat)),length=(102)),notecol="black",tracecol=F,dendrogram="none",Rowv=F,Colv=F,density.info="none",keysize=1,cexCol=0.7,cexRow=0.7,symkey=F,lwid=c(0.7,0.3),lmat=rbind(c(4,3),c(1,2)))
  }
 if(!sig){
    heatmap.2(-log10(pval_mat),margins=margin,lhei=lheight,col=colorRampPalette(c("white","#ffffbf","#fee090","#fdae61","#f46d43","#d73027","#a50026","darkred"))(101),breaks=seq(0,max(-log10(pval_mat)),length=(102)),tracecol=F,dendrogram="none",Rowv=F,Colv=F,density.info="none",keysize=1,cexCol=0.7,cexRow=0.7,symkey=F,lwid=c(0.7,0.3),lmat=rbind(c(4,3),c(1,2)))
  }
  ## text in top right corner
  par(xpd = NA)
  mtext(dat_descr, adj = 1, side = 3)
  par(xpd = F)
}



gestaltheat<-function(dat_mat,dat_descr='',multi_page=T){
## currently breaks if remainder==1, need an additional contingency..
  if(multi_page){
    dat_mat=dat_mat[do.call(order, (lapply(1:NCOL(dat_mat), function(i) dat_mat[, i]))), ]
    if(nrow(dat_mat>100)){
      k=0
      remainder=(nrow(dat_mat)-(k+100))
      cat("\tmulti-page plot :\n")

      while(remainder>1){
        cat("\t",k+1,k+100,"\n")
        dat_plot=dat_mat[(k+1):(k+100),,drop=F]
        k=k+100
        remainder=(nrow(dat_mat)-(k+100))
        Heatp(dat_plot,sig=T,dat_descr=dat_descr)
      }

      if(remainder<0){
        cat("\t",k+1,k+(100+remainder),"\n")
        dat_plot=dat_mat[(k+1):(k+(100+remainder)),,drop=F]
        Heatp(dat_plot,sig=T,dat_descr=dat_descr)
      }
    }
  }
}



Venn<-function(dat_lis,dat_descr='',...){
##  calculate proportional overlaps for Venn Diagram
##  dat_lis - named lists of items   list(first=c(gene1,gene2),second=c(gene2,gene3))
##   + for some reason plotting does not seem to work within the function, use the returned 'fit' obj to  plot(fit, fill_opacity = 0.3,counts=T)
Library('eulerr')

# if(length(dat_lis)>3){stop('currently only able to handle lists of 3 or less')}
nlaps=list()
  for(ivar in names(dat_lis)){
     nlaps[[as.character(ivar)]]=length(dat_lis[[ivar]])
    for(icom in names(dat_lis)[names(dat_lis)!=ivar]){
      nlaps[[as.character(paste(ivar,icom,sep='&'))]]=length(intersect(dat_lis[[ivar]],dat_lis[[icom]]))
    }
  }
##  clearly need an extra step like in the loop above & addition for 4 overlapping etc
  if(length(dat_lis)>=3){
    nlaps[[as.character(paste(names(dat_lis),collapse='&'))]]=length(Reduce(intersect, list(dat_lis)))
  }

  fit=euler(unlist(nlaps))
  pdat=plot(fit,counts=T,...)

  # mtext(dat_descr,adj=1,side=3)

# ?plot.euler
# ## S3 method for class 'euler'
#      plot(x, fill = qualpalr_pal, fill_alpha = 0.4,
#        auto.key = FALSE, counts = FALSE, labels = is.logical(auto.key) &&
#        !isTRUE(auto.key), fontface = "bold", par.settings = list(), ...,
#        default.prepanel = prepanel.euler, default.scales = list(draw = FALSE),
#        panel = panel.euler, outer_strips, fill_opacity)
  return(list(nlaps=nlaps,plot=pdat,fit=fit))
}




###•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•###
###•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•###



pca<-function(dat_mat,check_scaled=T,n_pcs=5,verbose=F,...){
##  INPUT	- dat_mat: rows - genes, columns - samples
##  PARAM	- n_pcs - number of PCs to use, default(n_pcs=5), if n_pcs=="" auto-calculate optimum number
##  PARAM	- check_scale - if input is pre-scaled, PCA will reflect the scaled dimension tie if columns scaled - PCA wrt to columns
##  NOTE	- function used : "prcomp(t(dat_mat),scale=T,center=T)
	if(check_scaled){
		if(verbose){cat('\tcheck if data is already scaled (row or column) - affects PCA\n')}
		humpty=apply(dat_mat,1,sd,na.rm=T)
		dumpty=apply(dat_mat,2,sd,na.rm=T)
		if(sum(humpty)==length(humpty)){stop('data already scaled by row, set "check_scale=F" to ignore')}
		if(sum(dumpty)==length(dumpty)){stop('data already scaled by column, set "check_scale=F" to ignore')}
		if(sum(dumpty)!=length(dumpty)&sum(humpty)!=length(humpty)){if(verbose){cat('\t + input data is not scaled\n')}}
	}

	options(warn=-1)
		if(n_pcs==''){
			if(verbose){cat('\t\tdetermine optimum number factors to extract (very time cosuming)\n')}

		# Determine Number of Factors to Extract
		Library('nFactors')
		ev=eigen(cor(dat_mat)) # get eigenvalues
		ap=parallel(subject=nrow(dat_mat),var=ncol(dat_mat),rep=10,cent=.05)
		nS=nScree(x=ev$values, aparallel=ap$eigen$qevpea)
		plotnScree(nS)

		}
	options(warn=0)

	pcs=prcomp(t(dat_mat),scale=T,center=T)
##  calculate pcs scaled by rotaion (prcomp - PCs have randomly assigned directions)
  pc=t(t(pcs$x)*apply(pcs$rotation,2,function(x){sign(x[which.max(abs(x))])}))

	pcstat=round(as.matrix(summary(pcs)$importance["Proportion of Variance",1:n_pcs]),digits=2)*100
	    colnames(pcstat)="PCA variance explained %"
	    if(verbose){print(pcstat)}
	return(invisible(list(pcs=pc,pca=pcs,pcstat=pcstat)))
}



pcplot<-function(dat_mat,dat_groups='',samp_labels=T,n_pch=1,do_plots=c(T,T,T),check_scaled=F,n_pcs=6,dat_descr="",pie_radius=1,help=F,verbose=T,legend_space=8,...){
  Library('ggplot2')
# 
  if(samp_labels){library(ggrepel)}
	if(help){
		cat('\tINPUT\t - dat_mat : rows - genes, columns - samples\n')
#		cat('\tINPUT\t - if dat_mat=list(dat_mat1,dat_mat2,...) each list is assumed to be a sub-grop of bigger dataset\n')
		cat('\tINPUT\t - dat_groups - optional vector, length=ncol(dat_mat) used to color pca data as groups, discarded if class(dat_mat)==list\n')
		
		cat('\tINPUT\t - n_pcs - number of PCs to use, if n_pcs=="" auto-calculate optimum number\n')
		cat('\tINPUT\t - check_scale - if input is pre-scaled, PCA will reflect the scaled dimension
			\t\t\tie if columns scaled - PCA wrt to columns\n')
		cat('\tNOTE\t - function used : "prcomp(t(dat_mat),scale=T,center=T)"\n\n')
	}
if(length(dat_groups)>1){if((ncol(dat_mat)!=length(dat_groups))){stop('ncol(dat_mat)==length(dat_groups) must be met')}}
	pchmix=rep(c(21:25)[1:n_pch],length=32)
	 dat_is_list=F
# 	if(class(dat_mat)=='list'){
# 		datleg=as.data.frame(unlist(lapply(dat_mat,ncol)))
# 			colnames(datleg)='n'
# 		datleg$name=rownames(datleg)
# 		datleg$color=colmix[1:nrow(datleg)]
# 		datleg$point=pchmix[1:nrow(datleg)]

# 		datcol=''
# 		datpch=''
# 	for(idat in 1:nrow(datleg)){
# 		datcol=c(datcol,rep(datleg$color[idat],datleg$n[idat]))
# 		datpch=c(datpch,rep(datleg$pch[idat],datleg$n[idat]))
# 	}
# 		datcol=datcol[-1]
# 		datpch=datpch[-1]
# 		dat_mat=as.data.frame(dat_mat)
# 		dat_is_list=T
# 	}

options(warn=-1)
	if(length(dat_groups)>1){
		datleg=Table(dat_groups)[,1:2]
			colnames(datleg)=c('name','n')
		datleg$color=colmix[1:nrow(datleg)]
		datleg$point=pchmix[1:nrow(datleg)]

		datcol=rep('magenta',length(dat_groups))
		datpch=rep(10,length(dat_groups))
	for(idat in 1:nrow(datleg)){
    datcol[dat_groups==datleg$name[idat]]=datleg$color[idat]
    datpch[dat_groups==datleg$name[idat]]=datleg$point[idat]
		# datcol=c(datcol,rep(datleg$color[idat],datleg$n[idat]))
		# datpch=c(datpch,rep(datleg$point[idat],datleg$n[idat]))
	}
		# datcol=datcol[-1]
		datpch=as.numeric(datpch[-1])
		dat_is_list=T
	}
options(warn=0)

	if(ncol(dat_mat)<n_pcs){
		cat('\tinput only has',ncol(dat_mat),'observations < "n_pcs" => ', ncol(dat_mat),'will be used instead\n')
		n_pcs=dat_mat
	}

	holder=pca(dat_mat,n_pcs=n_pcs,check_scaled=check_scaled)
	pcs=as.data.frame(holder$pcs)
	pcstat=holder$pcstat
	rm(holder)


  pcs$legend=rownames(pcs)

	if(do_plots[1]){
		if(verbose){cat('\t\tpiechart - % variance explained\n')}
        par(mar=c(2,10,2,10))
			pie(pcstat,main=paste("Variance Explained (%) by PC1-5\n",dat_descr),cex=2,radius=pie_radius,labels=paste0("PC",1:n_pcs,"  ",pcstat,"%"),col=colmix[1:n_pcs])
        par(mar=c(5,4,4,10))
	}

	if(do_plots[2]){
		if(verbose){cat('\t\tplot - pc1~pc2, pc2~pc3 etc\n')}
		for(ipc in 1:(n_pcs-1)){
			if(!dat_is_list){
#				par(mfrow=c(ceiling((n_pcs)/2),3))
        plot(pcs[,ipc],pcs[,ipc+1],pch=16,col=rgb(0, 0, 0,alpha=0.2),main=paste0('PC',ipc,' v PC',ipc+1,' ',dat_descr),xlab=paste0('PC',ipc,' ',pcstat[ipc],'%'),ylab=paste0('PC',ipc+1,' ',pcstat[ipc+1],'%'),frame.plot=F)
        if(samp_labels){text(pcs[,ipc],pcs[,ipc+1],labels=rownames(pcs))}
#				par(mfrow=c(1,1))
			}

			if(dat_is_list){
# 				par(mar=c(5,4,4, legend_space))
# 				plot(pcs[,ipc],pcs[,ipc+1],pch=datpch,col=datcol,bg=datcol,main=paste0('PC',ipc,' v PC',ipc+1,' ',dat_descr),xlab=paste0('PC',ipc,' ',pcstat[ipc],'%'),ylab=paste0('PC',ipc+1,' ',pcstat[ipc+1],'%'),frame.plot=F)
#         if(samp_labels){text(pcs[,ipc],pcs[,ipc+1],labels=rownames(pcs))}
# ##  create a new plot overlay (with no left margin) with legend on the topright
# 				par(fig=c(0,1,0,1), oma=c(0, 4, 0, 0), mar=c(0, 4, 0, 0), new=TRUE)
# 				plot.new()
# 				legend(x="topright",pch=datleg$point,box.lwd=0,box.col="white",col=datleg$color,pt.bg=datleg$color,legend=paste0(datleg$n,'  ',datleg$name))

pdat=as.data.frame(pcs[,c(colnames(pcs)[ipc], colnames(pcs)[ipc+1])])
  colnames(pdat)=c('x','y')

if(samp_labels){
  print(
      # ggplot(as.data.frame(pcs), aes_string(colnames(pcs)[ipc], colnames(pcs)[ipc+1], label = 'rownames(pcs)') +
      ggplot(pdat,aes(x,y,label=rownames(pdat))) +
      geom_point(color = datcol) +
      geom_text_repel(color=datcol) +
      theme_classic(base_size=16)
        )}

if(!samp_labels){
  print(
      # ggplot(as.data.frame(pcs), aes_string(colnames(pcs)[ipc], colnames(pcs)[ipc+1], label = 'rownames(pcs)') +
      ggplot(pdat,aes(x,y,label=rownames(pdat))) +
      geom_point(color = datcol) +
      # geom_text_repel(color=datcol) +
      theme_classic(base_size=16)
        )}
			}
		}
	}

	# if(do_plots[3]){
		
	# 	if(verbose){cat('\t\tbiplot - not informative on expression (vectors==genes)\n')}
	# 	Library('ggbiplot')
	# 	if(!dat_is_list){dummy=ggbiplot(pcs,obs.scale=1, var.scale=1, ellipse=TRUE, circle=TRUE)}
	# 	if(dat_is_list){dummy=ggbiplot(pcs,obs.scale=1, var.scale=1, ellipse=TRUE, circle=TRUE,groups=datcol)}
	# 	dummy=dummy + scale_color_discrete(name='')
	# 	dummy=dummy + theme(legend.direction='horizontal',legend.position='top')
	# 	dummy=dummy + theme_bw() + theme(panel.border=element_blank(),panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.line=element_line(colour="black"))
	# 	print(dummy)
	# }
	return(invisible(list(pcs=pcs,pcstat=pcstat,datcol=datcol)))
}



clust<-function(dat_mat,dat_groups,scale_dat=F,clust_method='ward.D2',k=1,cor_method='spearman',do_plots=T,dat_descr='',par_mar=c(4,1,3,20),help=F,...){
 if(help){
  cat('\n\tINPUT :\tdat_mat - auto-detect data frame or list of data frames - 1 per condition (samples=columns)\n')
  cat('\tNOTE  :\tcor_method - correlation method to calculate distance see "cor" for options, option: "dist" - distance on raw data, no correlation calculated, "as.dist" treat input as distances\n\n')
  cat('\tNOTE  :\tk- specify number of clusters for cutree, can use range i.e. 2:5, if k="dynamic", WGCNA function "cutreeHybrid" is used to cut the tree\n\n')
 }
  if(do_plots){
    Library('dendextend') ##  moved to the top to avoid waiting for clustering only to find that the library does not exist..
  }

dat_groups=as.character(dat_groups)   ##  below does not like factors
  ugrp=(unique(dat_groups))
  cat('\tfound',length(ugrp),'unique groups from dat_groups\n')
  colvec=dat_groups
  ik=1
  for(igrp in ugrp){
    colvec[colvec==igrp]=colmix[ik]
    ik=ik+1
  }



 if(scale_dat){
  cat('\t- scale and center data (by columns)\n')
  dat_mat=scale(dat_mat,scale=T,center=T)
 }

  if(cor_method=='as.dist'){
   cat('\t- as.dist(dat_mat)\n')
        distmat=as.dist(dat_mat)
  }
  if(cor_method=='dist'){
   cat('\t- calculating eucledian distance\n')
        distmat=dist(t(dat_mat))
  }
  if(cor_method!='dist'&cor_method!='as.dist'){
   cat('\t- calculating distance based on',cor_method,'correlation\n')
        distmat=as.dist(1-abs(t(cor(dat_mat,method=cor_method))))
  }

    clustat=hclust(distmat,method=clust_method)

  if(k!='dynamic'){
    cat('\t- cutree, k=',k,'\n')
    trestat=cutree(clustat,k=k)
    main_text=paste('\ndistance=',cor_method,'cluster.method=',clust_method,'k =',paste(k,collapse=', '),'\n',dat_descr)
  }
  # if(k=='dynamic'){
  #   Library('WGCNA')      ##  required for k='dynamic' only i.e. cutreeHybrid
  #   cat('\t- dynamic tree cut - WGCNA function, works best with clust_method="average"\n')
  #   trestat=cutreeHybrid(clustat,as.matrix(distmat),minClusterSize=2,deepSplit=0)$labels
  #   main_text=paste(dat_descr,'\ndistance=',cor_method,'cluster.method=',clust_method,'clusters determined by WGCNA: "cutreeHybrid"')
  # }

  clustnm=(unique(trestat))
  clust=list()
  for(iclust in 1:length(clustnm)){
    clust[[as.character(clustnm)[iclust]]]=names(trestat)[trestat==clustnm[iclust]]
  }
    if(do_plots){
      cat('\t- plotting results\n')
    par_cur=par()$mar
    par(mar=par_mar)

      clusden=as.dendrogram(clustat)
      labels_colors(clusden)=colvec[order.dendrogram(clusden)]
      
      clusden=color_branches(clusden, k=k)

      plot(clusden, horiz=TRUE,...)

      colored_bars(colvec, clusden, horiz=TRUE)

## colored_bars can be used to show k-means clusters (supports multiple bars, including colvec), as per EG
#   k234=cutree(dend, k=2:4)
#   colored_bars(cbind(k234[,3:1], col_car_type), dend, rowLabels=c(paste0("k=", 4:2), "Car Type"))

      mtext(text=main_text,side=3,cex=1.5)

    }


          readme='\n\toutput contains :
            \t1. clust - members of modules based on k
            \t2. distmat - distance matrix used to perfom hierarchical clustering
            \t3. clustat - ouput of hcust(distmat)
            \t4. trestat - output of cutree(clustat)
            \t5. colvec - vector of colors used
            \n'
  return(invisible(list(clust=clust,distmat=distmat,clustat=clustat,trestat=trestat,colvec=colvec,readme=readme)))

}



# clust<-function(dat_mat,horiz=T,scale_dat=F,clust_method='ward.D2',k=1,cor_method='dist',do_plots=T,plot_cex=0.8,dat_descr='',par_mar=c(4,1,3,20),help=F,...){
# ##  ,... refers to plotDendroAndColors   ::   library(WGCNA)
#  if(help){
#   cat('\n\tINPUT :\tdat_mat - auto-detect data frame or list of data frames - 1 per condition (samples=columns)\n')
#   cat('\tNOTE  :\tcor_method - correlation method to calculate distance see "cor" for options, option: "dist" - distance on raw data, no correlation calculated, "as.dist" treat input as distances\n\n')
#   cat('\tNOTE  :\tk- specify number of clusters for cutree, can use range i.e. 2:5, if k="dynamic", WGCNA function "cutreeHybrid" is used to cut the tree\n\n')
#  }
#   dat_is_list=F
#  if(class(dat_mat)=='list'){
#     cat('\t- processing list information\n')
#   dat_is_list=T
#   colvec=""
#   for(ilis in 1:length(dat_mat)){
#     colvec=c(colvec,rep(colmix[ilis],ncol(dat_mat[[ilis]])))
#   }
#   colvec=colvec[-1]
#   dat_mat=as.data.frame(dat_mat)
#   }

# #print(horiz)
# #print(dat_is_list)
#  if(scale_dat){
#   cat('\t- scale and center data (by columns)\n')
#   dat_mat=scale(dat_mat,scale=T,center=T)
#  }
#    if(do_plots){
#       Library('dendextend') ##  moved it here to avoid waiting for clustering only to find that the library does not exist..
#    }

#   if(cor_method=='as.dist'){
#    cat('\t- as.dist(dat_mat)\n')
#         distmat=as.dist(dat_mat)
#   }


#   if(cor_method=='dist'){
#    cat('\t- calculating eucledian distance\n')
#         distmat=dist(t(dat_mat))
#   }

#   if(cor_method!='dist'&cor_method!='as.dist'){
#    cat('\t- calculating distance based on',cor_method,'correlation\n')
#         distmat=as.dist(1-abs(t(cor(dat_mat,method=cor_method))))
#     }

#     clustat=hclust(distmat,method=clust_method)


#   if(k!='dynamic'){
#     cat('\t- cutree, k=',k,'\n')
#     trestat=cutree(clustat,k=k)
#     main_text=paste(dat_descr,'\ndistance=',cor_method,'cluster.method=',clust_method,'k =',paste(k,collapse=', '))
#   }
#   if(k=='dynamic'){
#     library(WGCNA)      ##  required for k='dynamic' only i.e. cutreeHybrid
#     cat('\t- dynamic tree cut - WGCNA function, works best with clust_method="average"\n')
#     trestat=cutreeHybrid(clustat,as.matrix(distmat),minClusterSize=2,deepSplit=0)$labels
#     main_text=paste(dat_descr,'\ndistance=',cor_method,'cluster.method=',clust_method,'clusters determined by WGCNA: "cutreeHybrid"')
#   }
# #   cutree$labels

#   clustnm=(unique(trestat))
#   clust=list()
#   for(iclust in 1:length(clustnm)){
#     clust[[as.character(clustnm)[iclust]]]=names(trestat)[trestat==clustnm[iclust]]
#   }
#     if(do_plots){
#       cat('\t- plotting results\n')
#     par_cur=par()$mar
#     par(mar=par_mar)

#       if(horiz){
#       clusden=as.dendrogram(clustat)
#       if(dat_is_list){labels_colors(clusden)=colvec[order.dendrogram(clusden)]}
      
#       clusden=color_branches(clusden, k=k)

#       if(dat_is_list){plot(clusden, horiz=TRUE,...)}

#       if(dat_is_list){colored_bars(colvec, clusden, horiz=TRUE)}

# ## colored_bars can be used to show k-means clusters (supports multiple bars, including colvec), as per EG
# #   k234=cutree(dend, k=2:4)
# #   colored_bars(cbind(k234[,3:1], col_car_type), dend, rowLabels=c(paste0("k=", 4:2), "Car Type"))

#       mtext(text=main_text,side=3,cex=1.5)

#     }

#     if(!horiz){
#       plotDendroAndColors(
#       clustat
#       ,trestat
#       #,cutHeight=300
#       ,hang=0.03
#       #,addGuide=TRUE
#       ,guideHang=0.05
#       ,main=main_text
#       ,cex.colorLabels=plot_cex
#       ,cex.dendroLabels=plot_cex
#       ,cex.rowText=plot_cex
#       ,...)
#     }
#     par(mar=par_cur)
#   }


# #  cat(readme)
#     if(dat_is_list){
#           readme='\n\toutput contains :
#             \t1. clust - members of modules based on k
#             \t2. distmat - distance matrix used to perfom hierarchical clustering
#             \t3. clustat - ouput of hcust(distmat)
#             \t4. trestat - output of cutree(clustat)
#             \t5. colvec - vector of colors used
#             \n'
#   return(invisible(list(clust=clust,distmat=distmat,clustat=clustat,trestat=trestat,colvec=colvec,readme=readme)))
#   }
#     if(!dat_is_list){
#           readme='\n\toutput contains :
#             \t1. clust - members of modules based on k
#             \t2. distmat - distance matrix used to perfom hierarchical clustering
#             \t3. clustat - ouput of hcust(distmat)
#             \t4. trestat - output of cutree(clustat)
#             \n'
#   return(invisible(list(clust=clust,distmat=distmat,clustat=clustat,trestat=trestat,readme=readme)))
#   }
# }



###•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•###
###•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•###


##  need to implement removal for all.na values
sd.check<-function(dat_mat,check_rows=F,check_cols=T,verbose=T){

	if(verbose){cat("\n+++++++++++++++++ data qc check +++++++++++++++++\n")}
	cat("dat_mat contains",ncol(dat_mat),"variables, of which :\n")
	dat_class=dat.class(dat_mat,verbose=verbose)

    dat_num=dat_mat[,dat_class==("numeric"),drop=F]
    dat_fac=dat_mat[,dat_class=="factor",drop=F]
    dat_otr=dat_mat[,!(dat_class%in%c("factor","numeric")),drop=F]

	rowind=1:nrow(dat_mat)
	colind=1:ncol(dat_mat)

	if(check_cols==T){
		fac_col=apply(dat_fac,2,function(x) sum(table(x)!=0))
		num_col=apply(dat_num,2,sd,na.rm=T)
		numvarcol=names(num_col)[num_col==0]

		if(length(numvarcol)){cat("\tcol - values do not vary (sd=0) :\t",paste(numvarcol,collapse=", "),"\n")}
			facvarcol=names(fac_col)[fac_col==1]
		if(length(facvarcol)){cat("\tcol - contains single value :\t\t",paste(facvarcol,collapse=", "),"\n")}
			facvaridc=names(fac_col)[fac_col==nrow(dat_fac)]
		if(length(facvaridc)){cat("\tcol - as many levels as rows :\t\t",paste(facvaridc,collapse=", "),"\n")}
			colind=!(colnames(dat_mat)%in% c(numvarcol,facvarcol,facvaridc))
	}

	if(check_rows==T){
		fac_row=apply(dat_fac,1,function(x) sum(table(x)!=0))
		num_row=apply(dat_num,1,sd,na.rm=T)

		numvarrow=names(num_row)[num_row==0]
		if(length(numvarrow)){cat("\t",length(numvarrow),"rows - values do not vary (sd=0) , first ",min(20,length(numvarrow)),":\n",paste(numvarrow[1:min(20,length(numvarrow))],collapse=", "),"\n")}
			facvarrow=names(fac_row)[fac_row==1]
		if(length(facvarrow)){cat("\t",length(facvarrow),"rows - contain single value 'factor', first ",min(20,length(facvarrow)),":\n",paste(facvarrow[1:min(20,length(facvarrow))],collapse=", "),"\n")}
			facvaridr=names(fac_row)[fac_row==nrow(dat_fac)]
		if(length(facvaridr)){cat("\trow - as many levels as rows :\t\t",paste(facvaridr,collapse=", "),"\n")}
			rowind=!(rownames(dat_mat)%in% c(numvarrow,facvarrow,facvaridr))
	}

	if(ncol(dat_otr)>0){
	    cat("\tcol - not factor nor numeric :\t",paste(names(dat_otr),collapse=", "),"\n")
	}
		cat("\n")

	return(invisible(dat_mat[rowind,colind]))
}




##  attempt to re-write the above, in doing so, found the bug that lead to 'NA' column names - ie cols/rows with missing 
# sd.check<-function(dat_mat,check_rows=F,check_cols=T,verbose=T){
#   if(verbose){
#     cat("\n+++++++++++++++++ data qc check +++++++++++++++++\n")
#     cat("\tcheck numberic and factor only\n")
#   }
  

#   if(check_rows){
#     if(verbose){cat("\tdat_mat contains",nrow(dat_mat),"variables, of which :\n")}
#     row_ind=dat.class(t(dat_mat),verbose=verbose)

#     row_num=dat_mat[row_ind%in%c('numeric','integer')]
#   } 



#   if(check_cols){
#     if(verbose){cat("\tdat_mat contains",ncol(dat_mat),"variables, of which :\n")}
#     col_ind=dat.class(dat_mat,verbose=verbose)

#     col_num=apply(dat_mat[,col_ind%in%c('numeric','integer')],2,sd,na.rm=T)

#   }

# }


deconv<-function(exp_mat,do_plots=F,verbose=F,mth=0.95,points=T,...){
##  https://www.r-bloggers.com/fitting-mixture-distributions-with-the-r-package-mixtools/  
  set.seed(0) ##  required for reproducibility -> in a single + limited test the gene numbers in upper and lower dont change, but not fully tested
  if(verbose){
    cat('\tUSE : applies deconvolution method to separate bimodal distirbution into two, returns the greater normal\n')
    cat('\tNOTE: expr_mat should be the log transformed matrix of gene expression\n')
  }

if(!length(mth)%in%c(1,2)) {stop('\tmth defines the quantiles to look for i.e.\n\t\t1. a single value for one tail eg mth=0.05, mth=0.95\n\t\t2. two values for both tails - 3 distributions  eg mth=c(0.2,0.8)')}

  Library('mixtools')

##  example of a more general case? fitting mean and sd is for comp time efficiency so may not be ideal for other data.. https://www.r-bloggers.com/fitting-mixture-distributions-with-the-r-package-mixtools/
  M=apply(exp_mat,1,mean)
  S=apply(exp_mat,1,sd)

if(length(mth)==1){
  cat('\n\tperform single end deconvolution mth=',mth,'\n')
  yy=normalmixEM(M,mu=quantile(M,c(0.25,0.75),lambda=c(0.5,0.5)))
  Mth=qnorm(mth,min(yy$mu),yy$sigma[which.min(yy$mu)])

  upper=exp_mat[M>Mth,,drop=F]
  lower=exp_mat[M<Mth,,drop=F]

  if(do_plots){
    pdat_hist=hist(exp_mat,col='darkgrey',prob=T,...)
#    hist(lower,col=rgb(1,0,0,0.5),breaks=max(30,100*(nrow(lower)/nrow(exp_mat))),add=T)
#    hist(upper,col=rgb(0,0,1,0.5),breaks=max(30,100*(nrow(upper)/nrow(exp_mat))),add=T)
  # lines(density(M, na.rm=T), lty=2, lwd=2)
  par(new=T)
  lines(density(upper, na.rm=T), lty=1, lwd=2,col='darkred')
  par(new=T)
  lines(density(lower, na.rm=T), lty=1, lwd=2,col='dodgerblue')
    if(points){rug(exp_mat,col=rgb(0, 0, 0,alpha=0.2))}   # add tick marks below histogram
#    legend(x="topright",pch=15,box.lwd=0,box.col="white",col=c('darkgrey',rgb(1,0,0,0.5),rgb(0,0,1,0.5)),pt.bg=c('darkgrey',rgb(1,0,0,0.5),rgb(0,0,1,0.5)),legend=c(paste0('n=',nrow(exp_mat)),paste0('n=',nrow(lower)),paste0('n=',nrow(upper))),cex=1)
    legend(x="topright",pch=15,box.lwd=0,box.col="white",col=c('darkgrey','darkred','dodgerblue'),pt.bg=c('darkgrey',rgb(1,0,0,0.5),rgb(0,0,1,0.5)),legend=c(paste0('density n=',nrow(exp_mat)),paste0('lower n=',nrow(lower)),paste0('upper n=',nrow(upper))),cex=1)
    cat('\tdataset n=',nrow(exp_mat),'lower n=',nrow(lower),'upper n=',nrow(upper),'\n')
  }


    print(shapiro.test(lower))
    print(shapiro.test(upper))
  return(invisible(list(upper=upper,lower=lower)))

}

if(length(mth)==2){
  cat('\n\tperform two tailed deconvolution mth=c(',paste(mth,collapse=','),')\n',sep='')
  yy=normalmixEM(M,mu=quantile(M,c(0.25,0.75),lambda=c(0.5,0.5)))
  Mth.1=qnorm(mth[1],min(yy$mu),yy$sigma[which.min(yy$mu)])
  Mth.2=qnorm(mth[2],min(yy$mu),yy$sigma[which.min(yy$mu)])

  dens=density(exp_mat)
  lower=exp_mat[M<Mth.1,,drop=F]
  midle=exp_mat[M>Mth.1 & M<Mth.2,,drop=F]  ##  technically a typo, but easier to see errors when vars are same length
  upper=exp_mat[M>Mth.2,,drop=F]


#https://stackoverflow.com/questions/9246040/axis-labeling-in-r-histogram-and-density-plots-multiple-overlays-of-density-plo
    # myhist <- hist(x,prob=FALSE,col="gray",xlim=c(0,100))
    # dens <- density(x)
    # axis(side=1, at=seq(0,100, 20), labels=seq(0,100,20))
    # lines(dens$x,dens$y*(1/sum(myhist$density))*length(x))

  if(do_plots){
    # pdat_hist=hist(exp_mat,col='darkgrey',prob=T,...)                  ##  type='n', does not wor, use col=NULL,border=NULL to remove the bars
    pdat_hist=hist(exp_mat,plot=F,prob=T,...)
    pdat_hist=hist(exp_mat,col=rgb(0, 0, 0,alpha=0.2),border=rgb(0, 0, 0,alpha=0.2),prob=T,...)
    # plot(exp_mat,type='n',ylim=c(0,1),xlim=c(floor(min(exp_mat)),ceiling(max(exp_mat))))
#    hist(lower,col=rgb(1,0,0,0.5),breaks=max(30,100*(nrow(lower)/nrow(exp_mat))),add=T)
#    hist(upper,col=rgb(0,0,1,0.5),breaks=max(30,100*(nrow(upper)/nrow(exp_mat))),add=T)
  # lines(density(M, na.rm=T), lty=2, lwd=2)
  # lines(density(M, na.rm=T)$x,dens$y*(1/sum(pdat_hist$density))*nrow(exp_mat), lty=2, lwd=2)    ##  scaling for prob=F
  par(new=T)
   # lines(density(lower, na.rm=T)$x,dens$y*(1/sum(pdat_hist$density))*nrow(exp_mat), lty=1, lwd=2,col='dodgerblue')    ##  scaling for prob=F
  lines(density(lower, na.rm=T), lty=1, lwd=2,col='darkred')                                                        ##  no scaling - used for prob=T
  par(new=T)
   # lines(density(midle, na.rm=T)$x,dens$y*(1/sum(pdat_hist$density))*nrow(exp_mat), lty=1, lwd=2,col='chartreuse4')
  lines(density(midle, na.rm=T), lty=1, lwd=2,col='chartreuse4')
  par(new=T)
  # lines(density(upper, na.rm=T)$x,dens$y*(1/sum(pdat_hist$density))*nrow(exp_mat), lty=1, lwd=2,col='darkred')
  lines(density(upper, na.rm=T), lty=1, lwd=2,col='dodgerblue')
    if(points){rug(exp_mat,col=rgb(0, 0, 0,alpha=0.2))}   # add tick marks below histogram

    pall=(shapiro.test(as.vector(exp_mat)))
    plow=(shapiro.test(lower))
    pmid=(shapiro.test(midle))
    ptop=(shapiro.test(upper))
    


#    legend(x="topright",pch=15,box.lwd=0,box.col="white",col=c('darkgrey',rgb(1,0,0,0.5),rgb(0,0,1,0.5)),pt.bg=c('darkgrey',rgb(1,0,0,0.5),rgb(0,0,1,0.5)),legend=c(paste0('n=',nrow(exp_mat)),paste0('n=',nrow(lower)),paste0('n=',nrow(upper))),cex=1)
    legend(x="topright",pch=15,box.lwd=0,box.col="white",col=c('darkgrey','darkred','chartreuse4','dodgerblue'),pt.bg=c('darkgrey',rgb(1,0,0,0.5),rgb(0,0,1,0.5),rgb(0,1,0,0.5)),
      legend=c(
        paste0('density n=',nrow(exp_mat),'  p.norm=',formatC(signif(pall$p.value),digits=2))
        ,paste0('lower n=',nrow(lower),'  p.norm=',formatC(signif(plow$p.value),digits=2))
        ,paste0('midle n=',nrow(midle),'  p.norm=',formatC(signif(pmid$p.value),digits=2))
        ,paste0('upper n=',nrow(upper),'  p.norm=',formatC(signif(ptop$p.value),digits=2))
        )

,cex=1)
    
    cat('\n\tdataset n=',nrow(exp_mat),'lower n=',nrow(lower),'middle n=',nrow(midle),'upper n=',nrow(upper),'\n')
    cat('\tshapiro normality test - lower',formatC(signif(plow$p.value),digits=2),' middle - ',formatC(signif(pmid$p.value),digits=2),'upper',formatC(signif(ptop$p.value),digits=2),'\n')
  }



  return(invisible(list(lower=lower,middle=midle,upper=upper)))

}

####  a mediocre toy dataset to show bimodal distribution
##x=matrix(rnorm(9000, mean = 0, sd = 0.5),ncol=3)
##y=matrix(rnorm(27000, mean = 5, sd = 4),ncol=3)
##z=rbind(x,y)
##deconv(z,T)

}



is.missing<-function(dat_mat,do_plots=F,use_grid=F,dat_descr='',define_na=c(NA,NaN),colvec=c('#9ebcda','#e6550d')){
##  check for missing values in dat_mat, by cols
##  do_plots=T to visualise missing (red) & non-missing (blue)
	misd=apply(dat_mat,2,function(x)(x%in%define_na))
		rownames(misd)=rownames(dat_mat)

	n_overlap=sum(apply(misd,1,sum)==0)
	cat('\t------------------   of total samples',nrow(dat_mat),', ',n_overlap,' (',frac(n_overlap,nrow(dat_mat),perc=T),'%) remaining if "complete.cases()"  ------------------\n')

	misn=as.data.frame(sort(apply(misd,2,sum),decreasing=T))
		colnames(misn)='count'
	misn$percent=round(misn$count/nrow(misd),digits=3)

	if(n_overlap<nrow(dat_mat)){
		cat('\t\t\t\tcolumns with missing dt (limited to to 10):\n\n')
		print(misn[1:min(nrow(misn),10),])

	}

	if(dat_descr!=''){
		paste0(dat_descr,'\n')
	}

	if(do_plots & !use_grid){
		misd=misd*1
		Library('gplots')
		heatmap.2(make.numeric(misd),breaks=seq(0,1,length=(3)),col=colvec,tracecol=F,dendrogram='both',Rowv=T,Colv=T,margins=c(12,12),density.info='none',keysize=1,cexCol=1.2,cexRow=1.2,symkey=F)

		mtext(dat_descr,adj=1,side=3,line=2)
		mtext(paste0('n.complete : ',n_overlap),adj=1,side=3)
	}

	if(do_plots & use_grid){
		misd=misd*1
		Library('gplots')
		heatmap.2(make.numeric(misd),breaks=seq(0,1,length=(3)),col=colvec,tracecol=F,dendrogram='both',Rowv=T,Colv=T,margins=c(12,12),density.info='none',keysize=1,cexCol=1.2,cexRow=1.2,symkey=F,colsep=0:ncol(data_mat),rowsep=0:nrow(data_mat),sepcolor='white',sepwidth=c(0.05,0.05))

		mtext(dat_descr,adj=1,side=3,line=2)
		mtext(paste0('n.complete : ',n_overlap),adj=1,side=3)
	}
	return(invisible(misn))
}



rmerge<-function(data_mat1,data_mat2,all=T,verbose=T){
  dat_out=merge(data_mat1,data_mat2,by="row.names",all=all)
    rownames(dat_out)=dat_out$Row.names
  dat_out=dat_out[,-which(colnames(dat_out)=="Row.names")]

  if(verbose){(cat('\n\t\tnrow x=',nrow(data_mat1),',  y=',nrow(data_mat2),',  merged=',nrow(dat_out),'\n\n',sep=''))}
  return(invisible(dat_out))
}



rmerge.list<-function(dat_lis,all=T,verbose=T){
  lis_merge=rmerge(dat_lis[[1]],dat_lis[[2]],verbose=verbose)
    colnames(lis_merge)=c(paste(names(dat_lis)[1],colnames(dat_lis[[1]]),sep='.'),paste(names(dat_lis)[2],colnames(dat_lis[[2]]),sep='.'))
  if(verbose){cat('\tnrows inputs:',nrow(dat_lis[[1]]),nrow(dat_lis[[1]])==nrow(dat_lis[[2]]))}   ##  keeping track of nrows in each list
  if(length(dat_lis)>2){
    for(ilis in 3:length(dat_lis)){
      if(verbose){cat(' ',nrow(dat_lis[[1]])==nrow(dat_lis[[ilis]]))}               ##  keeping track of nrows in each list
      lis_merge=rmerge(lis_merge,dat_lis[[ilis]],verbose=verbose)
        colnames(lis_merge)[ilis]=paste(names(dat_lis)[ilis],colnames(dat_lis[[ilis]]),sep='.')
    }
  }
  if(verbose){cat('\tnrows output:',nrow(dat_lis[[1]])==nrow(lis_merge),'\n')}                    ##  keeping track of nrows final output
  return(invisible(lis_merge))
}



###•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•###
###•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•###



celltr<-function(clusters_list,runname="",inpath='~/Dropbox/SHARED/tools/Data_to_load_CellFET/',outpath=getwd(),selection=F){ #inpath="~/Dropbox/SHARED/tools/Data_to_load_CellFET",
cat('\tNOTE:\tclusters_list only ENSG ids currently supported')
cat("\tCell background is the genes with one2one human orthologous of mice genes used to build the list of cell class enriched genes by Zeisel et al 2015(Science)\n")
  # library(MetaDE)
  # library('parallel')

  ###load data
 # Load(paste(inpath,"/HUMQb.Rdata",sep=""))            #"HUMQb" human ENSid orthologous genes of mice background genes
 # Load(paste(inpath,"/hmscDF_neuron.Rdata",sep=""))    #"hmscDF" human ENSid orthologous of mice single cell enriched by class dataframe
  
  ### create a matrix for results
  cFET=matrix(nrow=length(clusters_list), ncol=14)
  row.names(cFET)=names(clusters_list)
  colnames(cFET)=c("cell class","FET p.value","FET FDR","OR","[95% OR CI] inf","OR [95% OR CI] sup",
                      "module cell enriched genes","module out cell enriched genes",
                      "non module cell enriched genes","non module out cell enriched genes",
                      "gene names of in modules cell enriched genes","module size","cell enriched genes size",
                      "% of genes in module in cell background"
  )

  
  resMsc=list()
  for(ccl in 1:length(hmscDF)){ # ccl: cell class
    cat('\t=====================',names(hmscDF)[ccl],'=====================',ccl,' of ',length(hmscDF),'\n')
    cclENS=hmscDF[[ccl]]
    ### function to fill the matrix of results
    #for(i in 1:length(clusters_list)){
    FUNC=function(i){
      Ms=length(clusters_list[[i]]) #Ms: module size
      CB=HUMQb[,'hsapiens_homolog_ensembl_gene'] #CB: cell background
      Cs=length(cclENS) #Cs: cell enriched genes size
      MCBp=length(intersect(CB,clusters_list[[i]]))/Ms #MCBp: % of genes in module in cell background

      #cFET
      cat('\t\t',names(clusters_list)[i],'\n')
      #calculate the number Mc of module i cell enriched genes(Mc: in module AND in cell class)
      Mc=length(intersect(cclENS,clusters_list[[i]]))
      McID=paste(unlist(HUMQb[which(CB %in% intersect(cclENS,clusters_list[[i]])),'external_gene_name']),collapse=", ")
      #calculate the number NMc of remaining genes not in module but in cell class
      NMc=length(cclENS)-Mc
      #calculate the number Mnc of genes in module but not in cell class
      Mnc=length(intersect(CB,clusters_list[[i]]))-Mc
      #calculate the number NMnc of genes out of module AND not in cell class
      NMnc=length(CB)-(Mc+NMc+Mnc)
      # contingency matrice for Fisher Exact Test FET all DNMs and ns DNMs
      matr=matrix(c(Mc,NMc,Mnc,NMnc), nrow=2)
      #FET
      #FisherM=fisher.test(matr,alternative="greater")
      FisherM=fisher.test(matr)
      Fisher.p=FisherM$p.value
      Fisher.or=FisherM$estimate
      Fisher.cinf=FisherM$conf.int[1]
      Fisher.cis=FisherM$conf.int[2]
      cFET[i,]=c(names(hmscDF)[ccl],Fisher.p,NA,Fisher.or,Fisher.cinf,Fisher.cis,Mc,Mnc,NMc,NMnc,McID,Ms,Cs,MCBp)
    }
#    cfet=mclapply(1:length(clusters_list),FUNC,mc.cores=detectCores())
    cfet=lapply(1:length(clusters_list),FUNC)
    #The cfet output object of the mclapply function is a list of n vectors cFET[i,] in the good order
    for(i in 1:length(clusters_list)){
      cFET[i,]=cfet[[i]]
    }
    cFET[,"FET FDR"]=p.adjust(cFET[,"FET p.value"],method="fdr")
#    write.table(cFET, sep='\t', file=paste(outpath,"/",names(hmscDF)[ccl],"_cFET_",runname,".txt",sep=""), row.names=TRUE, quote=FALSE, col.names=NA)
    resMsc[[ccl]]=cFET
  }  
  names(resMsc)=names(hmscDF)
  if(selection==T){
    select=which(as.numeric(resMsc[[1]][,"FET FDR"]) < 0.2)
    cat("number of selected modules for ", names(resMsc)[1]," :",length(select),'\n')
    if(length(select)==1){
      SignifT=resMsc[[1]][c(select,NA),]
      SignifT=SignifT[-which(is.na(rownames(SignifT))==T),]
    }else{
      SignifT=resMsc[[1]][select,]
    }
    for(ccl in 2:length(resMsc)){
      select=which(as.numeric(resMsc[[ccl]][,"FET FDR"]) < 0.2)
      cat("number of selected modules for ", names(resMsc)[ccl]," :",length(select),'\n')
      if(length(select)==1){
        SignifT=rbind(SignifT,resMsc[[ccl]][c(select,NA),])
        SignifT=SignifT[-which(is.na(rownames(SignifT))==T),]
      }else{
        SignifT=rbind(SignifT,resMsc[[ccl]][select,])
      }          
    }
#    write.table(SignifT, sep='\t', file=paste(outpath,'/significant_cFET_',runname,'.txt',sep=''), row.names=TRUE, quote=FALSE, col.names=NA) 
  }else{
    allT=resMsc[[1]]
    for(ccl in 2:length(resMsc)){
      allT=rbind(allT,resMsc[[ccl]])
    }
#    write.table(allT, sep='\t', file=paste(outpath,'/ALL_cFET_',runname,'.txt',sep=''), row.names=TRUE, quote=FALSE, col.names=NA) 
  }
  return(as.data.frame(lapply(resMsc,function(x){x[,'FET p.value']})))     
}




dnmr<-function(dat_lis,dtb='default',phen=''){ #bkg,
##  dtb - required list - DNM in 'conrols' ie healthy parents & offspring
##   dnmDB - pre-made dataset, part of "adds" package, downloaded from http://denovo-db.gs.washington.edu/denovo-db/  ## mapped to HUGO gene ids, genes with DNM in controls removed from DNM in
  if(dtb[1]=='default'){
    cat('\tLoad(~/Dropbox/PROJ/ednm/dtb/denovo-db.variants.v.1.5__frameshift_missense_stopgain.Rdata)\n')
    # Load('~/Dropbox/PROJ/ednm/dtb/denovo-db.variants.v.1.5__frameshift_missense_stopgain.Rdata')
    dtb=dnmd
  }      ##  load pre-made dataset, part of "adds" package
  if(!('control'%in%names(dtb))){stop('"control" - list of DNM in healthy controls && offspring (named "control") is required')}


   cat('\tcheck DNM dtb and background compatibility\n')
  # bkg=overlap(unlist(dtb),bkg)

  # bkg=c(bkg$inter,bkg$inb)  ##  background is specified as overap && 'expressed' - ie have significant signal in the dataset used to derive the list
  ## the above definition is the same as the "bkg" input by definiton..

  dtb_contr=dtb$control
  dtb=dtb[names(dtb)!='control']

   cat('\n')
  # str(bkg)
  if(length(intersect(unlist(dtb),unlist(dat_lis)))==0){stop('check that dat_lis and bkg IDs are HUGO gene names OR match the provided dtb')}
  # dtb=lapply(dtb,function(x){x[x%in%bkg]})
  dnmen=list()
  fetp=list()
  cat('\n\nperform FET enrichment\n')
  for(idnm in names(dtb)){
    cat('\t',idnm)
    holder=list()
    for(idat in names(dat_lis)){
      holder[[idat]]=unlist(fet(
              # sampl=dat_lis[[idat]]
              # ,bkgrnd=bkg
              # ,success=dtb[[idnm]]
              # ,
              counts=T
              ,samp.success=sum(dtb[[idnm]]%in%dat_lis[[idat]])
              ,bkgrnd.success=sum(dtb_contr%in%dat_lis[[idat]])
              ,samp.fail=sum(!(dtb[[idnm]]%in%dat_lis[[idat]]))
              ,bkgrnd.fail=sum(!(dtb_contr%in%dat_lis[[idat]]))
              ,tail='greater'
              ))

    }
    dnmen[[idnm]]=as.data.frame(t(as.data.frame(holder)))
    fetp[[idnm]]=dnmen[[idnm]]$FETp

  }
    cat('\n\n')

  fetp=as.data.frame(fetp)
    rownames(fetp)=names(dat_lis)

  return(list(fetp=fetp,dnmen=dnmen))
}



eefnr<-function(dat_lis,dtb='default',phen=''){ #bkg,
##  dtb - required list - DNM in 'conrols' ie healthy parents & offspring
##   dnmDB - pre-made dataset, part of "adds" package, downloaded from http://denovo-db.gs.washington.edu/denovo-db/  ## mapped to HUGO gene ids, genes with DNM in controls removed from DNM in
  if(dtb[1]=='default'){
    # cat('\tLoad(~/Dropbox/PROJ/ednm/dtb/denovo-db.variants.v.1.5__frameshift_missense_stopgain.Rdata)\n')
    # Load('~/Dropbox/PROJ/ednm/dtb/denovo-db.variants.v.1.5__frameshift_missense_stopgain.Rdata')
    dtb=eefndb
  }      ##  load pre-made dataset, part of "adds" package
  if(!('control'%in%names(dtb))){stop('"control" - list of DNM in healthy controls && offspring (named "control") is required')}


   cat('\tcheck DNM dtb and background compatibility\n')
  # bkg=overlap(unlist(dtb),bkg)

  # bkg=c(bkg$inter,bkg$inb)  ##  background is specified as overap && 'expressed' - ie have significant signal in the dataset used to derive the list
  ## the above definition is the same as the "bkg" input by definiton..

  dtb_contr=dtb$control
  dtb=dtb[names(dtb)!='control']

   cat('\n')
  # str(bkg)
  if(length(intersect(unlist(dtb),unlist(dat_lis)))==0){stop('check that dat_lis and bkg IDs are HUGO gene names OR match the provided dtb')}
  # dtb=lapply(dtb,function(x){x[x%in%bkg]})
  dnmen=list()
  fetp=list()
  cat('\n\nperform FET enrichment\n')
  for(idnm in names(dtb)){
    cat('\t',idnm)
    holder=list()
    for(idat in names(dat_lis)){
      holder[[idat]]=unlist(fet(
              # sampl=dat_lis[[idat]]
              # ,bkgrnd=bkg
              # ,success=dtb[[idnm]]
              # ,
              counts=T
              ,samp.success=sum(dtb[[idnm]]%in%dat_lis[[idat]])
              ,bkgrnd.success=sum(dtb_contr%in%dat_lis[[idat]])
              ,samp.fail=sum(!(dtb[[idnm]]%in%dat_lis[[idat]]))
              ,bkgrnd.fail=sum(!(dtb_contr%in%dat_lis[[idat]]))
              ,tail='greater'
              ))

    }
    dnmen[[idnm]]=as.data.frame(t(as.data.frame(holder)))
    fetp[[idnm]]=dnmen[[idnm]]$FETp

  }
    cat('\n\n')

  fetp=as.data.frame(fetp)
    rownames(fetp)=names(dat_lis)

  return(list(fetp=fetp,dnmen=dnmen))
}






fet<-function(sampl,bkgrnd,success,counts=F,samp.success,bkgrnd.success,samp.fail,bkgrnd.fail,tail='greater'){  # ,alternative='greater',...
# alternative ='greater'
# phyper(success_in_sample, success_in_bkgd, failure_in_bkgd, sample_size, lower.tail=TRUE)

#fisher.test(matrix(c(x, 13-x, 5-x, 34+x), 2, 2), alternative='less');
# Numerical parameters in order:
# (success-in-sample, success-in-left-part, failure-in-sample, failure-in-left-part).
  if(!counts){
      bkgrnd=bkgrnd[!(bkgrnd%in%sampl)]
      test_res=list(samp.success=sum(sampl%in%success),bkgrnd.success=sum(bkgrnd%in%success),samp.fail=sum(!(sampl%in%success)),bkgrnd.fail=sum(!(bkgrnd%in%success)))
      test_mat=matrix(unlist(test_res),nrow=2,dimnames=list(c('samp','bkgrnd'),c('success','fail')))
      test_out=fisher.test(test_mat,alternative=tail)
  
#   print(test_mat)

      test_res$n.genes=length(sampl)
      test_res$FETp=(test_out$p.value)
      test_res$fetOR=round(test_out$estimate) #,digits=3 
      test_res$lowerCI=round(test_out$conf.int[1])#,digits=3
      test_res$upperCI=round(test_out$conf.int[2])#,digits=3
#   test_res$samp.success=paste(sampl[sampl%in%success],collapse=' ')
      return(((test_res)))
  }

  if(counts){
    test_res=list(samp.success=samp.success,bkgrnd.success=bkgrnd.success,samp.fail=samp.fail,bkgrnd.fail=bkgrnd.fail)
    test_mat=matrix(unlist(test_res),nrow=2,dimnames=list(c('samp','bkgrnd'),c('success','fail')))
    test_out=fisher.test(test_mat,alternative=tail)
    
#      print(test_mat)

    test_res$n.genes=sum(samp.success,samp.fail)
    test_res$FETp=(test_out$p.value)
    test_res$fetOR=round(test_out$estimate,digits=3)
    test_res$lowerCI=round(test_out$conf.int[1],digits=3)
    test_res$upperCI=round(test_out$conf.int[2],digits=3)
#   test_res$samp.success=paste(sampl[sampl%in%success],collapse=' ')
    return(((test_res)))

  }
}






lm.mat<-function(data_mat,verbose=F,symm_fix=T){
##  USE:  ~ lm for matrix of variables (pairwise) ~ cor.test()
##  WARNING: y variable can-not be categorical => if 2 variables are both categorical, rest fixed by symmetry
##  INPUT: matrix/data.frame of variables rows=samples, columns=variables (numeric or factor)
##  missing values can be used, only relevant variable n will be affected
##  for factor variables (in data_mat), min P of lm() is used\n\n")#
  data_mat=as.data.frame(data_mat)
    lmpstat=matrix(NA,ncol=ncol(data_mat),nrow=ncol(data_mat))
    colnames(lmpstat)=colnames(data_mat)
    rownames(lmpstat)=colnames(data_mat)
    rsqstat=lmpstat
    nsample=lmpstat
    k=1
    for(yvar in colnames(data_mat)){
        for(xvar in colnames(data_mat)){
            if(yvar!=xvar){
                tester=data_mat[,c(yvar,xvar)]
                tester=tester[complete.cases(tester),]
                nsample[xvar,yvar]=nrow(tester)
              if(!is.factor(tester[,yvar])){    ##  y variable can-not be a factor
                holder=summary(lm(as.matrix(tester[,yvar,drop=F])~.,data=tester[,xvar,drop=F]))
                dummy=holder$coefficients
                dummy=dummy[-1,,drop=F]
         #### using min for factors - 1 p-value per factor ==> only interested in the lowest one
                dummy=dummy[which(dummy[,"Pr(>|t|)"]==min(dummy[,"Pr(>|t|)"])),]    ##  not doing this risk selecting the wrong sign for Rsq below if factor
                lmpstat[xvar,yvar]=dummy['Pr(>|t|)']
                rsqstat[xvar,yvar]=holder$r.sq*sign(dummy['Estimate']) ## add the direction of relationship
              }
            }
        }
    k=lcount(k,length(colnames(data_mat)))
    }

    if(symm_fix){
      ##  y variable can-not be a factor -> fix missing by symmetry,by construction, only columns are affected (and not rows)
      decider=apply(rsqstat,2,function(x){(sum(is.na(x))==length(x))})
  #  rows and columns - same order by construction..
    rsqstat[,decider]=t(rsqstat[decider,])
    lmpstat[,decider]=t(lmpstat[decider,])
    diag(rsqstat)=1
    diag(lmpstat)=1
    }

return(invisible(list(lmp=lmpstat,rsq=rsqstat,nsamp=nsample)))
}



###•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•###
###•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•###



bgcommon<-function(list_dat,transform=F,dat_mat='',union=F,verbose=T){		#help=F,
# if(help){
#  cat("\n\tUSE\t: determine common background (union or intersect) across all entries in expression list\n")
#  cat("\tNOTE\t: list_dat - list of expression matrices: row=genes, col=samples\n")
#  cat("\tNOTE\t: transfrom=T also returns the list_dat processed for common bg\n")
#  cat("\tNOTE\t: dat_mat required only if union=T & transform=T \n")
# }
  if(verbose){cat('\tcalculate common background across',length(list_dat),'datasets\n')}

   bgcommon=rownames(list_dat[[1]])
  for(ilis in 2:length(list_dat)){
     if(!union){bgcommon=intersect(bgcommon,rownames(list_dat[[ilis]]))}
     if(union){bgcommon=union(bgcommon,rownames(list_dat[[ilis]]))}
  }

  if(!union&verbose){cat("\n\tintersect:",length(bgcommon),"genes common to all",length(list_dat),"datasets\n")}
  if(union&verbose){cat("\n\tunion",length(bgcommon),"genes common to all",length(list_dat),"datasets\n")}

  

  if(!transform){return(invisible(bgcommon))}

  if(transform & !union){
    if(verbose){cat('\ttransform all datasets to "intersect" bakcground\n')}
   listt=list()
    for(ilis in 1:length(list_dat)){
     listt[[names(list_dat)[ilis]]]=list_dat[[names(list_dat)[ilis]]][bgcommon,,drop=F]
    }

    return((listt))
  }

  if(transform & union){
    if(verbose){cat('\ttransform all datasets to "union" bakcground\n')}
   listt=list()
    for(ilis in 1:length(list_dat)){
     listt[[names(list_dat)[ilis]]]=dat_mat[bgcommon,colnames(list_dat[[names(list_dat)[ilis]]]),drop=F]
    }

    return((listt))
  }

}



###•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•###
###•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•##•###
#contrast=c("condition","treated","untreated")
deg.limma<-function(dat_mat,cov_mat,contrast=NULL,deg_type=c('univariate'),sanity=T,verbose=T){  #'multivariate'
##  differential gene expression using "limma" pkg
##  dat_mat - rows=genes, cols=samples
##  cov_mat - data.frame - basis for making model.matrix(~.,data=cov_mat)
##        + alternatively can be used as covariate eg factor -> levels, continuous - as is
##  contrast    - specify which colname from cov_mat - important if want to look at the combined effect of a factor variable with >2 levels
  if(length(contrast)!=1& 'multivariate'%in%deg_type){stop('for deg_type=="multivariate", contrast isrequired - specify a colname in "cov_mat" to use as the primary comparison - essential only for factors with n.levels>2')}


  dat_class=dat.class(cov_mat)
  if(sum(!dat_class%in%c('factor','numeric'))!=0){stop('expect class(cov_mat) as factor or numeric only, convert character to numeric or factor')}

  if(!sanity){
    order_check=sum(colnames(dat_mat)!=rownames(cov_mat))
    if(order_check!=0){
      warning('\n\t\tWARNING\t: (colnames(dat_mat)==rownames(cov_mat)) shows : ',order_check,' not in same order','\n')
    }
  }
  if(sanity){
    holder=intersect(colnames(dat_mat),rownames(cov_mat))
    dat_mat=dat_mat[,holder,drop=F]
    cov_mat=cov_mat[holder,,drop=F]
     if(verbose){cat('\t\tcolnames(dat_mat) n=',ncol(dat_mat),' rownames(cov_mat) n=',nrow(cov_mat),' same order',sum(colnames(dat_mat)==rownames(cov_mat)),'\n')}
  }

 Library('limma')
   if('multivariate'%in%deg_type){
     cat('\n\tperform multivariate model for all covariates provided\n')

    mltvar=list()
      desm=model.matrix(~., data=cov_mat)
#       colnames(desm)=c('intercept',colnames(cov_mat))
      degall=lmFit(dat_mat, desm)
      degall=eBayes(degall)

    for(ides in colnames(desm)){
       lprogr(ides,colnames(desm))
      mltvar[[ides]]=topTable(degall, coef=ides, adjust="BH", number=nrow(dat_mat))
        # mltvar[[ides]]=mltvar[[ides]][,c('logFC','P.Value','adj.P.Val')]
        colnames(mltvar[[ides]])[colnames(mltvar[[ides]])=='P.Value']='Pval'
        colnames(mltvar[[ides]])[colnames(mltvar[[ides]])=='adj.P.Val']='FDR'
        
      }

      mltvar[[paste0('all.',contrast)]]=topTable(degall, coef=colnames(desm)[grepl(contrast,colnames(desm))], adjust="BH", number=nrow(dat_mat))
        colnames(mltvar[[paste0('all.',contrast)]])[colnames(mltvar[[paste0('all.',contrast)]])=='P.Value']='Pval'
        colnames(mltvar[[paste0('all.',contrast)]])[colnames(mltvar[[paste0('all.',contrast)]])=='adj.P.Val']='FDR'


    ndeg=unlist(lapply(mltvar,function(x){sum(x$FDR<0.01)}))
    degg=(lapply(mltvar,function(x){rownames(x)[x$FDR<0.01]}))


    # write.file(degg['SeizureFree2'],file='/Users/ks907/Dropbox/PROJ/epitar/methyl/out/txt/multivar_lm.SeizureFree2.txt',row.names=F,col.names=F)
    # write.file(degg[['SeizureFree3']],file='/Users/ks907/Dropbox/PROJ/epitar/methyl/out/txt/multivar_lm.SeizureFree3.txt',row.names=F,col.names=F)
    # write.file(degg[['all.SeizureFree']],file='/Users/ks907/Dropbox/PROJ/epitar/methyl/out/txt/multivar_lm.all.SeizureFree.txt',row.names=F,col.names=F)

  }

  if('univariate'%in%deg_type){
     cat('\n\tperform univariate model for all covariates provided\n')

    univar=list()
    for(icov in colnames(cov_mat)){
     lprogr(icov,colnames(cov_mat))
      desm=model.matrix(~.,data=cov_mat[,icov,drop=F])
#       colnames(desm)=c('intercept',icov)
      degsin=lmFit(dat_mat, desm)
      degsin=eBayes(degsin)
####  for Factor variables, this will select
#     for(ides in colnames(desm)){
# #      cat('\t',ides,'\n')
#       univar[[ides]]=topTable(degsin, coef=ides, adjust="BH", number=nrow(dat_mat))
#         univar[[ides]]=univar[[ides]][,c('logFC','P.Value','adj.P.Val')]
#         colnames(univar[[ides]])=c('logFC','Pval','FDR')

#     }
    univar[[icov]]=topTable(degsin,coef=NULL, adjust="BH", number=nrow(dat_mat))
      colnames(univar[[icov]])[grepl('P.Value',colnames(univar[[icov]]))]='Pval'
      colnames(univar[[icov]])[grepl('adj.P.Val',colnames(univar[[icov]]))]='FDR'
              head(univar[[icov]])

     # 'topTableF' ranks genes on the basis of moderated F-statistics for
     # one or more coefficients. If 'topTable' is called and 'coef' has
     # two or more elements, then the specified columns will be extracted
     # from 'fit' and 'topTableF' called on the result. 'topTable' with
     # 'coef=NULL' is the same as 'topTableF', unless the fitted model
     # 'fit' has only one column.

  }
  }

    # readme='\tunivar\t- limma using one covariate at a time
    #     \tmultvar\t- limma using all covariates in same model
    #     \tmodel.m\t- model.matrix(~0+., data=cov_mat) used to generate results
    #     '
    
    # return(invisible(list(univar=univar,multvar=mltvar,readme=readme)))
    if('univariate'%in%deg_type & !'multivariate'%in%deg_type){return(invisible((univar)))}

    if(!'univariate'%in%deg_type & 'multivariate'%in%deg_type){return(invisible((mltvar)))}

    if('univariate'%in%deg_type & 'multivariate'%in%deg_type){return(invisible(list(univar=univar,multvar=mltvar)))}
}





# deg.limma<-function(dat_mat,cov_mat,sanity=T,verbose=T){
# ##  differential gene expression using "limma" pkg
# ##  dat_mat - rows=genes, cols=samples
# ##  cov_mat - data.frame - basis for making model.matrix(~.,data=cov_mat)
# ##				+ alternatively can be used as covariate eg factor -> levels, continuous - as is
# 	dat_class=dat.class(cov_mat)
# 	if(sum(!dat_class%in%c('factor','numeric'))!=0){stop('expect class(cov_mat) as factor or numeric only, convert character to numeric or factor')}

# 	if(!sanity){
# 		order_check=sum(colnames(dat_mat)!=rownames(cov_mat))
# 		if(order_check!=0){
# 			warning('\n\t\tWARNING\t: (colnames(dat_mat)==rownames(cov_mat)) shows : ',order_check,' not in same order','\n')
# 		}
# 	}
# 	if(sanity){
# 		holder=intersect(colnames(dat_mat),rownames(cov_mat))
# 		dat_mat=dat_mat[,holder,drop=F]
# 		cov_mat=cov_mat[holder,,drop=F]
# 		 if(verbose){cat('\t\tcolnames(dat_mat) n=',ncol(dat_mat),' rownames(cov_mat) n=',nrow(cov_mat),' same order',sum(colnames(dat_mat)==rownames(cov_mat)),'\n')}
# 	}

#  Library('limma')
# 	if(ncol(cov_mat)==1){
#     cat('\n\tunivariate limma model ~',colnames(cov_mat),'\n')
# 		desm=model.matrix(~.,data=cov_mat)
# 			# colnames(desm)=c('intercept',colnames(cov_mat))        ##  this will only work for numeric vars
# 		degall=lmFit(dat_mat, desm)
# 		degall=eBayes(degall)
# 		# degall=topTable(degall,coef=colnames(cov_mat), adjust="BH", number=nrow(dat_mat))
#     degall=topTable(degall,coef=3, adjust="BH", number=nrow(dat_mat))
# 		degall=degall[,c('logFC','P.Value','adj.P.Val')]
# 			colnames(degall)=c('logFC','Pval','FDR')
#       Table(degall$FDR<0.01)
# 		return(invisible(degall))
# 	}

# 	if(ncol(cov_mat)>1){
# 		univar=list()
# 		mltvar=list()

# 			desm=model.matrix(~., data=cov_mat)
# #				colnames(desm)=c('intercept',colnames(cov_mat))
# 			degall=lmFit(dat_mat, desm)
# 			degall=eBayes(degall)

#     for(ides in colnames(desm)){
#       mltvar[[ides]]=topTable(degall, coef=ides, adjust="BH", number=nrow(dat_mat))
#         mltvar[[ides]]=mltvar[[ides]][,c('logFC','P.Value','adj.P.Val')]
#         colnames(mltvar[[ides]])=c('logFC','Pval','FDR')
#       }


# 		for(icov in colnames(cov_mat)){
# #      cat('\t------------------   ',icov,'   ------------------\n')
# 			desm=model.matrix(~.,data=cov_mat[,icov,drop=F])
# #				colnames(desm)=c('intercept',icov)
# 			degsin=lmFit(dat_mat, desm)
# 			degsin=eBayes(degsin)

#     for(ides in colnames(desm)[2:ncol(desm)]){
# #      cat('\t',ides,'\n')
#       univar[[ides]]=topTable(degsin, coef=ides, adjust="BH", number=nrow(dat_mat))
#         univar[[ides]]=univar[[ides]][,c('logFC','P.Value','adj.P.Val')]
#         colnames(univar[[ides]])=c('logFC','Pval','FDR')

# 		}
#   }
# 		readme='\tunivar\t- limma using one covariate at a time
# 				\tmultvar\t- limma using all covariates in same model
#         \tmodel.m\t- model.matrix(~0+., data=cov_mat) used to generate results
# 				'
# 		return(invisible(list(univar=univar,multvar=mltvar,readme=readme)))
# 	}
# }




degr<-function(count_mat,samp_inf,design_var=NA,contrast=NA,method='DESeq2',normcont=F,check_contrast_levels=T){
##  method    - options=c('edgeR','DESeq2','both')
##  normcont    - use deseq2 functionality to return normalised counts etc
##  contrast    - contrast=c("condition","treated","untreated"), required for "results()" - "is recommended for exact specification of the levels which should be compared and their order"
##  count_mat   - matrix of counts, all mapped should be used, including all zero rows (rows-genes, cols-samples)
##  samp_inf    - sample information matrix (rows-samples, cols-variables)
##  design_var   - char string of dependent variables, ie colnames(samp_inf) to be included in model forumla
  cat('\n\t=====================    Sanity checks    =====================\n')

     cat('\tcheck - number of samples in same order',frac(sum(colnames(count_mat)==rownames(samp_inf)),ncol(count_mat),perc=T),'% \n')

  if(is.na(design_var[1])){stop('missing required variable - design_var: char string of dependent variables, ie colnames(samp_inf) to be included in model forumla')}
  if(sum(design_var%in%colnames(samp_inf))!=length(design_var)){stop('design_var required in the same format/spelling as colnames(samp_inf)')}

  if(length(contrast)!=3|is.na(contrast[1])){stop('"contrast" variable is required, format: contrast=c("condition","treated","untreated"), where condition==column name in design_var')}
  if(length(contrast)==3){cat('\tcontrast matrix specified as :\n\t\t"variable"\t',contrast[1],'\n\t\t"untreated"\t',contrast[3],'\n\t\t"treated"\t',contrast[2],'\n\n')}

##  make sure that the factor levels are correct for this (ie controls are correclty oriented for the model.matrix()
  samp_inf=droplevels(samp_inf[,c(contrast[1],design_var[!(design_var%in%contrast[1])]),drop=F])    ##  re-order the matrix such that contrast is fist, esp if design_var does not have the contrast column
  
  if(check_contrast_levels){
  cat('\t\tremove information from "samp_inf" that is not part of the "contrast" groups\n')
  samp_inf=samp_inf[samp_inf[,contrast[1]]%in%contrast[2:3],,drop=F]
  }
  samp_inf[,contrast[1]]=relevel(samp_inf[,contrast[1]],contrast[3])
  str(samp_inf)

if(check_contrast_levels){
  cat('\t\tadjust "count_mat" to match samp_inf, assumes rownames(samp_inf)==colnames(count_mat)\n')
  count_mat=count_mat[,rownames(samp_inf)]
  cat('\tnrow(samp_inf)=',nrow(samp_inf),' ncol(count_mat)=',ncol(count_mat),'\n',sep='')
}

if(method=='DESeq2'|method=='both'){
  cat('\n\t=====================    perform differential gene expression using DESeq2 (RNA-seq)    =====================\n')
##  making formula in functions
# https://stackoverflow.com/questions/14678738/pass-formula-to-function-in-r
# https://stackoverflow.com/questions/12967797/is-there-a-better-alternative-than-string-manipulation-to-programmatically-build/12967834#12967834
  Library('DESeq2')
#  cat('\tNOTE: results will be calculated with respect to the last variable in "design_var"\n')  ## superseeded by requiring "contrast" to be explicitly stated (best way to make sure the correct direction of logFC)
  desvar=design_var
  desvar=reformulate(termlabels=design_var)

   cat('\tdesign_var - formula:  ',as.character(desvar),'\n')
  dds=DESeqDataSetFromMatrix(countData=count_mat,colData=samp_inf,design=desvar)
  dds=estimateSizeFactors(dds)
  #estimateDispersions
  dds=DESeq(dds)
  resm=results(dds,contrast)    ##  default - uses the last var in design in DESeqDataSetFromMatrix()
  
  if(normcont){
   cat('\tcalculate norm counts & distance from using rlog (not same as log2)\n')
  rld=rlog(dds,blind=F)
  vsd=assay(vst(dds,blind=F))
  cdis=dist(t(assay(rld)))
  rld=assay(rld)  ##  assay function required to extract the information from the overly complex obj    ##  https://bioconductor.org/packages/release/bioc/vignettes/DESeq2/inst/doc/DESeq2.html#how-do-i-use-vst-or-rlog-data-for-differential-testing
  }
  # cat('\tgenerate DEG results for each variable in design_var\n')    ## ie to see if any have genes with significant association..
  # resl=list()
  # for(ivar in design_var){
  #   dummy=dds@colData@listData[[ivar]]
  #   if(class(dummy)%in%c('factor')){
  #     for(ilev in samp_inf[,ivar]){

  #     }     
  #   }
  # }
  readmedes='\tDESeq2 results using "degdes" function, output as below:
  \t\t- dds\t- object produced by "DESeq()" function..  dds@colData@listData
  \t\t- resm\t- results matrix using "results(dds)" - using the default which takes the last variable in supplied "design_var"
  \t\t- deg\t- data frame of DEG data from resm object, deg=deg[!is.na(deg$padj),] (ie remove data considered "not expressed" by "results()")
  \t\t- vsd\t- normalised counts using the variance stabilising transformation "vst(dds,blind=T)", ie not using covariate info
  \t\t- cdis\t- distance using "dist(t(assay(rld)))"
  '

  deg=as.data.frame(resm@listData)
    rownames(deg)=resm@rownames
  deg=deg[!is.na(deg$padj),]
  deg=deg[order(deg$padj,decreasing=F),]
}

if(method=='edgeR'|method=='both'){
  cat('\n\t=====================    perform differential gene expression using edgeR (RNA-seq)    =====================\n')
##  perform differential gene expression edgeR    ------------------------------
## edgeRUsersGuide.pdf
## 2.10.3 Testing for DE genes (page 20)..
## page 21
##  Alternatively, one can perform QL F-test to test for differential expression. While the likelihood ratio test is a more obvious choice for inferences with GLMs, the QL F-test is preferred as it reflects the uncertainty in estimating the dispersion for each gene. It provides more robust and reliable error rate control when the number of replicates is small. The QL dispersion estimation and hypothesis testing can be done by using the functions glmQLFit() and glmQLFTest().
  Library('edgeR')


  dego=DGEList(counts=count_mat)  ##  add group=samp_inf[,contrast[1]],remove.zeros=T
  dego=calcNormFactors(dego)
  desm=model.matrix(~.,data=samp_inf)
  dego=estimateDisp(dego,desm)

# qlf=list()
# ltr=list()
  # for(ivar in colnames(desm)[2:ncol(desm)]){
  #   lprogr(ivar,colnames(desm)[2:ncol(desm)])
##  perform quasi-likelihood F-tests  ------------------------------
ivar=colnames(desm)[grepl(contrast[1],colnames(desm))]
    holder=glmQLFit(dego,desm)
    holder=glmQLFTest(holder,coef=ivar)
    # qlf[[ivar]]=topTags(holder,n=nrow(count_mat))[[1]]
    qlf=topTags(holder,n=nrow(count_mat))[[1]]
    print(str(qlf))
    return(qlf)

##  perform likelihood ratio tests    ------------------------------
    # holder=glmFit(dego,desm)
    # holder=glmLRT(holder,coef=ivar)
    # ltr[[ivar]]=topTags(holder,n=nrow(count_mat))
  # }
  
  readmeedr='\tedgeR results using quasi-likelihood F-test (qlf) & perform likelihood ratio test (ltr) models
  \t\t- ltr\t- glmQLFTest(glmQLFit()) - one for each of the covariates supplied, case/control is first
  \t\t- qlf\t- glmLRT(glmFit()) - one for each of the covariates supplied, case/control is first
  \t\t- desm\t- design.matrix()
  '

 }

  if(method=='edgeR'){return(invisible(list(qlf=qlf,desm=desm,samp=samp_inf,readme=readmeedr)))}  #ltr=ltr,
  if(method=='DESeq2'& normcont){return(invisible(list(deg=deg,desobj=dds,resm=resm,rld=rld,vsd=vsd,cdis=cdis,samp=samp_inf,readme=readmedes)))}
  if(method=='DESeq2' & !normcont){return(invisible(list(deg=deg,desobj=dds,samp=samp_inf,readme=readmedes)))}
  if(method=='both'){
    edger=list(ltr=ltr,qlf=qlf,desm=desm,samp=samp_inf,readmee=readmeedr)
    deseq=#list(deg=deg,desobj=dds,samp=samp_inf,readme=readmedes)    ##•• turn off vsn ••## list(deg=deg,desobj=dds,resm=resm,vsd=vsd,cdis=cdis,samp=samp_inf,readmed=readmedes)
    return(invisible(c(edger,deseq))) 
  }
}






degsv<-function(
  contmat               ##  raw counts matrix (samples = columns)
  ,datcov               ##  all covariates AND group variable (assumes no irrelevant vars in there)
  ,datgrp               ##  required input eg =c('genotype','ko')   ##  column to use for groups and name of controls (ie the variable to calculate topGenes wrt)
  ,nsvafac=NULL           ##  NULL - caclulates 'optimum number', can overwrite but may give weird / uninformative errors (decrease n factors to fix), some sort of relationship with n.samples & covars ~ degrees of freedom
  ,p_thresh=0.01 
  ){

  Library('edgeR')
  Library('sva')

    # Head(contmat)
  contfilt=contmat[apply(contmat,1,function(x){sum(x<5)!=length(x)}),]    ##  rm all zero only
    # Head(contfilt)

  cpms=cpm(contfilt,normalized.lib.sizes=T,log=F)             ## no log transform since svaseq seems to do that by itself

  datcov=droplevels(datcov)
  mmodf=model.matrix(~.,data=datcov)
  mmodc=model.matrix(~.,data=datcov[,colnames(datcov)!=datgrp[1],drop=F])

  print(head(mmodf))
  print(head(mmodc))
  svafacs=svaseq(as.matrix(cpms),mod=mmodf, mod0=mmodc, n.sv=nsvafac)

    y=DGEList(counts=as.matrix(contmat),group=datcov[,datgrp[1]],remove.zeros=T)
    y=calcNormFactors(y)
    y=estimateDisp(y,mmodf)

    fit=glmFit(y,mmodf)
    lrt=glmLRT(fit,coef=colnames(mmodf)[grepl(datgrp[1],colnames(mmodf))])

    degs=topTags(lrt,n=nrow(contmat))$table
      sum(degs$FDR<p_thresh)
      sum(degs$FDR<0.1)

    degg=list()
    degg$deg=rownames(degs[degs$FDR<p_thresh,])
    degg$degup=rownames(degs[degs$FDR<p_thresh & degs$logFC>0,])
    degg$degdn=rownames(degs[degs$FDR<p_thresh & degs$logFC<0,])

  print(str(degg))

  return(list(degg=degg,degs=degs,svafacs=svafacs,edger=lrt))
}






# deg.edgeR<-function(dat_mat,group,type='RNAseq'){
#   Library('edgeR')
# ##  1.4 Quick start
# ##  edgeR offers many variants on analyses. In general, the glm pipeline is recommended as it offers great flexibilities. There are two testing methods under the glm framework: likelihood ratio test and quasi-likelihood F-test. The details of these two methods are described in Chapter 2.
# ##  A typical edgeR analysis might look like the following. Here we assume there are four RNA-Seq libraries in two groups, and the counts are stored in a tab-delimited text file, with gene symbols in a column called Symbol.
#   if(class(group)!=c('factor')){stop('currently tested to only work for factor(group)')}
#   if(class(group)==c('factor')){
#    cat('\tgroup class -',class(group),'\n')
#   # group=as.factor(group)
#   desm=model.matrix(~group)
#     rownames(desm)=colnames(dat_mat)
#     colnames(desm)=gsub('group','',colnames(desm))
#    cat('\n\t',as.character(group)[1],' - designated as "control"\n\tfirst 3 rows of design matrix :\n',sep='')
#    print(desm[1:3,])


#   edge=DGEList(counts=dat_mat,group=group)
#   edge=calcNormFactors(edge)
#   edge=estimateDisp(edge,desm)

# ##  To perform quasi-likelihood F-tests:
#   fit=glmQLFit(edge,desm)
#   deg=list()
#   cat('\n\tperform comparisons\n')
#   for(igrp in colnames(desm)[2:ncol(desm)]){
#     lprogr(igrp,colnames(desm))
#     deg[[igrp]]=glmQLFTest(fit,coef=igrp)
#   }

# ##  To perform likelihood ratio tests:
#   # lrfit=glmFit(edge,desm)
#   # lrlrt=glmLRT(fit,coef=coef)
#   }
#   return(invisible(list(deg=deg,desm=desm,fit=fit)))
# }


cov.impact<-function(dat_mat,cov_mat,do_plots=c(F,F,F,T,T),gene_level=F,sanity=T,verbose=T,n_pcs=5){
##  determine which variables (cov_mat) affect expression (dat_mat)
##  dat_mat     - rows=genes, cols=samples
##  cov_mat     - rows=samples, cols=variables - numeric or factor (for categorical)
##  1. calculate PCs from dat_mat
##  2. correlate PCs to cov_mat - using lm() since expect categorical as well as continuous variables
##  3. gene_level=F - optional - limma deg based on covariates (1 at a time and all combined)
dat_mat=as.data.frame(dat_mat)
Library('psych')
  dat_class=dat.class(cov_mat)
  if(sum(!dat_class%in%c('factor','numeric'))!=0){stop('expect class(cov_mat) as factor or numeric only, convert character to numeric or factor')}

  if(!sanity){
    order_check=sum(colnames(dat_mat)!=rownames(cov_mat))
    if(order_check!=0){
      warning('\n\tWARNING\t: (colnames(dat_mat)==rownames(cov_mat)) shows : ',order_check,' not in same order','\n')
    }
  }
  if(sanity){
    holder=intersect(colnames(dat_mat),rownames(cov_mat))
    dat_mat=dat_mat[,holder]
    cov_mat=cov_mat[holder,]
     if(verbose){cat('\tcolnames(dat_mat) n=',ncol(dat_mat),' rownames(cov_mat) n=',nrow(cov_mat),' same order',sum(colnames(dat_mat)==rownames(cov_mat)),'\n')}
  }

   if(verbose){cat('\t\tcalculate',n_pcs,'Principal Componenets using dat_mat\n')}

  if(do_plots[1]){hist(dat_mat,breaks=100)}
   
  pcs=pca(dat_mat,n_pcs=n_pcs)$pcs[,1:n_pcs]

  pclmp=matrix(NA,nrow=ncol(cov_mat),ncol=ncol(pcs))
    colnames(pclmp)=colnames(pcs)
#    rownames(pclmp)=c(colnames(cov_mat),'all.cov')
    rownames(pclmp)=colnames(cov_mat)
  pclmr=pclmp

   if(verbose){cat('\t\taccess impact of',ncol(cov_mat),'covariates on Principal Componenets using lm()\n')}
  for(ipcs in colnames(pcs)){
##  univariate
    for(icov in colnames(cov_mat)){
      # lprogr(icov,colnames(cov_mat))
      holder=linear(pcs[,ipcs],cov_mat[,icov])
      pclmp[icov,ipcs]=holder$pval
      pclmr[icov,ipcs]=holder$rsq
      rm(holder)
    }
##  multivariate
      # holder=linear(pcs[,ipcs],cov_mat)
      # pclmp['all.cov',ipcs]=holder$pval
      # pclmr['all.cov',ipcs]=holder$rsq
      # rm(holder)    
  }

  pclmp[pclmp==0]=9.88131291682493e-324
  pcaf=apply(pclmp,2,p.adjust,method='fdr')

  # Heat(pclmp,values='pval',Rowv=T,Colv=F,main='lm(PC~covariate)\n-log10(P-value)',margin=c(5,45),cexrow=1.2,cexcol=1.2)#,lhei=c(0.1,1),lwid=c(0.1,1),margin=c(1,1))
  Heat(pcaf,values='pval',Rowv=T,Colv=F,main='lm(PC~covariate) -log10(FDR)\nFDR for number of covariates only, not PCs',margin=c(5,45),cexrow=1.2,cexcol=1.2)#,lhei=c(0.1,1),lwid=c(0.1,1),margin=c(1,1))
  Heat(pclmr,values='cor',Rowv=T,Colv=F,main='lm(PC~covariate)\nR.squared',margin=c(5,45),cexrow=1.2,cexcol=1.2)

  pairs.panels(cbind(pcs,cov_mat),method='spearman',lm=T)   ##  lm: Plot the linear fit rather than the LOESS smoothed fits
  # pairs.panels(cbind(pcs,cov_mat[,apply(pclmp,1,function(x){sum(x<0.01)>0})]),method='spearman',lm=T)   ##  lm: Plot the linear fit rather than the LOESS smoothed fits
  # pairs.panels(cbind(pcs,cov_mat[,apply(pclmp,1,function(x){sum(x<0.01)>0})]),method='spearman')


  if(!gene_level){
    readme='
    \tpcs\t- principal componenets prcomp(dat_mat)
    \tpcap\t- P values of univiariate lm(PCs~cov_mat) ie 1 pc against 1 covar
    \tpcar\t- R sqared of univiariate lm(PCs~cov_mat) ie 1 pc against 1 covar
    '
    return(list(pcs=pcs,pcap=pclmp,pcar=pclmr,pcaf=pcaf))
  }

  if(gene_level){
    cov_mat=cbind(cov_mat,pcs)
     if(verbose){cat('\t\taccess impact of',ncol(cov_mat),'covariates (including PCs) on ',nrow(dat_mat),'genes using limma\n')}
    
    holder=deg.limma(dat_mat,cov_mat,sanity=F,verbose=T)

    genep=lapply(holder$univar,function(x){x[,'FDR',drop=F]})
    gener=lapply(holder$univar,function(x){x[,'logFC',drop=F]})

    genep=rmerge.list(genep,verbose=F)
    gener=rmerge.list(gener,verbose=F)
    genep[genep==0]=9.88131291682493e-324
    
    if(do_plots[4]){
      par(mfrow=c(2,1))
      for(icov in colnames(genep)){
        if(verbose){cat('\t\t',icov,'\n')}
        pdat=(genep[,icov])
          if(length(unique(pdat))>1){
            hist(pdat,breaks=50,main=paste0('FDR  ',icov),prob=T,xlim=c(0,1))
            rug(pdat)
          # lines(density(pdat), col="darkred", lwd=2)                                  # add a density estimate with defaults
          # lines(density(pdat, adjust=2), lty="dotted", col="darkgreen", lwd=2)      # add another "smoother" density
            n_gene_sig=sum(pdat<0.01)
            pc_gene_sig=frac(sum(pdat<0.01),length(pdat),perc=T)
            mtext(paste0('genes significant FDR<1% : ',n_gene_sig,' (',pc_gene_sig,'%)'),adj=1,side=3)

            pdat= -log10(pdat)
            hist(pdat,breaks=50,main=paste0('-log10(FDR)  ',icov),prob=T)
            mtext(paste0('genes significant FDR<1% : ',n_gene_sig,' (',pc_gene_sig,'%)'),adj=1,side=3)
            rug(pdat)
          # lines(density(pdat), col="darkred", lwd=2)                                # add a density estimate with defaults
          # lines(density(pdat, adjust=2), lty="dotted", col="darkgreen", lwd=2)    # add another "smoother" density
        }
      }
    }


    readme='
    \tpcs\t- principal componenets prcomp(dat_mat)
    \tpcap\t- P values of univiariate lm(PCs~cov_mat) ie 1 pc against 1 covar
    \tpcar\t- R sqared of univiariate lm(PCs~cov_mat) ie 1 pc against 1 covar
    \tpcaf\t- pcap adjusted for multiple testing (FDR)

    \tgenep\t- P values of univiariate limma of all genes(dat_mat) against 1 covar
    \tgener\t- R squared of univiariate limma of all genes(dat_mat) against 1 covar
    '
    return(invisible(list(pcs=pcs,pcap=pclmp,pcar=pclmr,pcaf=pcaf,genep=genep,gener=gener,readme=readme)))
  }
}




gsea.enrich<-function(genlis,rnkdat,dat_descr='',gsea_path="/Data/ks/gsea_lib1/",nperm=10000,min_clust_size=10,max_clust_size=5000,do_plots=F,...){
##  gsea_path           - required - folder containing a sea2-2.2.3.jar file, need permissions to write to folder (gsea required input files made by the function and writes output to file)
##  gsea version        - hardcoded below as (gsea2-2.2.3.jar), if using different version, this needs to be changed
##  PLOTS : do_plots=T  - create a pdf('/path/to/file/plot_file_name.pdf') before the funciton and dev.off() to finish plotting - all enrichment plots will be saved to the .pdf


##  INPUT : genlis      - named list of gene sets to use for enrichment  list(setA=c('gene1','gene2','gene3'))
##  INPUT : rnkdat      - ranked list of genes - autodetect 2 possible input types   1. rows=genes, single column of rank info  2. column1 =genes, column2 = rank info
##  OUTPUT              - the function will produce an R object containg all relevant enrichment informatioin


##  WARNING :           - this function will break if folders with "my_analysis" in the name already exist in gsea_path - to be safe, do not put any files other than gsea2-2.2.3.jar in the working folder, also delete any error logs generated if the function breaks
##  WARNING :           - if the function breaks, remove any files/folders generated in gsea_path/working/  (eg previous input files, error report folders) the latter will conflict with the function trying to read GSEA output files from subsequent runs
##  WARNING :           - gsea2-2.2.3.jar can only run one enrichment at a time, to run multiple instances of the enrichment simultaneously, create additional folders contaiing gsea2-2.2.3.jar (one for each parralel execution)




#	gsea_path="/Users/ks/Dropbox/bin/gsea/"
gsea_path=paste0(gsea_path,'/')   ##  just in case cos the error msg is not helpful otherwise, probably will fail on windows since weird hashes
system(paste0('mkdir ',gsea_path,'/working'))

#	if(dat_descr!=''){dat_descr=paste0(dat_descr,'.')}
####  .gmt file - module 1 per row, name followed by \t genes in module		------------------------------------------
#	write.file(t(c(names(genlis),t(as.data.frame(genlis)))),file=paste0(gsea_path,'/working/',dat_descr,'gsea_enrich.gmt'),row.names=F,col.names=F)
#	write.file(t(c(names(genlis),t(as.data.frame(genlis)))),file=paste0(gsea_path,'/working/gsea_enrich.gmt'),row.names=F,col.names=F)
cat('\twrite genlis.gmt\n')
sink(paste0(gsea_path,'/working/gsea_enrich.gmt'))
  for(ilis in names(genlis)){

   cat(as.vector(paste(c(ilis,ilis,genlis[ilis][[1]]),collapse="\t")),"\n",sep="")
  }
sink()
####  .rnk file - 2 columns "IDs", "P"		------------------------------------------
	if(!ncol(rnkdat)%in%c(1,2)){stop('rnkdat does not have 2 coluns')}
	if(ncol(rnkdat)==2){
		bkg=rnkdat[,1]
		cat('\tncol(rnkdat)==2, assuming rnkdat already in correct format colnames = c("IDs","P"), where P==rank measure\n')
#		write.file(rnkdat,file=paste0(gsea_path,'/working/',dat_descr,'gsea_enrich.rnk'),row.names=F,col.names=T)
    if(sum(grepl('Inf|NA|NaN',rnkdat[,2]))>0){stop('rnkdat contains one of these values: "Inf", "NA", "NaN" ')}
		write.file(rnkdat,file=paste0(gsea_path,'/working/gsea_enrich.rnk'),row.names=F,col.names=T)
	}
	if(ncol(rnkdat)==1){
		cat('\tncol(rnkdat)==1, assuming rnkdat has appropriate rownames and ranks in the column\n')
		rnkdat=as.data.frame(rnkdat)
			colnames(rnkdat)='P'
		rnkdat$IDs=rownames(rnkdat)
		rnkdat=rnkdat[,c('IDs','P')]
		bkg=rownames(rnkdat)
    if(sum(grepl('Inf|NA|NaN',rnkdat[,2]))>0){stop('rnkdat contains one of these values: "Inf", "NA", "NaN" ')}
#		write.file(rnkdat,file=paste0(gsea_path,'/working/',dat_descr,'gsea_enrich.rnk'),row.names=F,col.names=T)
		write.file(rnkdat,file=paste0(gsea_path,'/working/gsea_enrich.rnk'),row.names=F,col.names=T)
	}
####  .chip file - colunms: "Probe Set ID",	"Gene Symbol",	"Gene Title" - same is easiest (background)		------------------------------------------
# build using the bkg list - derived based on the rnk list above
	chipdat=list("Probe Set ID"=bkg,"Gene Symbol"=bkg,"Gene Title"=bkg)
	chipdat=as.data.frame(chipdat)
		colnames(chipdat)=c("Probe Set ID","Gene Symbol","Gene Title")		##  assuming GSEA is actually picky about colnames
#	write.file(chipdat,file=paste0(gsea_path,'/working/',dat_descr,'gsea_enrich.chip'),row.names=F,col.names=T)
	write.file(chipdat,file=paste0(gsea_path,'/working/gsea_enrich.chip'),row.names=F,col.names=T)


  options(scipen=999)	##  prevent R using scientific notation to numbers
##-Xmx5000mm flags the amount of memory available to java. The default is -Xmx512m
##  all hail Aida for working out the full cmd code to run GSEA, i still remember her frustration when she was doing this..
##   -rpt_label adds a label to the out_folder which can make it too long -> mid part of the name becomes ".." - the part required for grep to work ==> hardcoded as blank
gsea_cmd=   paste0("java -cp ",gsea_path,"gsea2-2.2.3.jar -Xmx50g xtools.gsea.GseaPreranked -gmx "
#     ,gsea_path,'/working/',dat_descr,'gsea_enrich.gmt'
      ,gsea_path,'/working/gsea_enrich.gmt'
        ," -collapse false -mode Max_probe -norm meandiv -nperm ",nperm
#       ," -rnk ",gsea_path,'/working/',dat_descr,'gsea_enrich.rnk'
        ," -rnk ",gsea_path,'/working/gsea_enrich.rnk'  
        , " -scoring_scheme classic -rpt_label  -chip ",gsea_path,'/working/gsea_enrich.chip'
        ," -include_only_symbols true -make_sets true -plot_top_x 20 -rnd_seed timestamp -set_max "
        ,max_clust_size, " -set_min ",min_clust_size," -zip_report false -out ",gsea_path, " -gui false"
        )
print(gsea_cmd)
  system(gsea_cmd)


  out_dir_nam=list.files(gsea_path,pattern='my_analysis')
#  out_dir_nam=out_dir_nam[out_dir_nam!='working']	
 resfnam=list.files(paste0(gsea_path,'/',out_dir_nam),pattern='.xls')

  humpty=read.delim(paste0(gsea_path,'/',out_dir_nam,'/',resfnam[grepl('gsea_report_for_na_pos',resfnam)]))
  dumpty=read.delim(paste0(gsea_path,'/',out_dir_nam,'/',resfnam[grepl('gsea_report_for_na_neg',resfnam)]))
  bumpty=read.delim(paste0(gsea_path,'/',out_dir_nam,'/',resfnam[grepl('na_pos_versus_na_neg',resfnam)]))
  fumpty=read.delim(paste0(gsea_path,'/',out_dir_nam,'/',resfnam[grepl('gene_set_sizes',resfnam)]))

resfnam=resfnam[!grepl('gsea_report_for_na_pos|gsea_report_for_na_neg|na_pos_versus_na_neg|gene_set_sizes',resfnam)]

resfnam=gsub('[.]xls','',resfnam)
sidat=list()
	for(isig in resfnam){
	  sidat[[isig]]=read.delim(paste0(gsea_path,'/',out_dir_nam,'/',isig,'.xls'))
	}

#pdf(paste0(gsea_path,'img/dummy.pdf'),height=5,width=10)		##  for testing purposes, to save plots, more efficient to create a pdf() before running the function & dev.off() after
  if(do_plots){
  	for(isig in names(sidat)){
  		holder=sidat[isig][[1]]

  		ylimdat=c(min(holder$RUNNING.ES)-0.1,(max(holder$RUNNING.ES)))
		plot(x=holder$RANK.IN.GENE.LIST,y=holder$RUNNING.ES,type='l',lwd=4,col='darkgreen',frame.plot=F,ylim=ylimdat,main=paste0(isig,'\n',dat_descr),las=1,xlab='rank in gene list',ylab='GSEA enrichment score',...) #ylim=ylimdat
		rug(x=holder$RANK.IN.GENE.LIST, ticksize = 0.1, side = 1, lwd = 0.5, col = par("fg"),quiet = getOption("warn") < 0)

		mtext(paste0('FDR pos  ',humpty[humpty$NAME==isig,]$FDR.q.val,'   FDR neg   ',dumpty[dumpty$NAME==isig,]$FDR.q.val),adj=1,side=1,line=4)
	}
  }
#dev.off()

	cat('\tclean-up - removing ',paste0('rm ',gsea_path,'/',out_dir_nam),'directory generated by GSEA\n')
	system(paste0('rm ',gsea_path,'/',out_dir_nam,'/*'))
	system(paste0('rm ',gsea_path,'/',out_dir_nam,'/edb/*'))
	system(paste0('rmdir ',gsea_path,'/',out_dir_nam,'/edb'))
	system(paste0('rmdir ',gsea_path,'/',out_dir_nam))
	cat('\tclean-up - removing ',paste0('rm ',gsea_path,'/working *'),'input files for GSEA\n')
	system(paste0('rm ',gsea_path,'/working/*'))
	return(invisible(list(pos=humpty,neg=dumpty,bkg=bumpty,set_size=fumpty,sigdat=sidat))) #genlis=holder,

}



wgcna.beta<-function(expr_mat,corFn='cor',networkType='signed hybrid',corOptions='spearman',betas=c(seq(1,10,by=1),seq(12,20,by=2)),...){
 cat('\tNOTE\t: use - apply WGCNA pickSoftThreshold() to matrix: samples=columns, genes=rows\n\n')
if(corFn=='cor'){cat('\t\t correlation function: ',corFn,' - ',corOptions,', network type: ',networkType,'\n\n',sep='')}
if(corFn=='bicor'){cat('\t\t correlation function',corFn,'network type:',networkType,'\n\n')}

  library('WGCNA')
    allowWGCNAThreads()
    set.seed(0)       # reproducibility 

  thChoice=pickSoftThreshold(t(expr_mat),powerVector=betas,...)
  cat('\n\tpower sugggested by WGCNA:',thChoice$power,'\n\n',verbose=1)

  return(invisible(thChoice))
}


wgcna.mods<-function(expr_mat,corType='bicor',power=7,signType='signed hybrid',mergeCutHei=0.15,dat_descr='',...){
 cat('\tUSE:\tperform basic WGCNA clustering analysis for a given matrix, return full outputs as list\n')
  library(WGCNA)
  allowWGCNAThreads()
  t1=Sys.time()

  minModSize=min(40, nrow(expr_mat)/2 )
# mergeCutHei=0.15

     modules=blockwiseModules(t(expr_mat),   # Input data, expect: samples - rows, genes - columns
      # Data checking options ----------------------------------------------
         checkMissingData=TRUE,
       
      # Options for splitting data into blocks ----------------------------------------------
         blocks=NULL,
         maxBlockSize=30000,
         blockSizePenaltyPower=5,
         randomSeed=12345,
       
      # if load TOM from previously saved file  ----------------------------------------------
      loadTOM=FALSE,
       
      # Network construction arguments: correlation options ----------------------------------------------
         corType=corType,            # pearson - default
         maxPOutliers=1, 
         quickCor=0,
         pearsonFallback="individual",
         cosineCorrelation=FALSE,
       
      # Adjacency function options ----------------------------------------------
         power=power,
         networkType=signType,      # network type. Allowed values are (unique abbreviations of) ‘"unsigned"’, ‘"signed"’, ‘"signed hybrid"’. See ‘adjacency’.
       
      # Topological overlap options ----------------------------------------------
         # TOMType='signed',
         # TOMDenom="min",
       
      # Saving or returning TOM ----------------------------------------------
         getTOMs=NULL,
         saveTOMs=FALSE, 
         saveTOMFileBase="blockwiseTOM",
   
    # Basic tree cut options ========================================================================
         deepSplit=2,                # default=2 simplified version of tree-cutting  (may be worth looking at tree first and defining proper threshold)
         detectCutHeight=0.995,    # dendrogram cut height for module detection. See cutreeDynamic for more details
         minModuleSize=minModSize,   # minimum module size for module detection. See cutreeDynamic for more details
        
      # Advanced tree cut options----------------------------------------------
         maxCoreScatter=NULL,         # maximum scatter of the core for a branch to be a cluster, given as the fraction of cutHeight relative to the 5th   percentile of joining heights. See cutreeDynamic for more details
         minGap=NULL,
         maxAbsCoreScatter=NULL, minAbsGap=NULL,
         minSplitHeight=NULL, minAbsSplitHeight=NULL,
       
         useBranchEigennodeDissim=FALSE,
         minBranchEigennodeDissim=mergeCutHei,
       
         stabilityLabels=NULL,
         minStabilityDissim=NULL,
       
         pamStage=TRUE, pamRespectsDendro=TRUE,
       
      # Gene reassignment, module trimming, and module "significance" criteria ----------------------------------------------
         reassignThreshold=1e-6,
         minCoreKME=0.5, 
#        minCoreKMESize=minModuleSize/3,    minModuleSize not found error..?
         minKMEtoStay=0.3,
       
      # Module merging options ----------------------------------------------
         mergeCutHeight=mergeCutHei, 
         impute=TRUE, 
         trapErrors=FALSE, 
       
      # Output options ----------------------------------------------
         numericLabels=FALSE,
       
      # Options controlling behaviour ----------------------------------------------
         nThreads=0,
         verbose=1,      # options '0', '1' , '2' , '3'
         indent=0,...)

  collectGarbage()

    print(Sys.time()-t1)


## generating mstat is a bit messy..
    mstat=as.data.frame(table(modules$colors))
    mstat=mstat[order(mstat[,2],decreasing=T),]
    msta0=mstat[(mstat[,1]=='grey'),]
    msta0$module='M0'
    mstat=mstat[!(mstat[,1]=='grey'),]
    mstat$module=paste0('M',1:nrow(mstat))
    mstat=rbind(mstat,msta0)
      colnames(mstat)=c('color','ngenes','module')
    mstat$color=as.character(mstat$color)
#    mstat$module=paste(mstat$module,mstat$color,sep="_")   ##  module name contains color
if(dat_descr!=''){mstat$module=paste(mstat$module,dat_descr,sep="_")}   ##  add info after module name

  mes=modules$MEs
    rownames(mes)=colnames(expr_mat)
    colnames(mes)=gsub('ME','',colnames(mes))
  mes=mes[,mstat$color]


  module_list=list()
  module_expr=list()
  for(imod in 1:nrow(mstat)){
    module_expr[[mstat$module[imod]]]=expr_mat[modules$colors==mstat$color[imod],]
    module_list[[mstat$module[imod]]]=rownames(module_expr[[mstat$module[imod]]])

  }

    mbg=as.data.frame('bkgrnd')
    mbg$length=nrow(expr_mat)
    mbg$name='bkgrnd'

      colnames(mbg)=colnames(mstat)
  mstat=rbind(mstat,mbg)


    module_expr[['bkgrnd']]=expr_mat
    module_list[['bkgrnd']]=rownames(expr_mat)


  readme=paste0('\n\tModules are named based on size M1 - biggest, M0 - unclustered, bkgrnd - all input genes, output contains :
    \t1.\tmodule_list - list containing names of genes in each module
    \t2.\tmodule_expr - expression matrix of all genes in module
    \t3.\tMEs         - module eigengenes (PC1) for each module
    \t4.\tmstat       - key used to name modules, includes module size
    \t5.\twgcna_out   - object containing full output of WGNCA blockwiseModules()
    \t6.\tsettings    - power=',power,' corType=',corType,' signType=',signType,'    \t7.\tdataset     - ',dat_descr,'
    \n')
  cat(readme)
  return(invisible(list(module_list=module_list,module_expr=module_expr,MEs=mes,mstat=mstat,wgcna_out=modules,readme=readme)))
}



wgcna.consensus<-function(list_expr,dat_descr='',corType='spearman',power=6,signType="signed",max_block_n=20000,...){
 cat("\tNOTE :\tInput data (list_expr) is expected as a list with each entry : rows=genes, columns=samples\n")
# cat("\tWARNING :\tthe checkMissingData option for WGCNA is disabled, set checkMissingData=T in the code to make robust to missing, alternatively use sd.check or is.missing to determine/remove non-varying or missing values\n")
  library(WGCNA)
  allowWGCNAThreads()

#  minModSize=min(40, nrow(expr_mat)/2 )

  for(ilis in 1:length(list_expr)){
    list_expr[[names(list_expr)[ilis]]]=list(data=t(list_expr[[names(list_expr)[ilis]]]))
  }

  t0=Sys.time()
  
  modules=blockwiseConsensusModules(
  list_expr          ##  lists with $data in each set containing expression: rows=samples, cols=genes

# Data checking options ------------------------------------------
  ,checkMissingData=T     ##  checks for missing and zero variance in expression.. 
     
# Blocking options ------------------------------------------
  ,blocks=NULL
  ,maxBlockSize=max_block_n        ##  ensure no separation is performed // if outdated machine ~4GB RAM, may need to change this to ~5,000, or better yet, consider upgrading
  ,blockSizePenaltyPower=5
    ,randomSeed=12345
     
# TOM precalculation arguments, if available ------------------------------------------
  ,individualTOMInfo=NULL         ##  pre-calculated TOM using blockwiseIndividualTOMs
     ,useIndivTOMSubset=NULL
     
# Network construction arguments: correlation options ------------------------------------------
# ,corType="pearson"
     ,maxPOutliers=1        ##  used only with bicor: Specifies the maximum percentile of data that can be considered outliers on either side of the median separately
     ,quickCor=0            ##  handling of missing
     ,pearsonFallback="individual"    ## using pearson when mean absolute deviation == zero -> cant perform bicor...
  ,cosineCorrelation=FALSE
     
# Adjacency function options ------------------------------------------
# ,power=6                  ## moved to function inputs
  ,networkType=signType   ## moved to function inputs  ##  options: "unsigned"’, ‘"signed"’, ‘"signed hybrid" See ‘adjacency’
     ,checkPower=TRUE
# Topological overlap options ------------------------------------------
  ,TOMType=signType      ## moved to function inputs   ##  options: "none"’, ‘"unsigned"’, ‘"signed"’. If ‘"none"’, adjacency will be used for clustering
     ,TOMDenom="min"          ## min - standard, mean - expreimental

# Save individual TOMs?  ------------------------------------------
  ,saveIndividualTOMs=F    ## TOM saved to disk
     ,individualTOMFileNames="individualTOM-Set%s-Block%b.RData"
     
# Consensus calculation options: network calibration ------------------------------------------
  ,networkCalibration=c("single quantile","full quantile","none")     ## "single", "quantile", "full quantile", "none"
     
# Simple quantile calibration options ------------------------------------------
  ,calibrationQuantile=0.95
     ,sampleForCalibration=TRUE
     ,sampleForCalibrationFactor=1000
     ,getNetworkCalibrationSamples=FALSE
     
# Consensus definition ------------------------------------------
  ,consensusQuantile=0       ## <<<<<<<<<<<<<<<<
     ,useMean=FALSE             ## <<<<<<<<<<<<<<<< use mean instead of consensusQuantile
     ,setWeights=NULL           ## <<<<<<<<<<<<<<<< for weighted mean
     
# Saving the consensus TOM ------------------------------------------
  ,saveConsensusTOMs=FALSE
     ,consensusTOMFileNames="consensusTOM-block.%b.RData"
     
# Internal handling of TOMs ------------------------------------------
  ,useDiskCache=F         ## TRUE=slower but more RAM efficient
     ,chunkSize=NULL
     ,cacheBase=".blockConsModsCache"
     ,cacheDir="."
     
# Alternative consensus TOM input from a previous calculation  ------------------------------------------
  ,consensusTOMInfo=NULL
     
# Basic tree cut options  ------------------------------------------
  ,deepSplit=2
  ,detectCutHeight=0.995
     ,minModuleSize=50           ##  default=20
     ,checkMinModuleSize=TRUE
     
# Advanced tree cut options ------------------------------------------
  ,maxCoreScatter=NULL
     ,minGap=NULL
     ,maxAbsCoreScatter=NULL
     ,minAbsGap=NULL
     ,minSplitHeight=NULL
     ,minAbsSplitHeight=NULL
     ,useBranchEigennodeDissim=FALSE
     ,minBranchEigennodeDissim=mergeCutHeight
     ,stabilityLabels=NULL
     ,minStabilityDissim=NULL
  ,pamStage=TRUE
     ,pamRespectsDendro=TRUE

# Gene reassignment and trimming from a module, and module "significance" criteria ------------------------------------------
  ,reassignThresholdPS=1e-4
#     ,trimmingConsensusQuantile=consensusQuantile    ##  breaks if specified here without outside variables
     ,minCoreKME=0.5
#     ,minCoreKMESize=minModuleSize/3
     ,minKMEtoStay=0.2
     
# Module eigengene calculation options ------------------------------------------
  ,impute=TRUE
     ,trapErrors=FALSE
     
  #Module merging options
  ,equalizeQuantilesForModuleMerging=FALSE
     ,quantileSummaryForModuleMerging="mean"
     ,mergeCutHeight=0.15
#  ,mergeConsensusQuantile=consensusQuantile    ##  breaks if specified here without outside variables
     
# Output options ------------------------------------------
  ,numericLabels=FALSE
# General options ------------------------------------------
  ,nThreads=0
     ,verbose=2
     ,indent=1
     ,...
     )

     print(Sys.time()-t0)
     collectGarbage()


    mstat=as.data.frame(table(modules$colors))
    mstat=mstat[order(mstat[,2],decreasing=T),]
    msta0=mstat[(mstat[,1]=='grey'),]
    msta0$module='M0'
    mstat=mstat[!(mstat[,1]=='grey'),]
    mstat$module=paste0('M',1:nrow(mstat))
    mstat=rbind(mstat,msta0)
      colnames(mstat)=c('color','ngenes','module')
    mstat$color=as.character(mstat$color)

if(dat_descr!=''){mstat$module=paste(mstat$module,dat_descr,sep="_")}   ##  add info after module name

## module membership ------------------------------------------
  module_list=list()
    for(imod in 1:nrow(mstat)){
        module_list[[mstat$module[imod]]]=colnames(list_expr[[1]]$data)[modules$color==mstat$color[imod]]        ##  assuming all module gene names are in the same order, if not, modules are probably unreliable anyhow
    }
    module_list[['bkgrnd']]=colnames(list_expr[[1]]$data)


  mes=modules$multiMEs                              ##  comes as a list, one for each list input
      names(mes)=names(list_expr)

  module_expr=list()
  for(ilis in 1:length(list_expr)){

##  module eigene output ------------------------------------------
    rownames(mes[[names(list_expr)[ilis]]]$data)=rownames(list_expr[[names(list_expr)[ilis]]]$data)
    dummy=mes[[names(list_expr)[ilis]]]$data        ## § get rid of the $data sub-tag in every list..
    mes[[names(list_expr)[ilis]]]=dummy

##  re-name MEs to match module names
        colnames(mes[[names(list_expr)[ilis]]])=gsub('ME','',colnames(mes[[names(list_expr)[ilis]]]))
    mes[[names(list_expr)[ilis]]]=mes[[names(list_expr)[ilis]]][,mstat$color]
        colnames(mes[[names(list_expr)[ilis]]])=paste('me',mstat$module,sep='') # change the names to meM1_descr

## module expression ------------------------------------------
    for(imod in 1:length(module_list)){
        module_expr[[names(list_expr)[ilis]]][[names(module_list)[imod]]]=list_expr[[names(list_expr)[ilis]]]$data[,module_list[[names(module_list)[imod]]]]
    }
  }


## add bkgrnd info to mstat
    mbg=as.data.frame('bkgrnd')
    mbg$length=ncol(list_expr[[1]]$data)
    mbg$name='bkgrnd'

      colnames(mbg)=colnames(mstat)
  mstat=rbind(mstat,mbg)


  readme=paste0('\n\tModules are named based on size M1 - biggest, M0 - unclustered, bkgrnd - all input genes, output contains :
    \t1.\tmodule_list - list containing names of genes in each module
    \t2.\tmodule_expr - expression matrix of all genes in module / input dataset
    \t3.\tMEs         - module eigengenes (PC1) for each module / dataset
    \t4.\tmstat       - key used to name modules, includes module size
    \t5.\twgcna_out   - object containing full output of WGNCA blockwiseConsensusModules(), can be used for plots etc
    \t6.\tsettings    - power=',power,' corType=',corType,' signType=',signType,' 
    \t7.\tdataset     - ',dat_descr,'   ',paste(names(list_expr),collapse=", "),'
    \n')
  cat(readme)
  return(invisible(list(module_list=module_list,module_expr=module_expr,MEs=mes,mstat=mstat,wgcna_out=modules,readme=readme)))
}



# wgcna.clust<-function(dat_lis,power=7,kmm=T,pamk=F,cor.method='spearman',dat_descr='',km_path=getwd(),minModSize=50,mergehei=0.15){
#   sinad=list()
#   # adjac=list()
#   multiExpr=list()
#   Library('WGCNA')
#   Library('km2gcn')   ## https://github.com/juanbot/km2gcn  ## https://bmcsystbiol.biomedcentral.com/articles/10.1186/s12918-017-0420-6
#   Library('fpc')

# if(!is.list(dat_lis)){stop('make sure input is a list - ie even if a single data matrix - required input format: dat_lis=list(dat_name=dat_mat)')}

# ##  calc adjacency - ie spearman correl. not using the weird transformation from adjacency function (1+cor_mat)/2
# cat('\tcalculate adjacency matrix using "spearman" correlation method\n')
#   for(ireg in names(dat_lis)){
#      lprogr(ireg,names(dat_lis))
#     multiExpr[[ireg]]$data=t(dat_lis[[ireg]])
# #    if(adjcal=='man'){
#       sinad[[ireg]]=cor(t(dat_lis[[ireg]]), method = cor.method)                  ##                                                        <<<<•••••••••••>>>>
#     # }
#     # if(adjcal!='man'){
#       sinad[[ireg]]=adjacency.fromSimilarity(sinad[[ireg]],type = 'signed hybrid',power=power)   ##  adjacency.fromSimilarity -> same TOM as using ((1+cormat)/2)^power
#     # }
#     # adjac[[ireg]]=adjacency(sinad[[ireg]],type = net.type,power=power,corFnc = "I", corOptions = "", distFnc = "I", distOptions = "")             ##  WGCNA function to calculate 'correct'
#     # adjac[[ireg]]=adjacency.fromSimilarity(sinad[[ireg]],type = net.type,power=power)   ##  the above adjacency options come from inspecting this function code

#   }

# ## 2.a.3 Calculation of Topological Overlap

# cat('\tcalculate Topological Overlap\n')
#   sintom=list()
#   for(ireg in names(dat_lis)){
#      lprogr(ireg,names(dat_lis))
#     # sintom[[ireg]]=TOMsimilarity(((1+sinad[[ireg]])/2)^power,TOMType = 'signed')    ##  for cor(t(dat_lis[[ireg]]), method = cor.method)    <<<<•••••••••••>>>>
#    # sintom[[ireg]]=TOMsimilarity(abs(sinad[[ireg]])^power,TOMType = 'unsigned')
#       sintom[[ireg]]=TOMsimilarity((sinad[[ireg]]),TOMType = 'signed')
#     gc()
#   }

# if(length(sintom)==1){
#   cat('\tstandard WGCNA\n')
#   sinconstom=sintom[[1]]
# }
# if(length(sintom)>1){
#   cat('\tconsensus WGCNA\n')
#   sinconstom=do.call(pmin,sintom)
# }
#     rownames(sinconstom)=rownames(sintom[[1]])
#     colnames(sinconstom)=colnames(sintom[[1]])


# ## 2.a.6 Clustering and module identification
# ## We use the consensus TOM as input to hierarchical clustering, and identify modules in the resulting dendrogram using the Dynamic Tree Cut algorithm [1]
# cat('\thierarchical clustering using "average"\n')
#   sinconsTree = hclust(as.dist(1-sinconstom), method = "average")

# ## Module identification using dynamic tree cut:
#   sinclust=cutreeDynamic(
#       dendro=sinconsTree
#       ,distM=(1-sinconstom)
#       ,deepSplit=2
#       # ,cutHeight=0.995
#       ,minClusterSize=minModSize
#       ,pamRespectsDendro = FALSE
#       )

# ##  unmerged module gene lists
#   sinmod=list()
#   for(imod in unique(sinclust)){
#     sinmod[[paste0('s',imod)]]=rownames(dat_lis[[1]])[sinclust==imod]
#   }

# cat('\tmerge modules with very similar module eigengenes (MEs), mergehei=',mergehei,'\n')
# merge=mergeCloseModules(multiExpr, sinclust, cutHeight=mergehei, verbose=3)
# ##  merged module gene lists
#   mermod=list()
#   for(imod in unique(merge$colors)){
#     mermod[[paste0(dat_descr,imod)]]=rownames(dat_lis[[1]])[merge$colors==imod]
#   }

# holder=list(moduleColors=sinclust,MEs=merge$oldMEs)

# net=''
# kmd=''
# if(length(sintom)==1){
#     cat('\tstandard WGCNA : apply k-means clustering (alternative module assignment to "cutreeDynamic")\n')
#   if(kmm){
#     net = applykM2WGCNA(net.label=dat_descr, net.file=holder, expr.data=t(dat_lis[[1]]), job.path=km_path, meg=0,beta = power,tom.matrix=sintom[[1]],plot.evolution=F,plot.go=F)

#     # kmme=net$MEs

#     dummy=net$moduleLabels
#     knam= Table(dummy)
#     knam$mnam=paste0(ireg,'.',knam$count)
#    dupnam=knam$mnam[duplicated(knam$mnam)]
#     for(idup in dupnam){
#       knam$mnam[knam$mnam%in%idup]=paste0(knam$mnam[knam$mnam%in%idup],'_',1:length(knam$mnam[knam$mnam%in%idup]))
#     }

#     kmod=list()
#     for(imod in knam$entry){
#       kmod[[knam$mnam[knam$entry==imod]]]=names(dummy[dummy==imod])
#     }

#   }

#  if(pamk){kmd=pamk(sintom[[1]],k=5:40,diss=T)}
# }


# modlen=unlist(lapply(kmod,length))


#   # nosmod=list()
#   # for(imod in unique(nosclust)){
#   #   nosmod[[paste0('n',imod)]]=rownames(dat_lis[[1]])[nosclust==imod]
#   # }


#   # pdf('/Data/ks/bspan/out/img/net_overlap.consensus_signed_unsigned.pdf',height=15,width=15)
#   # dummy=net.cons(sinmod,nosmod,do_plots=T,p_thresh=0.9,main="Maximum % of overlap with p hyper > 0.9")
#   # Heat(make.numeric(dummy$pc_max),values='cor',values.cex=0.5)
#   # dev.off()


# # kmod

#   return((list(kmod=kmod,mlis=mermod)))   #kmm=net,
#   # return((list(mlis=mermod,kmod=kmod,mes=merge$newMEs,unmergedmlis=sinmod,merge=merge,constom=sinconstom,tom=sintom,adj=sinad,cols=sinclust,kmm=net,pamk=kmd)))
#   # return((list(mlis=mermod,mes=merge$newMEs,unmergedmlis=sinmod,merge=merge,constom=sinconstom,tom=sintom,adj=sinad,cols=sinclust)))
# }


wgcna.clust<-function(dat_lis,pow_lis,dat_descr='M',kmm=T,cor.method='spearman',minModSize=40,mergehei=0.15,kmm_path=getwd()){
  sinad=list()
  # adjac=list()
  multiExpr=list()
  Library('WGCNA')
  Library('km2gcn')   ## https://github.com/juanbot/km2gcn  ## https://bmcsystbiol.biomedcentral.com/articles/10.1186/s12918-017-0420-6
  Library('fpc')

if(!is.list(dat_lis)){stop('make sure input is a list - ie even if a single data matrix - required input format: dat_lis=list(dat_name=dat_mat)')}
if(length(dat_lis)>1 & kmm){
  warning('cannot use kmm for consensus modules (expression matrix required), using kmm=F')
  kmm=F
}
if(dat_descr=='M'){warning('dat_descr is used for module naming, consider a more informative prefix than default=="M"')}

####============================================================================================================
##  1.  calc adjacency - ie spearman correl. not using the weird transformation from adjacency function (1+cor_mat)/2
cat('\n\tcalculate "signed hybrid" adjacency matrix using ',cor.method,' correlation method\n')
  for(ireg in names(dat_lis)){
     lprogr(ireg,names(dat_lis))
    multiExpr[[ireg]]$data=t(dat_lis[[ireg]])
#    if(adjcal=='man'){
      sinad[[ireg]]=cor(t(dat_lis[[ireg]]), method = cor.method)                  ##                                                        <<<<•••••••••••>>>>
    # }
    # if(adjcal!='man'){
      sinad[[ireg]]=adjacency.fromSimilarity(sinad[[ireg]],type = 'signed hybrid',power=pow_lis[[ireg]])   ##  adjacency.fromSimilarity -> same TOM as using ((1+cormat)/2)^power
    # }
    # adjac[[ireg]]=adjacency(sinad[[ireg]],type = net.type,power=power,corFnc = "I", corOptions = "", distFnc = "I", distOptions = "")             ##  WGCNA function to calculate 'correct'
    # adjac[[ireg]]=adjacency.fromSimilarity(sinad[[ireg]],type = net.type,power=power)   ##  the above adjacency options come from inspecting this function code

  }

####============================================================================================================
## 2.  Calculation of Topological Overlap
cat('\tcalculate Topological Overlap - signed\n')
  sintom=list()
  for(ireg in names(dat_lis)){
     lprogr(ireg,names(dat_lis))
    # sintom[[ireg]]=TOMsimilarity(((1+sinad[[ireg]])/2)^power,TOMType = 'signed')    ##  for cor(t(dat_lis[[ireg]]), method = cor.method)    <<<<•••••••••••>>>>
   # sintom[[ireg]]=TOMsimilarity(abs(sinad[[ireg]])^power,TOMType = 'unsigned')
      sintom[[ireg]]=TOMsimilarity((sinad[[ireg]]),TOMType = 'signed')
    gc()
  }

if(length(sintom)==1){
  cat('\tstandard WGCNA\n')
  sinconstom=sintom[[1]]
}
if(length(sintom)>1){
  cat('\tconsensus WGCNA\n')
  sinconstom=do.call(pmin,sintom)
}
    rownames(sinconstom)=rownames(sintom[[1]])
    colnames(sinconstom)=colnames(sintom[[1]])

####============================================================================================================
## 3. Clustering and module identification
## We use the consensus TOM as input to hierarchical clustering, and identify modules in the resulting dendrogram using the Dynamic Tree Cut algorithm [1]
cat('\thierarchical clustering using "average"\n')
  sinconsTree = hclust(as.dist(1-sinconstom), method = "average")

## Module identification using dynamic tree cut:
  sinclust=cutreeDynamic(
      dendro=sinconsTree
      ,distM=(1-sinconstom)
      ,deepSplit=2
      # ,cutHeight=0.995
      ,minClusterSize=minModSize
      ,pamRespectsDendro = FALSE
      )

##  unmerged module gene lists
  sinmod=list()
  for(imod in unique(sinclust)){
    sinmod[[paste0('s',imod)]]=rownames(dat_lis[[1]])[sinclust==imod]
  }

####============================================================================================================
## 4. Merge modules based on ME
cat('\tmerge modules with very similar module eigengenes (MEs), mergehei=',mergehei,'\n')
merge=mergeCloseModules(multiExpr, sinclust, cutHeight=mergehei, verbose=3)
##  merged module gene lists
  mermod=list()
  for(imod in unique(merge$colors)){
    mermod[[paste0(dat_descr,imod)]]=rownames(dat_lis[[1]])[merge$colors==imod]
  }

holder=list(moduleColors=sinclust,MEs=merge$oldMEs)

    dummy=mermod
    modlen=as.data.frame(unlist(lapply(dummy,length)))
      modlen$entry=names(dummy)
    modlen$mnam=paste0(dat_descr,'.',modlen[,1])
    dupnam=modlen$mnam[duplicated(modlen$mnam)]
    for(idup in dupnam){
      modlen$mnam[modlen$mnam%in%idup]=paste0(modlen$mnam[modlen$mnam%in%idup],'_',1:length(modlen$mnam[modlen$mnam%in%idup]))
    }

    cmod=list()
    for(imod in modlen$entry){
      cmod[[modlen$mnam[modlen$entry==imod]]]=dummy[[imod]]
    }

####============================================================================================================
## 5. kmm optimisation of module membership based on kmeans
net=''
kmd=''
if(length(sintom)==1){
    cat('\tstandard WGCNA : apply k-means clustering (alternative module assignment to "cutreeDynamic")\n')
  if(kmm){
    net = applykM2WGCNA(net.label=dat_descr, net.file=holder, expr.data=t(dat_lis[[1]]), job.path=kmm_path, meg=0,beta = pow_lis[[1]],tom.matrix=sintom[[1]],plot.evolution=F,plot.go=F)
    # net = applykM2WGCNA(net.label=dat_descr, net.file=holder, expr.data=t(dat_lis[[1]]), job.path=km_path, meg=0,beta = power,tom.matrix=sintom[[1]],plot.evolution=F,plot.go=F)
    # kmme=net$MEs

    dummy=net$moduleLabels
    knam= Table(dummy)
    knam$mnam=paste0(dat_descr,'.',knam$count)
   dupnam=knam$mnam[duplicated(knam$mnam)]
    for(idup in dupnam){
      knam$mnam[knam$mnam%in%idup]=paste0(knam$mnam[knam$mnam%in%idup],'_',1:length(knam$mnam[knam$mnam%in%idup]))
    }

    kmod=list()
    for(imod in knam$entry){
      kmod[[knam$mnam[knam$entry==imod]]]=names(dummy[dummy==imod])
    }

  }

 # if(pamk){kmd=pamk(sintom[[1]],k=5:40,diss=T)}
}

  if(!kmm){return(list(cmod=cmod,cdat=mermod,pow_lis=pow_lis,bkg=rownames(dat_lis[[1]])))}
  if(kmm){return(list(cmod=cmod,kmod=kmod,cdat=mermod,kdat=net,pow_lis=pow_lis,bkg=rownames(dat_lis[[1]])))}
}





cyt.connect<-function(cor_mat,thresh=0.001,use_pcor=T){
#	USE: \t- convert correlation matrix to cytoscape connections
#	NOTE:\t- pcor=T - use Aracne can to calculate partial correlations
#	NOTE:\t- thresh=0.001, pcor=F - use correlation matrix to get connections that pass the R2 threshold
	if(nrow(cor_mat)!=ncol(cor_mat)){
		stop('\n\tERROR: expect a square correlation matrix \n\n')
	}

	if(use_pcor){
		 cat('\tcalculating partial correlations using Aracne PMID: 16723010\n')
		Library('minet')
		cor_mat=aracne(abs(cor_mat))
	}

	for(irow in 1:nrow(cor_mat)){
		humpty=as.data.frame(cor_mat[irow,][cor_mat[irow,]>=thresh])
			colnames(humpty)='pcor'
			humpty$a=rownames(cor_mat)[irow]
			humpty$b=rownames(humpty)
#	cat(nrow(humpty),'\n')
#	print(irow)
		if(irow==1){dumpty=humpty}
		if(irow>1){dumpty=rbind(dumpty,humpty)}

#print(nrow(dumpty))
	}
	dumpty=dumpty[dumpty$a!=dumpty$b,]	##  remove genes interacting with themselves
	rownames(dumpty)=1:nrow(dumpty)
	cat('\t',length(unique(c(dumpty$a,dumpty$b))),'of',nrow(cor_mat),' : ',(length(unique(c(dumpty$a,dumpty$b)))/nrow(cor_mat))*100,'% of genes have connections','\n')

## filtering to remove duplicate connections ie a~b, b~a

	dtb=dumpty
	rm(humpty)
	rm(dumpty)
	## more elegant way to do this is first sort the genes then paste them..........
	  dtb$x=paste(dtb$a,dtb$b)
	  dtb$y=paste(dtb$b,dtb$a)
	  dumpty=list()
	 k=nrow(dtb)
	cat('\tremoving duplicate connectoins\n')
	while(nrow(dtb)>0){
	  humpty=dtb[which(dtb$x==dtb$x[1] | dtb$x==dtb$y[1] | dtb$y==dtb$y[1] | dtb$y==dtb$y[1]), ]
	  dtb=dtb[-which(dtb$x==dtb$x[1] | dtb$x==dtb$y[1] | dtb$y==dtb$y[1] | dtb$y==dtb$y[1]), ]

		dumpty[[paste(sort(unique(c(humpty$a,humpty$b))),collapse='_')]]=t(as.data.frame(list(
		      a=sort(unique(c(humpty$a,humpty$b)))[1]
		      ,b=sort(unique(c(humpty$a,humpty$b)))[2]
		      ,pcor=max(humpty$pcor)
		  )))
		  	cat(round(1-nrow(dtb)/k,digits=2),'\r');flush.console()
	}
	cat('\n')
	return(invisible(as.data.frame(t(as.data.frame(dumpty)))))

	## if partial correl does not give all genes connections, it is feasible to add a single connection to missing genes based on max of correl
} 




webg<-function(genset,genbkg='',quick=F,idtype='genesymbol',organism='hsapiens',enrichtype=c('go','pathw','other'),method='ORA',outdir=getwd(),fdrThr=0.1){
##  organism - currently expects only hsapiens or mmusculus, some things may break otherwise
##  idtype - same for geneset and genbkg, assume that the user is using well mapped & related gene sets
##  WebGestaltR(options) - code currently disregarding non hard-coded choices
##  enrichtype - shorthand for terms is translated into query terms for WebGestalt, alternatively use full names as in WebGestalt. mixing is not supported ie NOT c('go','pathway_Reactome')
##  idtype - typically used: "genesymbol","ensembl_gene_id","entrezgene"

Library('WebGestaltR')
  if(!is.vector(genset)&method=='ORA'){
    stop(paste0('class(genset) is ',class(genset),', expect vector'))
  }

# "pathway_KEGG",
if(sum(enrichtype%in%c('go','pathw','other'))>0){
##  translate base keywords into WebGestalt query terms    --------------------------------------------------------------------------------------------------------------------------------
full=c("geneontology_Biological_Process_noRedundant","geneontology_Cellular_Component_noRedundant","geneontology_Molecular_Function_noRedundant","pathway_Panther","pathway_Reactome","pathway_Wikipathway","network_PPI_BIOGRID","network_miRNA_target","network_Transcription_Factor_target","chromosomalLocation_CytogenicBand","geneontology_Biological_Process","geneontology_Cellular_Component","geneontology_Molecular_Function","network_Kinase_target","disease_Disgenet","disease_GLAD4U","disease_OMIM","drug_DrugBank","drug_GLAD4U","phenotype_Human_Phenotype_Ontology","phenotype_Mammalian_Phenotype_Ontology")
termkey=as.data.frame(full)
termkey$spp=c("any","any","any","any","any","any","any","any","any","any","any","any","any","hsapiens","hsapiens","hsapiens","hsapiens","hsapiens","hsapiens","hsapiens","mmusculus")
termkey$term=c("go","go","go","pathw","pathw","pathw","other","other","other","other","other","other","other","other","other","other","other","other","other","other","other")

if(quick){termkey=termkey[c(1,4,6,16),]}

##  abridged version    --------------------------------------------------------------------------------------------------------------------------------
termkey=termkey[!termkey$full%in%c("chromosomalLocation_CytogenicBand","geneontology_Biological_Process","geneontology_Cellular_Component","geneontology_Molecular_Function","network_Kinase_target","drug_GLAD4U","network_PPI_BIOGRID","network_miRNA_target"),]


full=enrichtype[enrichtype%in%full]
enrichtype=termkey[termkey$term%in%enrichtype & termkey$spp%in%c(organism,'any'),'full']
}

cat('\nterms selected:\n')
print(enrichtype)
##  hardcode currently available species/enrichments to avoid wasting time connecting every time..
  if(method=='ORA'&length(genbkg)==1){
    if(genbkg==''){
      stop('for enrichment method="ORA", provide a background gene set or use method="GSEA"')
    }
  }

  if(method=='GSEA'){
    if(ncol(genset)!=2){
      stop('for enrichment method="GSEA", provide a genset with 2 columns - gene ids, rank metric')
    }
  }


  if(!(organism%in%c("athaliana","btaurus","celegans","cfamiliaris","drerio","sscrofa","dmelanogaster","ggallus","hsapiens","mmusculus","rnorvegicus","scerevisiae"))){
    if(!(organism%in%listOrganism() )){
      stop(paste0('"organism" ',organism,' - not supported by WebGestalt, use "listOrganism()" to find check for correct name'))
    }
  }


  if(organism=='mmusculus'){
    enrichal=c("geneontology_Biological_Process_genesymbol","geneontology_Biological_Process","geneontology_Cellular_Component","geneontology_Molecular_Function","geneontology_Biological_Process_noRedundant","geneontology_Cellular_Component_noRedundant","geneontology_Molecular_Function_noRedundant","pathway_KEGG","pathway_Panther","pathway_Reactome","pathway_Wikipathway","network_miRNA_target","network_PPI_BIOGRID","network_Transcription_Factor_target","phenotype_Mammalian_Phenotype_Ontology","chromosomalLocation_CytogenicBand")
    if(sum(enrichtype%in%enrichal)!=length(enrichtype)){
      enrichal=listGeneSet(organism="mmusculus")
      if(sum(enrichtype%in%enrichal)!=length(enrichtype)){
        stop(paste0('"enrichtype" ',paste(enrichtype[!enrichtype%in%enrichal],collapse=', '),' - not supported by WebGestalt, use "listGeneSet(organism="relevant_species")"'))
      }
    }

    idtypal=c("ABI_550XL_Wildfire","ABI_SOLiD_Exon","ABI_SOLiD_Klaus","ABI_SOLiD","affy_ClariomD-MTA-1_0.na36.mm10","affy_HTMG-430PM_GPL11180","affy_mg_u74a","affy_mg_u74av2","affy_mg_u74b","affy_mg_u74bv2","affy_mg_u74c","affy_mg_u74cv2","affy_microRNA4.0_GPL21572","affy_moe430a","affy_moe430b","affy_moex_1_0_st_v1","affy_mogene_1_0_st_v1","affy_mogene_2_1_st_v1","affy_mouse430_2","affy_mouse430a_2","affy_mu11ksuba","affy_mu11ksubb","agilent_Custom_GPL2510","agilent_sureprint_g3_ge_8x60k","agilent_ToxicogenomicsG4121A_GPL891","agilent_wholegenome_4x44k_v1","agilent_wholegenome_4x44k_v2","codelink","dbSNP","embl","ensembl_gene_id","ensembl_peptide_id","entrezgene_protein-coding","entrezgene","genename","genesymbol","illumina_mouseref_8_v2","illumina_mousewg_6_v1","illumina_mousewg_6_v2","illumina_ScriptSeq_RNASeqV2","illumina_WG6_Customarray","interpro","IonTorrent_RNASeqMature","MGI","Protein_GeneChip34","Protein_GeneChip41","Protein_GeneChip51","protein_id","refseq_mrna","refseq_peptide","unigene","uniprot_swissprot")
    if(!(idtype%in%idtypal)){
      idtypal=listIDType(organism="mmusculus")
      if(!(idtype%in%idtypal)){
        stop(paste0(paste('"idtype" ',idtype[!idtype%in%idtypal],collapse=', '),' - not supported by WebGestalt, use "listIDType(organism="relevant_species")"'))
      }
    }

  }

  if(organism=='hsapiens'){
    enrichal=c("geneontology_Biological_Process_genesymbol","geneontology_Biological_Process","geneontology_Cellular_Component","geneontology_Molecular_Function","geneontology_Biological_Process_noRedundant","geneontology_Cellular_Component_noRedundant","geneontology_Molecular_Function_noRedundant","pathway_KEGG","pathway_Panther","pathway_Reactome","pathway_Wikipathway","network_miRNA_target","network_PPI_BIOGRID","network_Transcription_Factor_target","phenotype_Mammalian_Phenotype_Ontology","chromosomalLocation_CytogenicBand")
    if(sum(enrichtype%in%enrichal)!=length(enrichtype)){
      enrichal=listGeneSet(organism="hsapiens")
      if(sum(enrichtype%in%enrichal)!=length(enrichtype)){
        stop(paste0('"enrichtype" ',paste(enrichtype[!enrichtype%in%enrichal],collapse=', '),' - not supported by WebGestalt, use "listGeneSet(organism="relevant_species")"'))
      }
    }

    idtypal=c("affy_Axiom_BioBank1.na35_probe","affy_Axiom_BioBank1.na35_rsid","affy_Axiom_PMRA.na35_probe","affy_Axiom_PMRA.na35_rsid","affy_Axiom_tx_v1.na35_probe","affy_Axiom_tx_v1.na35_rsid","affy_GenomeWideSNP_5.na35_SNP_probe","affy_GenomeWideSNP_5.na35_SNP_rsid","affy_GenomeWideSNP_6.na35_SNP_probe","affy_GenomeWideSNP_6.na35_SNP_rsid","affy_hc_g110","affy_hg_focus","affy_hg_u133_plus_2","affy_hg_u133a_2","affy_hg_u133a","affy_hg_u133b","affy_hg_u95a","affy_hg_u95av2","affy_hg_u95b","affy_hg_u95c","affy_hg_u95d","affy_hg_u95e","affy_hta_2_0","affy_huex_1_0_st_v2","affy_hugene_1_0_st_v1","affy_hugene_2_0_st_v1","affy_hugenefl","affy_Mapping10K_Xba142.na32_SNP_probe","affy_Mapping10K_Xba142.na32_SNP_rsid","affy_Mapping250K_Nsp.na32_SNP_probe","affy_Mapping250K_Nsp.na32_SNP_rsid","affy_Mapping250K_Sty.na32_SNP_probe","affy_Mapping250K_Sty.na32_SNP_rsid","affy_Mapping50K_Hind240.na32_SNP_probe","affy_Mapping50K_Hind240.na32_SNP_rsid","affy_Mapping50K_Xba240.na32_SNP_probe","affy_Mapping50K_Xba240.na32_SNP_rsid","affy_OncoScan.na33.r1_SNP_probe","affy_OncoScan.na33.r1_SNP_rsid","affy_primeview","affy_RosettaMerck_Human_RSTA","affy_u133_x3p","agilent_cgh_44b","agilent_custom_SAGE_Bionetworks_GPL564","agilent_human_custom_GPL564","agilent_sureprint_g3_ge_8x60k_v2","agilent_sureprint_g3_ge_8x60k","agilent_wholegenome_4x44k_v1","agilent_wholegenome_4x44k_v2","codelink","dbSNP","embl","ensembl_gene_id","ensembl_peptide_id","entrezgene_protein-coding","entrezgene","genename","genesymbol","illumina_human_methylation_27","illumina_human_methylation_450","illumina_human-6v3.0_expression_beadchip","illumina_humanht_12_v3","illumina_humanht_12_v4","illumina_humanref_8_v3","illumina_humanRef-8v2.0_expression_beadchip_GPL6104","illumina_humanwg_6_v1","illumina_humanwg_6_v2","illumina_humanwg_6_v3","illumina_Infinium_HumanMethylation_beadchip","illumina_Sentrix_HumanRef-8v2_GPL2700","interpro","protein_id","refseq_mrna","refseq_peptide","The_Genotype-Tissue_ExpressionProjectGTEx","unigene","uniprot_swissprot")
    if(!(idtype%in%idtypal)){
      idtypal=listIDType(organism="hsapiens")
      if(!(idtype%in%idtypal)){
        stop(paste0(paste('"idtype" ',idtype[!idtype%in%idtypal],collapse=', '),' - not supported by WebGestalt, use "listIDType(organism="relevant_species")"'))
      }
    }

  }

  print(str(genset))
  print(str(genbkg))
  enrichlis=list()
  for(ityp in enrichtype){
    lprogr(ityp,enrichtype,T)
    enrichlis[[ityp]]=as.data.frame('')   ## create blank space
  if(method=='ORA'){
    enrichlis[[ityp]]=WebGestaltR(
      enrichMethod=method                   ##  options=c('ORA','GSEA')
      ,organism=organism                  ##  listOrganism -- to check the available organisms
      ,enrichDatabase=ityp                ##  listGeneSet -- to check the available functional databases for the selected organism
              
      ,interestGene=genset                ##  Users can also use the R object as the input. If ‘enrichMethod’ is ‘ORA’, ‘interestGene’ should be an R ‘vector’ object containing the interesting gene list. If ‘enrichMethod’ is ‘GSEA’, ‘interestGene’ should be an R ‘data.frame’ object containing two columns: the gene list and the corresponding scores.
## df - column 1 - genes, column 2 - rank metric
      ,interestGeneType=idtype              ##  supported ID type of the WebGestaltR for the selected organism can be found by the function ‘listIDType’

      ,referenceGene=genbkg               ##  For ORA method, users can also use the R object as the reference gene list.  ‘referenceGene’ should be an R ‘vector’ object containing the reference gene list.
      ,referenceGeneType=idtype             ##  The ID type of the reference gene list. The supported ID type of the WebGestaltR for the selected organism can be found by the function ‘listIDType’.  If the ‘organism’ is ‘others’, users do not need to set this parameter.

##  commented=='default'    --------------------------------------------------------------------------------------------------------------------------------
      # ,enrichDatabaseFile=NULL                  ##  If users set ‘organism’ as ‘others’ or set ‘enrichDatabase’ as ‘others’, users need to upload a GMT file as the functional categories for the enrichment analysis. The extension of the file should be ‘gmt’ and the first column of the file is the category ID, the second one is the external link for the category. Genes annotated to the category are from the third column.  All columns are separated by tab.
      # ,enrichDatabaseType=NULL                  ##  If users set ‘enrichDatabase’ as ‘others’ ...
      # ,enrichDatabaseDescriptionFile=NULL             ##  Users can also upload a description file for the uploaded ‘enrichDatabaseFile’
      # ,interestGeneFile=NULL                    ##  If ‘enrichMethod’ is ‘ORA’, the extension of the ‘interestGeneFile’ should be ‘txt’ and the file can only contain one column: the interesting gene list. If ‘enrichMethod’ is ‘GSEA’, the extension of the ‘interestGeneFile’ should be ‘rnk’ and the file should contain two columns separated by tab: the gene list and the corresponding scores
      #
      ,collapseMethod="mean"                  ##  The method to collapse the duplicate ids for the GSEA method. ‘mean’, ‘median’, ‘min’ and ‘max’ represent the mean, median, minimum and maximum of scores for the duplicate ids.
      # ,referenceGeneFile=NULL                 ##  For ORA method, the users need to upload the reference gene list. The extension of the ‘referenceGeneFile’ should be ‘txt’ and the file can only contain one column: the reference gene list
      # ,referenceSet=NULL                    ##  GSEA >> ‘listReferenceSet’ -- Users can directly select the reference set from the existing platform in the WebGestaltR and do not need to upload the reference set. All existing platform supported in the WebGestaltR can be found by the function ‘listReferenceSet’.  If ‘referenceGeneFile’ and ‘refereneceGene’ are \ codeNULL, WebGestaltR will use the ‘referenceSet’ as the reference gene set. Otherwise, WebGestaltR will use the user uploaded reference set for the enrichment analysis.
      # ,minNum=10                          ##  WebGestaltR will exclude the categories with the number of annotated genes less than ‘minNum’ for the enrichment analysis. Default=10
      ,maxNum=2000                        ##  WebGestaltR will exclude the categories with the number of annotated genes larger than ‘maxNum’ for the enrichment analysis. The default is ‘500’
      # ,fdrMethod="BH"                   ##  ORA >> FDR methods: ‘holm’, ‘hochberg’, ‘hommel’, ‘bonferroni’, ‘BH’ and ‘BY’. The default is ‘BH’.
      # ,sigMethod="fdr"                  ##  Two significant methods are available in the WebGestaltR: ‘fdr’ and ‘top’. ‘fdr’ means the enriched categories are identified based on the FDR and ‘top’ means all categories are ranked based on FDR and then selected top categories as the enriched categories. The default is ‘fdr’
      ,fdrThr=fdrThr                    ##  The significant level for the ‘fdr’ method. The default is ‘0.05’.
      # ,topThr=10                          ##  The threshold for the ‘top’ method. The default is ‘10’
      # ,dNum=20                          ##  The number of enriched categories visualized in the final report.  The default is ‘20’ and the maximum is ‘100’. A larger ‘dNum’ will increase the running time
      # ,perNum=1000                        ##  The number of permutations for the GSEA method. The default is ‘1000’.
      # ,lNum=20                          ##  The number of categories with the output leading edge genes for the GSEA method.  The default is ‘20’.  ‘Note’: GSEA first ranks the categories based on NES (normalized enrichment score) instead of FDR and then outputs the leading edge genes for top ‘lNum’ categories. Because NES does not necessarily decrease with the increase of the FDR, using ‘sigMethod’ defined in WebGestaltR to identify the significant categories may cause some categories with outputted leading edge genes are not included in the final result even if the number of significant categories is larger than ‘lNum’.
      ,is.output=F                        ##  If ‘is.output’ is TRUE, WebGestaltR will create a folder named by the ‘projectName’ and save the mapping results, GO slim summary, enrichment results and an user-friendly HTML report in the folder. Otherwise, WebGestaltR will only return an R ‘data.frame’ object containing the enrichment results. If hundreds of gene list need to be analyzed simultaneous, it is better to set ‘is.output’ as FALSE.
      ,outputDirectory=outdir                   ##  The output directory for the results
      # ,projectName=NULL                     ##  The name of the project. If ‘projectName’ is Null, WebGestaltR will use time stamp as the project name
      # ,keepGSEAFolder=FALSE                   ##  If ‘keepGSEAFolder’ is TRUE, WebGestaltR will keep all folders generated from GSEA tool that contain all figures and tables related to the GSEA analysis
      # ,methodType="R"                       ##  For the large ID mapping table (e.g. dbSNP mapping table), Users can use ‘R’ or ‘Python’ function to read it.  Sometimes ‘Python’ code is faster than the ‘R’ code.  If users use ‘Python’ code to read the mapping table, users should first install python and the module ‘pandas’ in the computer
      # ,dagColor="binary"                      ##  If ‘dagColor’ is binary, the significant terms in the DAG structure will be colored by red for ORA method or red (positive related) and blue (negative related) for GSEA method. If ‘dagColor’ is continous, the significant terms in the DAG structure will be colored by the red gradient for ORA method or red (positive related) and blue (negative related) gradient for GSEA method.based on the corresponding FDR
      # ,hostName="http://www.webgestalt.org/"            ##  The server URL for accessing the data. User can use ‘listArchiveURL’ function to get all archive version URL
    )
  }
  if(method=='GSEA'){
      enrichlis[[ityp]]=WebGestaltR(
      enrichMethod=method                   ##  options=c('ORA','GSEA')
      ,organism=organism                  ##  listOrganism -- to check the available organisms
      ,enrichDatabase=ityp                ##  listGeneSet -- to check the available functional databases for the selected organism
              
      ,interestGene=genset                ##  Users can also use the R object as the input. If ‘enrichMethod’ is ‘ORA’, ‘interestGene’ should be an R ‘vector’ object containing the interesting gene list. If ‘enrichMethod’ is ‘GSEA’, ‘interestGene’ should be an R ‘data.frame’ object containing two columns: the gene list and the corresponding scores.
## df - column 1 - genes, column 2 - rank metric
      ,interestGeneType=idtype              ##  supported ID type of the WebGestaltR for the selected organism can be found by the function ‘listIDType’

      # ,referenceGene=genbkg               ##  For ORA method, users can also use the R object as the reference gene list.  ‘referenceGene’ should be an R ‘vector’ object containing the reference gene list.
      # ,referenceGeneType=idtype             ##  The ID type of the reference gene list. The supported ID type of the WebGestaltR for the selected organism can be found by the function ‘listIDType’.  If the ‘organism’ is ‘others’, users do not need to set this parameter.

##  commented=='default'    --------------------------------------------------------------------------------------------------------------------------------
      # ,enrichDatabaseFile=NULL                  ##  If users set ‘organism’ as ‘others’ or set ‘enrichDatabase’ as ‘others’, users need to upload a GMT file as the functional categories for the enrichment analysis. The extension of the file should be ‘gmt’ and the first column of the file is the category ID, the second one is the external link for the category. Genes annotated to the category are from the third column.  All columns are separated by tab.
      # ,enrichDatabaseType=NULL                  ##  If users set ‘enrichDatabase’ as ‘others’ ...
      # ,enrichDatabaseDescriptionFile=NULL             ##  Users can also upload a description file for the uploaded ‘enrichDatabaseFile’
      # ,interestGeneFile=NULL                    ##  If ‘enrichMethod’ is ‘ORA’, the extension of the ‘interestGeneFile’ should be ‘txt’ and the file can only contain one column: the interesting gene list. If ‘enrichMethod’ is ‘GSEA’, the extension of the ‘interestGeneFile’ should be ‘rnk’ and the file should contain two columns separated by tab: the gene list and the corresponding scores
      #
      ,collapseMethod="mean"                  ##  The method to collapse the duplicate ids for the GSEA method. ‘mean’, ‘median’, ‘min’ and ‘max’ represent the mean, median, minimum and maximum of scores for the duplicate ids.
      # ,referenceGeneFile=NULL                 ##  For ORA method, the users need to upload the reference gene list. The extension of the ‘referenceGeneFile’ should be ‘txt’ and the file can only contain one column: the reference gene list
      # ,referenceSet=NULL                    ##  GSEA >> ‘listReferenceSet’ -- Users can directly select the reference set from the existing platform in the WebGestaltR and do not need to upload the reference set. All existing platform supported in the WebGestaltR can be found by the function ‘listReferenceSet’.  If ‘referenceGeneFile’ and ‘refereneceGene’ are \ codeNULL, WebGestaltR will use the ‘referenceSet’ as the reference gene set. Otherwise, WebGestaltR will use the user uploaded reference set for the enrichment analysis.
      # ,minNum=10                          ##  WebGestaltR will exclude the categories with the number of annotated genes less than ‘minNum’ for the enrichment analysis. Default=10
      ,maxNum=2000                        ##  WebGestaltR will exclude the categories with the number of annotated genes larger than ‘maxNum’ for the enrichment analysis. The default is ‘500’
      # ,fdrMethod="BH"                   ##  ORA >> FDR methods: ‘holm’, ‘hochberg’, ‘hommel’, ‘bonferroni’, ‘BH’ and ‘BY’. The default is ‘BH’.
      # ,sigMethod="fdr"                  ##  Two significant methods are available in the WebGestaltR: ‘fdr’ and ‘top’. ‘fdr’ means the enriched categories are identified based on the FDR and ‘top’ means all categories are ranked based on FDR and then selected top categories as the enriched categories. The default is ‘fdr’
      ,fdrThr=fdrThr                    ##  The significant level for the ‘fdr’ method. The default is ‘0.05’.
      # ,topThr=10                          ##  The threshold for the ‘top’ method. The default is ‘10’
      # ,dNum=20                          ##  The number of enriched categories visualized in the final report.  The default is ‘20’ and the maximum is ‘100’. A larger ‘dNum’ will increase the running time
      # ,perNum=1000                        ##  The number of permutations for the GSEA method. The default is ‘1000’.
      # ,lNum=20                          ##  The number of categories with the output leading edge genes for the GSEA method.  The default is ‘20’.  ‘Note’: GSEA first ranks the categories based on NES (normalized enrichment score) instead of FDR and then outputs the leading edge genes for top ‘lNum’ categories. Because NES does not necessarily decrease with the increase of the FDR, using ‘sigMethod’ defined in WebGestaltR to identify the significant categories may cause some categories with outputted leading edge genes are not included in the final result even if the number of significant categories is larger than ‘lNum’.
      ,is.output=F                        ##  If ‘is.output’ is TRUE, WebGestaltR will create a folder named by the ‘projectName’ and save the mapping results, GO slim summary, enrichment results and an user-friendly HTML report in the folder. Otherwise, WebGestaltR will only return an R ‘data.frame’ object containing the enrichment results. If hundreds of gene list need to be analyzed simultaneous, it is better to set ‘is.output’ as FALSE.
      ,outputDirectory=outdir                   ##  The output directory for the results
      # ,projectName=NULL                     ##  The name of the project. If ‘projectName’ is Null, WebGestaltR will use time stamp as the project name
      # ,keepGSEAFolder=FALSE                   ##  If ‘keepGSEAFolder’ is TRUE, WebGestaltR will keep all folders generated from GSEA tool that contain all figures and tables related to the GSEA analysis
      # ,methodType="R"                       ##  For the large ID mapping table (e.g. dbSNP mapping table), Users can use ‘R’ or ‘Python’ function to read it.  Sometimes ‘Python’ code is faster than the ‘R’ code.  If users use ‘Python’ code to read the mapping table, users should first install python and the module ‘pandas’ in the computer
      # ,dagColor="binary"                      ##  If ‘dagColor’ is binary, the significant terms in the DAG structure will be colored by red for ORA method or red (positive related) and blue (negative related) for GSEA method. If ‘dagColor’ is continous, the significant terms in the DAG structure will be colored by the red gradient for ORA method or red (positive related) and blue (negative related) gradient for GSEA method.based on the corresponding FDR
      # ,hostName="http://www.webgestalt.org/"            ##  The server URL for accessing the data. User can use ‘listArchiveURL’ function to get all archive version URL
    )
  }

  ##  may need to add 'no enrichment' for used terms that did not get a list when returned from WebGestaltR
  cat('\n')
  }

  return(invisible(enrichlis))
}




webg.plot<-function(webg_list,do_plots=F,zero_replace=1e-20,p_thresh=0.1){
#  warning('NOTE: this function is designed for 2+ modules i.e. list(M1=webg_output,M2=webg_output)')
  if(length(webg_list)>1){
    cat('\tplot heatmap of multiple "modules"\n')
    enricht=unique(unlist(lapply(webg_list,names)))
    enrich_dat=list()
    pdat=list()
    for(iter in enricht){
      lprogr(iter,enricht,T)
      molder=''
      for(imod in names(webg_list)){
        cat('\t\t',imod)

        if(!is.null(webg_list[[imod]][[iter]])){

        if('description'%in%colnames(webg_list[[imod]][[iter]])){
          holder=webg_list[[imod]][[iter]][,'FDR',drop=F]
            colnames(holder)=imod
            # rownames(holder)=webg_list[[imod]][[iter]][,'description']
            rownames(holder)=paste0(webg_list[[imod]][[iter]][,'geneset'],';   ',webg_list[[imod]][[iter]][,'description'])  ##  
##  GSEA - can produce erros like the below
#          geneset                               description
# 139 R-MMU-427413 NoRC negatively regulates rRNA expression
# 211 R-MMU-573389 NoRC negatively regulates rRNA expression
        }
        if(!'description'%in%colnames(webg_list[[imod]][[iter]])&'geneset'%in%colnames(webg_list[[imod]][[iter]])){
          holder=webg_list[[imod]][[iter]][,'FDR',drop=F]
            colnames(holder)=imod
            # rownames(holder)=webg_list[[imod]][[iter]][,'geneset']
        }

        molder=rmerge(molder,holder,verbose=F)
        cat('\t',max(min(holder),spval(holder,value=2,mode='min'),na.rm=T),'\n')    ## extract the lowest p-value in this set
        }

##  when one of the gene lists is very small
        # if(length(webg_list[[imod]][[iter]])==0){
        #   holder=as.data.frame(1)
        #     colnames(holder)=imod
        #   molder=rmerge(molder,holder,verbose=F)
        # }
        if(length(webg_list[[imod]][[iter]])==1){
          if(webg_list[[imod]][[iter]]=="ERROR: All genes in the uploaded gene list are not annotated to any category of the functional database."){ ## breaks if is.null()==T
            holder=as.data.frame(1)
              colnames(holder)=imod
            molder=rmerge(molder,holder,verbose=F)
            cat('\t no enrichment found\n')   ## extract the lowest p-value in this set
          }
        }
        if(is.null(webg_list[[imod]][[iter]])){
          holder=as.data.frame(1)
            colnames(holder)=imod
          molder=rmerge(molder,holder,verbose=F)
          cat('\t no enrichment found\n')   ## extract the lowest p-value in this set
        }
      }
      molder=molder[rownames(molder)!='1',colnames(molder)!='x']
      molder[is.na(molder)]=1       ##  missing == no enrichment => 1
      molder[molder==0]=1e-20       ##  p-value==0 is not viable for plotting => set as "zero_replace"
##  remove any rows where no value passes the p_threshold (ie all are "suggestive" enrichments)
      molder=molder[apply(molder,1,function(x){sum(x<p_thresh)>0}),]
      molder=molder[do.call(order,(lapply(1:NCOL(molder),function(i)molder[,i]))),]
      enrich_dat[[iter]]=make.numeric(molder)
      if(do_plots){
        return(gestaltheat(enrich_dat[[iter]]))   ##  need to add mtext description for enrichment type
      }
    }
  return(enrich_dat)
  }
}










# cmapr<-function(modg,bkg,de_thresh=0.1,n_genes=5,logfc_thresh=0){  ## combine the stuffs below to use with function rather than combined stuff as is atm
# ####===================================================================================
# ####  1. module info  ----------------------------------------------------------------
# ##  lmod$up   - character vector of genes up-regulated, if not directional supply all in either $up or $down
# ##  lmod$down - optional down-regulated module genes
# ####  2. module dataset background  ----------------------------------------------------------------
# ##  bkg           - character vector of all genes used to generate modules
# ####  3. cmap database  ----------------------------------------------------------------
# ##  degdb         - additional input - degdat$drug_name_etc[,c('logFC','FDR')]  ## column names require this format
# ##  logfc_thresh - use as absolute -> automatically *-1 for downreg
# cat('\n\tNOTE: this function REQUIRES properly formatted database named "degdb", cmap version available from :
#     \t\thttps://www.dropbox.com/s/18l1w2jbqld2mej/cmap.enrich.data.Rdata?dl=0\n\n')


# cat('\t\tcommon background correction\n')
# cmap_bkg=bgcommon(degdb)
# bkg=intersect(cmap_bkg,bkg)

# degdb=lapply(degdb,function(x){x[rownames(x)%in%bkg,]})


# modg=lapply(modg,function(x){x[x%in%bkg]})

# # Head(degdb)


# mstat=list()
# sigen=list()
# sumry=list()


# cat('\t\tperform enrichment, genes considered significantly regulated by drug if:
#       \t\t de_thresh=',de_thresh,'
#       \t\t n_genes=',n_genes,'
#       \t\t logfc_thresh=',logfc_thresh,'\n\n
# ')


# k=1
# calncol=0
# # idru=names(degdb)[3]
# for(idru in names(degdb)){
#   holder=list()

#   holder$bkg=(degdb[[idru]])
#   holder$drug_sigup=(holder$bkg[holder$bkg$FDR<=de_thresh & holder$bkg$logFC>=logfc_thresh,])
#   holder$drug_sigdown=(holder$bkg[holder$bkg$FDR<=de_thresh & holder$bkg$logFC<=(logfc_thresh*(-1)),])    ##

#   if(nrow(holder$drug_sigup)>n_genes | nrow(holder$drug_sigdown)>n_genes){

#     if('up'%in%names(modg)){
#       sigen[[idru]]$up=modg$up
#       sigen[[idru]]$up_down=intersect(rownames(holder$drug_sigdown),modg$up)
#       sigen[[idru]]$up_up=intersect(rownames(holder$drug_sigup),modg$up)

#       # mstat[[paste(idru,'up_downreg',sep='.')]]=unlist(fet(sampl=modg$up,bkgrnd=rownames(holder$bkg),success=rownames(holder$drug_sigdown),counts=F,tail='greater'))
#       # mstat[[paste(idru,'up_upreg',sep='.')]]  =unlist(fet(sampl=modg$up,bkgrnd=rownames(holder$bkg),success=rownames(holder$drug_sigup),counts=F,tail='greater'))

#       mstat[[idru]]$up_downreg=unlist(fet(sampl=modg$up,bkgrnd=rownames(holder$bkg),success=rownames(holder$drug_sigdown),counts=F,tail='greater'))
#       mstat[[idru]]$up_upreg  =unlist(fet(sampl=modg$up,bkgrnd=rownames(holder$bkg),success=rownames(holder$drug_sigup),counts=F,tail='greater'))

#     }

#     if('down'%in%names(modg)){
#       sigen[[idru]]$down=modg$down
#       sigen[[idru]]$down_down=intersect(rownames(holder$drug_sigdown),modg$down)
#       sigen[[idru]]$down_up=intersect(rownames(holder$drug_sigup),modg$down)

#       # mstat[[paste(idru,'down_downreg',sep='.')]]=unlist(fet(sampl=modg$down,bkgrnd=rownames(holder$bkg),success=rownames(holder$drug_sigdown),counts=F,tail='greater'))
#       # mstat[[paste(idru,'down_upreg',sep='.')]]  =unlist(fet(sampl=modg$down,bkgrnd=rownames(holder$bkg),success=rownames(holder$drug_sigup),counts=F,tail='greater'))

#       mstat[[idru]]$down_downreg=unlist(fet(sampl=modg$down,bkgrnd=rownames(holder$bkg),success=rownames(holder$drug_sigdown),counts=F,tail='greater'))
#       mstat[[idru]]$down_upreg=unlist(fet(sampl=modg$down,bkgrnd=rownames(holder$bkg),success=rownames(holder$drug_sigup),counts=F,tail='greater'))

#     }

#     if('down'%in%names(modg) & 'up'%in%names(modg)){
#       # nsuccs=mstat[[paste(idru,'down_upreg',sep='.')]][1:2]+mstat[[paste(idru,'up_downreg',sep='.')]][1:2]
#       nsuccs=mstat[[idru]]$down_upreg[1:2]+mstat[[idru]]$up_downreg[1:2]
#       nfails=c(length(unique(unlist(modg)))-nsuccs[1],nrow(holder$bkg)-nsuccs[2])
#       # mstat[[paste(idru,'reverse',sep='.')]]=unlist(fet(samp.success=nsuccs[1], bkgrnd.success=nsuccs[2], samp.fail=nfails[1], bkgrnd.fail=nfails[2], tail = "greater",counts=T))
#       mstat[[idru]]$reverse=unlist(fet(samp.success=nsuccs[1], bkgrnd.success=nsuccs[2], samp.fail=nfails[1], bkgrnd.fail=nfails[2], tail = "greater",counts=T))

#       # nsuccs=mstat[[paste(idru,'down_downreg',sep='.')]][1:2]+mstat[[paste(idru,'up_upreg',sep='.')]][1:2]
#       nsuccs=mstat[[idru]]$down_downreg[1:2]+mstat[[idru]]$up_upreg[1:2]
#       nfails=c(length(unique(unlist(modg)))-nsuccs[1],nrow(holder$bkg)-nsuccs[2])
#       # mstat[[paste(idru,'mimic',sep='.')]]  =unlist(fet(samp.success=nsuccs[1], bkgrnd.success=nsuccs[2], samp.fail=nfails[1], bkgrnd.fail=nfails[2], tail = "greater",counts=T))
#       mstat[[idru]]$mimic=unlist(fet(samp.success=nsuccs[1], bkgrnd.success=nsuccs[2], samp.fail=nfails[1], bkgrnd.fail=nfails[2], tail = "greater",counts=T))


#     }
#   }
#   k=lcount(k,length(degdb))
# }

#   cat('\t\tcompile output\n')
#   rm(holder)
#   k=1
#   mpval=''
#   msens=''
#   mspec=''
#   for(idru in names(mstat)){
#     holder=as.data.frame(t(as.data.frame(mstat[[idru]])))
#     holder$sensitivity=holder$samp.success/(holder$samp.success+holder$samp.fail)
#     holder$specificity=holder$samp.success/(holder$samp.success+holder$bkgrnd.success)

#     dummy=holder[,'FETp',drop=F]
#       colnames(dummy)=idru
#     mpval=rmerge(mpval,dummy,verbose=F)

#     dummy=holder[,'sensitivity',drop=F]
#       colnames(dummy)=idru
#     msens=rmerge(msens,dummy,verbose=F)

#     dummy=holder[,'specificity',drop=F]
#       colnames(dummy)=idru
#     mspec=rmerge(mspec,dummy,verbose=F)

#     k=lcount(k,length(mstat))
#   }

#   mpval=t(mpval[rownames(mpval)!='1',colnames(mpval)!='x'])
#   msens=t(msens[rownames(msens)!='1',colnames(msens)!='x'])
#   mspec=t(mspec[rownames(mspec)!='1',colnames(mspec)!='x'])


#   orcol=c('reverse','up_downreg','down_upreg','mimic','up_upreg','down_downreg')
#   mpval=mpval[,orcol[orcol%in%colnames(mpval)]]
#   msens=msens[,orcol[orcol%in%colnames(msens)]]
#   mspec=mspec[,orcol[orcol%in%colnames(mspec)]]

#   cat('\n\n')

#   return(list(pval=mpval,sensitivity=msens,specificity=mspec,full=mstat))

# }


cmapr<-function(modg,bkg,de_thresh=0.1,n_genes=5,logfc_thresh=0){  ## combine the stuffs below to use with function rather than combined stuff as is atm
####===================================================================================
####  1. module info  ----------------------------------------------------------------
##  lmod$up   - character vector of genes up-regulated, if not directional supply all in either $up or $down
##  lmod$down - optional down-regulated module genes
####  2. module dataset background  ----------------------------------------------------------------
##  bkg           - character vector of all genes used to generate modules
####  3. cmap database  ----------------------------------------------------------------
##  degdb         - additional input - degdat$drug_name_etc[,c('logFC','FDR')]  ## column names require this format
##  logfc_thresh - use as absolute -> automatically *-1 for downreg
cat('\n\tNOTE: this function REQUIRES properly formatted database named "degdb", cmap version available from :
    \t\thttps://www.dropbox.com/s/18l1w2jbqld2mej/cmap.enrich.data.Rdata?dl=0\n\n')


cat('\t\tcommon background correction\n')
cmap_bkg=bgcommon(degdb)
bkg=intersect(cmap_bkg,bkg)

degdb=lapply(degdb,function(x){x[rownames(x)%in%bkg,]})


modg=lapply(modg,function(x){x[x%in%bkg]})

# Head(degdb)


mstat=list()
mlfcw=list()
sigen=list()
sumry=list()


cat('\t\tperform enrichment, genes considered significantly regulated by drug if:
      \t\t de_thresh=',de_thresh,'
      \t\t n_genes=',n_genes,'
      \t\t logfc_thresh=',logfc_thresh,'\n\n
')






k=1
calncol=0
# idru=names(degdb)[3]
for(idru in names(degdb)){
  holder=list()

  holder$bkg=(degdb[[idru]])
  holder$drug_sigup=(holder$bkg[holder$bkg$FDR<=de_thresh & holder$bkg$logFC>=logfc_thresh,])
  holder$drug_sigdown=(holder$bkg[holder$bkg$FDR<=de_thresh & holder$bkg$logFC<=(logfc_thresh*(-1)),])    ##

  if(nrow(holder$drug_sigup)>n_genes | nrow(holder$drug_sigdown)>n_genes){

    if('up'%in%names(modg)){
      sigen[[idru]]$up=holder$bkg[modg$up,]
      sigen[[idru]]$up_down=holder$bkg[intersect(rownames(holder$drug_sigdown),modg$up),]
      sigen[[idru]]$up_up=holder$bkg[intersect(rownames(holder$drug_sigup),modg$up),]

      # mstat[[paste(idru,'up_downreg',sep='.')]]=unlist(fet(sampl=modg$up,bkgrnd=rownames(holder$bkg),success=rownames(holder$drug_sigdown),counts=F,tail='greater'))
      # mstat[[paste(idru,'up_upreg',sep='.')]]  =unlist(fet(sampl=modg$up,bkgrnd=rownames(holder$bkg),success=rownames(holder$drug_sigup),counts=F,tail='greater'))

      mstat[[idru]]$up_downreg=unlist(fet(sampl=rownames(sigen[[idru]]$up),bkgrnd=rownames(holder$bkg),success=rownames(holder$drug_sigdown),counts=F,tail='greater'))
      mstat[[idru]]$up_upreg  =unlist(fet(sampl=rownames(sigen[[idru]]$up),bkgrnd=rownames(holder$bkg),success=rownames(holder$drug_sigup),counts=F,tail='greater'))

      mlfcw[[idru]]$up_downreg=unlist(fet(
                samp.success=round(sum(abs(sigen[[idru]]$up_down)$logFC))
                ,bkgrnd.success=round(sum(abs(holder$drug_sigdown[!rownames(holder$drug_sigdown)%in%rownames(sigen[[idru]]$up_down),]$logFC)))
                ,samp.fail=round(sum(abs(sigen[[idru]]$up[!sigen[[idru]]$up%in%sigen[[idru]]$up_down]$logFC)))
                ,bkgrnd.fail=round(sum(abs(holder$bkg[!rownames(holder$bkg)%in%rownames(holder$drug_sigdown),]$FDR)))
                ,counts=T,tail='greater'))
      mlfcw[[idru]]$up_upreg  =unlist(fet(
                samp.success=round(sum(abs(sigen[[idru]]$up_up)$logFC))
                ,bkgrnd.success=round(sum(abs(holder$drug_sigup[!rownames(holder$drug_sigup)%in%rownames(sigen[[idru]]$up_up),]$logFC)))
                ,samp.fail=round(sum(abs(sigen[[idru]]$up[!sigen[[idru]]$up%in%sigen[[idru]]$up_up]$logFC)))
                ,bkgrnd.fail=round(sum(abs(holder$bkg[!rownames(holder$bkg)%in%rownames(holder$drug_sigup),]$FDR)))
                ,counts=T,tail='greater'))


    }

    if('down'%in%names(modg)){
      sigen[[idru]]$down=holder$bkg[modg$down,]
      sigen[[idru]]$down_down=holder$bkg[intersect(rownames(holder$drug_sigdown),modg$down),]
      sigen[[idru]]$down_up=holder$bkg[intersect(rownames(holder$drug_sigup),modg$down),]

      # mstat[[paste(idru,'down_downreg',sep='.')]]=unlist(fet(sampl=modg$down,bkgrnd=rownames(holder$bkg),success=rownames(holder$drug_sigdown),counts=F,tail='greater'))
      # mstat[[paste(idru,'down_upreg',sep='.')]]  =unlist(fet(sampl=modg$down,bkgrnd=rownames(holder$bkg),success=rownames(holder$drug_sigup),counts=F,tail='greater'))

      mstat[[idru]]$down_downreg=unlist(fet(sampl=rownames(sigen[[idru]]$down),bkgrnd=rownames(holder$bkg),success=rownames(holder$drug_sigdown),counts=F,tail='greater'))
      mstat[[idru]]$down_upreg=unlist(fet(sampl=rownames(sigen[[idru]]$down),bkgrnd=rownames(holder$bkg),success=rownames(holder$drug_sigup),counts=F,tail='greater'))

      mlfcw[[idru]]$down_upreg=unlist(fet(
                samp.success=round(sum(abs(sigen[[idru]]$down_up)$logFC))
                ,bkgrnd.success=round(sum(abs(holder$drug_sigup[!rownames(holder$drug_sigup)%in%rownames(sigen[[idru]]$down_up),]$logFC)))
                ,samp.fail=round(sum(abs(sigen[[idru]]$down[!sigen[[idru]]$down%in%sigen[[idru]]$down_up]$logFC)))
                ,bkgrnd.fail=round(sum(abs(holder$bkg[!rownames(holder$bkg)%in%rownames(holder$drug_sigup),]$FDR)))
                ,counts=T,tail='greater'))

      mlfcw[[idru]]$down_downreg  =unlist(fet(
                samp.success=round(sum(abs(sigen[[idru]]$down_down)$logFC))
                ,bkgrnd.success=round(sum(abs(holder$drug_sigdown[!rownames(holder$drug_sigdown)%in%rownames(sigen[[idru]]$down_down),]$logFC)))
                ,samp.fail=round(sum(abs(sigen[[idru]]$down[!sigen[[idru]]$down%in%sigen[[idru]]$down_down]$logFC)))
                ,bkgrnd.fail=round(sum(abs(holder$bkg[!rownames(holder$bkg)%in%rownames(holder$drug_sigdown),]$FDR)))
                ,counts=T,tail='greater'))

    }

    if('down'%in%names(modg) & 'up'%in%names(modg)){
      # nsuccs=mstat[[paste(idru,'down_upreg',sep='.')]][1:2]+mstat[[paste(idru,'up_downreg',sep='.')]][1:2]
      nsuccs=mstat[[idru]]$down_upreg[1:2]+mstat[[idru]]$up_downreg[1:2]
      nfails=c(length(unique(unlist(modg)))-nsuccs[1],nrow(holder$bkg)-nsuccs[2])
      # mstat[[paste(idru,'reverse',sep='.')]]=unlist(fet(samp.success=nsuccs[1], bkgrnd.success=nsuccs[2], samp.fail=nfails[1], bkgrnd.fail=nfails[2], tail = "greater",counts=T))
      mstat[[idru]]$reverse=unlist(fet(samp.success=nsuccs[1], bkgrnd.success=nsuccs[2], samp.fail=nfails[1], bkgrnd.fail=nfails[2], tail = "greater",counts=T))


      nsuclf=mlfcw[[idru]]$down_upreg[1:2]+mlfcw[[idru]]$up_downreg[1:2]
      nfailf=c(length(unique(unlist(modg)))-nsuclf[1],nrow(holder$bkg)-nsuclf[2])

      mlfcw[[idru]]$reverse=unlist(fet(samp.success=nsuclf[1], bkgrnd.success=nsuclf[2], samp.fail=nfailf[1], bkgrnd.fail=nfailf[2], tail = "greater",counts=T))


      # nsuccs=mstat[[paste(idru,'down_downreg',sep='.')]][1:2]+mstat[[paste(idru,'up_upreg',sep='.')]][1:2]
      nsuccs=mstat[[idru]]$down_downreg[1:2]+mstat[[idru]]$up_upreg[1:2]
      nfails=c(length(unique(unlist(modg)))-nsuccs[1],nrow(holder$bkg)-nsuccs[2])
      # mstat[[paste(idru,'mimic',sep='.')]]  =unlist(fet(samp.success=nsuccs[1], bkgrnd.success=nsuccs[2], samp.fail=nfails[1], bkgrnd.fail=nfails[2], tail = "greater",counts=T))
      mstat[[idru]]$mimic=unlist(fet(samp.success=nsuccs[1], bkgrnd.success=nsuccs[2], samp.fail=nfails[1], bkgrnd.fail=nfails[2], tail = "greater",counts=T))

      nsuclf=mlfcw[[idru]]$down_downreg[1:2]+mlfcw[[idru]]$up_upreg[1:2]
      nfailf=c(length(unique(unlist(modg)))-nsuclf[1],nrow(holder$bkg)-nsuclf[2])

      mlfcw[[idru]]$mimic=unlist(fet(samp.success=nsuclf[1], bkgrnd.success=nsuclf[2], samp.fail=nfailf[1], bkgrnd.fail=nfailf[2], tail = "greater",counts=T))
    }
  }
  k=lcount(k,length(degdb))
}

  cat('\t\tcompile output\n')
  rm(holder)
  k=1
  mpval=''
  mlfpv=''
  msens=''
  mspec=''
  for(idru in names(mstat)){
    holder=as.data.frame(t(as.data.frame(mstat[[idru]])))
    holder$sensitivity=holder$samp.success/(holder$samp.success+holder$samp.fail)
    holder$specificity=holder$samp.success/(holder$samp.success+holder$bkgrnd.success)

    dummy=holder[,'FETp',drop=F]
      colnames(dummy)=idru
    mpval=rmerge(mpval,dummy,verbose=F)

    dummy=holder[,'sensitivity',drop=F]
      colnames(dummy)=idru
    msens=rmerge(msens,dummy,verbose=F)

    dummy=holder[,'specificity',drop=F]
      colnames(dummy)=idru
    mspec=rmerge(mspec,dummy,verbose=F)


    holder=as.data.frame(t(as.data.frame(mlfcw[[idru]])))
    dummy=holder[,'FETp',drop=F]
      colnames(dummy)=idru
    mlfpv=rmerge(mlfpv,dummy,verbose=F)

    k=lcount(k,length(mstat))
  }

  mpval=t(mpval[rownames(mpval)!='1',colnames(mpval)!='x'])
  msens=t(msens[rownames(msens)!='1',colnames(msens)!='x'])
  mspec=t(mspec[rownames(mspec)!='1',colnames(mspec)!='x'])
  mlfpv=t(mlfpv[rownames(mlfpv)!='1',colnames(mlfpv)!='x'])

  orcol=c('reverse','up_downreg','down_upreg','mimic','up_upreg','down_downreg')
  mpval=mpval[,orcol[orcol%in%colnames(mpval)]]
  msens=msens[,orcol[orcol%in%colnames(msens)]]
  mspec=mspec[,orcol[orcol%in%colnames(mspec)]]
  mlfpv=mlfpv[,orcol[orcol%in%colnames(mlfpv)]]

  cat('\n\n')

  return(list(pval=as.data.frame(mpval),pvlf=as.data.frame(mlfpv),sensitivity=as.data.frame(msens),specificity=as.data.frame(mspec),fullp=mstat,fullfp=mlfcw,sigen=sigen))

}



resid<-function(dat_mat,cov_mat){
   cat('\tcolnames(dat_mat) same order as rownames(cov_mat)\n')
   print(Table(colnames(dat_mat)==rownames(cov_mat)))
  residd=t(lm(t(dat_mat)~.,data=cov_mat)$residuals)
  return(invisible(residd))
}





gset<-function(rnk_list,gene_set_list,gene_set_bkg,method='gsea',pval_col_nam='FDR',lf_col_nam='logFC'){
##  Piano wrapper - collection of gene set enrichment analysis methods - GSEA appears bugged
## rnk list - first column used for ranks AND direction
#,direction_col_nam='logFC'   ##  assumed is same as lf_col_nam
Library('piano')
# Library()
plike=c("fisher","stouffer","reporter","tailStrength","wilcoxon","mean","median","sum")
tlike=c("maxmean","gsea","fgsea","page")


cat('\t\tcommon background correction\n')
cmap_bkg=bgcommon(rnk_list)
bkg=intersect(cmap_bkg,gene_set_bkg)
rnk_list=lapply(rnk_list,function(x){x[rownames(x)%in%bkg,]})
gene_set_list=lapply(gene_set_list,function(x){x[x%in%bkg]})

for(iset in names(gene_set_list)){
  holder=as.data.frame(gene_set_list[[iset]])
    holder$set=iset
  if(which(names(gene_set_list)==iset)==1){
    genset=holder
  }
  if(which(names(gene_set_list)==iset)!=1){
    genset=rbind(genset,holder)
  }
}
  colnames(genset)[1]='gene'
  Table(genset$set)

gsares=list()
k=1
if(method%in%plike){
  cat('\t',method,': p-like => use',pval_col_nam,'as ranking and sign of',lf_col_nam,'to determine direction\n')
  for(irnk in names(rnk_list)){
    gsares[[irnk]]=runGSA(
          geneLevelStats=rnk_list[[irnk]][,pval_col_nam,drop=F]
          ,directions=rnk_list[[irnk]][,lf_col_nam,drop=F]
          ,gsc=loadGSC(genset)
          ,geneSetStat=method
          # ,signifMethod="geneSampling"
          ,adjMethod="fdr" ##  p.adjust methods + none
          # ,gsSizeLim=c(1,Inf)
          # ,permStats=NULL
          # ,permDirections=NULL
          # ,nPerm=1e4
          # ,gseaParam=1
          ,ncpus=4
          ,verbose=T
          )
    k=lcount(k,length(rnk_list))
  }
}

gsares=list()
k=1
if(method%in%tlike){
  cat('\t',method,'t-like => use',lf_col_nam,'as ranking\n')
  for(irnk in names(rnk_list)){
    print(head(rnk_list[[irnk]][,lf_col_nam,drop=F]))
    print(str(loadGSC(genset)))
    gsares[[irnk]]=runGSA(
          geneLevelStats=rnk_list[[irnk]][,lf_col_nam,drop=F]
          # ,directions=rnk_list[[irnk]][,lf_col_nam,drop=F]
          ,gsc=loadGSC(genset)
          ,geneSetStat=method
          # ,signifMethod="geneSampling"
          ,adjMethod="fdr" ##  p.adjust methods + none
          # ,gsSizeLim=c(1,Inf)
          # ,permStats=NULL
          # ,permDirections=NULL
          # ,nPerm=1e4
          # ,gseaParam=1
          ,ncpus=4
          ,verbose=T
          )
    k=lcount(k,length(rnk_list))
  }
}

#   Head(gsares)
# dummy=gsares[[1]]
# holder=as.data.frame(dummy[13:31])
#   colnames(holder)=names(dummy[13:31])
#   rownames(holder)=names(gene_set_list)
# holder
  return(invisible(gsares))
}










annot.combine<-function(expr_mat,annot_mat,annot_from,annot_to,combine_method='median'){
##  OPTIONS : supported "combine_method"=c('mean','median','sum') ## sum for RNA-seq : transcript per million (TPM) if mapped such that reads are not shared across transcripts (eg cufflinks), else median is more appropriate
##  process expression matrix with corresponding annotation matrix
##  - re-mapping id types and taking median of non-uniquly mapping ids
##  - annot_from & annot_to - expect the name of colname containing current and new ids respectively


  if(!combine_method%in%c('mean','median','sum')){
    stop(paste0('combine_method="',combine_method,'" not currently supported'))
  }

##  identify multiple id to gene mappings and take median expression
  ids=intersect(annot_mat[,annot_from],rownames(expr_mat))
    cat('\n\texpr_mat',round(length(ids)/nrow(expr_mat),digits=3)*100,'%  ids intresect with annot_mat\n')
    cat('\tannot_mat',annot_from,round(length(ids)/length(unique(annot_mat[,annot_from]))*100,digits=3),'%  ids intresect with expr_mat\n')

  expr_mat=expr_mat[ids,]
  annot_mat=annot_mat[annot_mat[,annot_from]%in%ids,]

  cat('\n\tremove non-specific (duplicated) ids in',annot_from,' :\n')
  annot_mat=get.duplicates(annot_mat,annot_from)$unique
  cat('\n\tget genes with multiple ids in',annot_to,' :\n')
  dupli=get.duplicates(annot_mat,annot_to)
  unic=dupli$unique
  dupl=dupli$duplicates

  if(nrow(unic)>0){
    print(1)
    ids=intersect(unic[,annot_from],rownames(expr_mat))
    expu=expr_mat[sort(ids),]
      rownames(unic)=unic[,annot_from]
    unic=unic[rownames(expu),]
    Table(rownames(expu)==rownames(unic))
    Table(unic[,annot_from]==rownames(unic))
      rownames(expu)=unic[,annot_to]

     cat('\n\t',frac(nrow(expu),nrow(expr_mat))*100,'%  ids uniquely mapped\n')
     expr_out=expu
  }



  if((nrow(unic)!=nrow(expr_out)) | (dupl!="")){
    print(2)
      udup=unlist(unique(dupl[,annot_to,drop=F]))
          print(udup)
    if(length(udup)>1){
      print(3)
      cat('\n\tmerge',length(udup),'genes, using ',combine_method,' on all mapped probes :\n')
  #   idup=udup[5]
      dupmed=list()
      k=1
      for(idup in udup){
        humpty=expr_mat[as.character(dupl[dupl[,annot_to]==idup,annot_from]),]

        if(combine_method=='median'){holder=apply(humpty,2,median,na.rm=T)}
        if(combine_method=='mean'){holder=apply(humpty,2,mean,na.rm=T)}
        if(combine_method=='sum'){holder=apply(humpty,2,sum,na.rm=T)}
        
        dupmed[[idup]]=holder
  #     dumpty=cor(humpty,holder)
  #     diag(dumpty)=NA
  #     dumpty=c(min(dumpty,na.rm=T),max(dumpty,na.rm=T),length(dumpty))
  #     cat('\t\t\t',paste(round(dumpty,digits=2),collapse=",\t"),'\n')
        k=lcount(k,length(udup))
      }
      dupfin=t(as.data.frame(dupmed))

        rownames(dupfin)=names(dupmed)
      expr_out=rbind(expu,dupfin)

    }
  }

    cat('\n\tfinal output contains',nrow(expr_out),' genes, ',round(nrow(expr_out)/nrow(expr_mat),digits=2)*100,'% of original input\n')
  return(invisible(expr_out))
}





# list.overlap<-function(alis,blis='',do.pcs=T,do.fet=T,verbose=T,do_plots=F,...){  ## integration required for T/F flags to change inpu
# # if(do_plots){library(corrplot)}           ##  dont want to run the loop if breaks after nearly done 
#   if(length(blis)==1){ if(blis==''){blis=alis}}   ##  convoluted to avoid the length>1 warning msg
#   print(length(blis))
#   matpc=as.data.frame(matrix(NA,nrow=(length(alis)),ncol=(length(blis))))
#   print(dim(matpc))
#     colnames(matpc)=names(blis)
#     rownames(matpc)=names(alis)
#   matpv=matpc
#   k=1

#   for(ilis in names(alis)){
#     if(verbose){k=lcount(k,length(alis))}
#     for(jlis in names(blis)){
#       humpty=alis[[ilis]]
#       dumpty=blis[[jlis]]
#       inters=intersect(humpty,dumpty)
#       unions=union(humpty,dumpty)

#       if(do.pcs){
#         matpc[ilis,jlis]=(length(inters)/length(unions))
# #       matpc[ilis,jlis]=(length(inters)/length(dumpty))
#       }

#       if(do.fet){
#         matpv[ilis,jlis]=fet(sampl=humpty,bkgrnd=unions,success=inters,counts=F,tail = 'greater')$FETp
# #       matpv[ilis,jlis]=fet(sampl=dumpty,bkgrnd=unions,success=inters,counts=F,alternative = "greater")$FETp
#       }
#     }
#   }

#   if(do_plots){
#     if(do.pcs){Heat(as.matrix(round(matpc,digits=2)*100),values='cor',values.rm='0',Colv=F,Rowv=F,main='(intersect / union) *100   2sf',...)}
#     if(do.fet){Heat(matpv),values='pval',Colv=F,Rowv=F,main='-log10(fet P-val)',...)}
#   }

#   if(do.pcs&do.fet){return(invisible(list(perc=matpc,pval=matpv)))}
#   if(do.pcs){return(invisible(matpc))}
#   if(do.fet){return(invisible(matpv))}

# }




list.as.df<-function(inlis,merge=F){
  cat('\tsimple cbind of list(character vectors) - add "NA" to standardise lengths\n')
  maxlen=max(unlist(lapply(inlis,length)))


 dflis=as.data.frame(matrix(NA,nrow=maxlen,ncol=length(inlis)))
   colnames(dflis)=names(inlis)

  for(ivec in names(inlis)){
    dummy=inlis[[ivec]]
    curlen=length(dummy)
    dummy=c(dummy,rep(NA,maxlen-curlen))
    dflis[,ivec]=dummy
  }
 return((dflis))
}



spatiotemp<-function(query,dtb_loc='~/Dropbox/PROJ/spatemp/dtb/ref/spatiotemp_db.GSE25219_rma_pli_dabg.sampid.lm_sva_clust.Rdata'){
##  dtb_loc .Rdata obj named spatiotemp_db in the format of expression_dataset - list[[ireg]][[idev]][genes,samples]
Load(dtb_loc)
  pdat=''
  for(ireg in names(spatiotemp_db)){

    molder=list()
    for(idev in names(spatiotemp_db[[ireg]])){
      holder=spatiotemp_db[[ireg]][[idev]]
      molder[[idev]]=median(unlist(holder[rownames(holder)%in%query,]))
    }

    dummy=t(as.data.frame((molder)))
      colnames(dummy)=ireg
    pdat=rmerge(pdat,dummy)

  }
  pdat=make.numeric(pdat[rownames(pdat)!='1',colnames(pdat)!='x'])
  pdat=pdat[names(spatiotemp_db[[ireg]]),]

  Heat(t(pdat),Colv=F)

}







geo.matrix<-function(datid,file_path){
##  USE: download GEO matrix data and process into smth usable
cur_dir=getwd()
datid=toupper(datid)
# dir.create(paste0(file_path,'/dtb/'))
# dir.create(paste0(file_path,'/dtb/',datid))
  dir.create(paste0(file_path,'/',datid))
# dir.create(file.path(path,'dtb',datid))

# paste0(path,'/dtb/',datid)
  #setwd(file.path(path,'dtb',datid))
  setwd(file.path(paste0(file_path,'/',datid)))
  getwd()

  system(paste0('wget -r -nH --cut-dirs=7 ftp://ftp.ncbi.nlm.nih.gov/geo/series/',substr(datid,1,5),'nnn/',datid,'/matrix/'))

# system(paste0('wget -r -nH --cut-dirs=7 https://www.ncbi.nlm.nih.gov/geo/download/?acc=',datid,'&format=file'))
  # -r                ##  recursively Dounload
  # -nH (--no-host-directories)   ##  cuts out hostname 
  # --cut-dirs=X            ##  (cuts out X directories)

  system(paste0('gunzip ',datid,'_series_matrix.txt.gz'))
# list.files()

  gse=readLines(file.path(paste0(file_path,'/',datid,'/',datid,'_series_matrix.txt')))

  cord=c(
    grep('!Sample_title',gse)
    ,grep('!series_matrix_table_begin',gse)
    ,grep('!series_matrix_table_end',gse)
  )

  meta=as.data.frame(strsplit(gse[1:(cord[1]-2)],'\t'))
    colnames(meta)=meta[1,]
    meta=t(meta[-1,])
  samp=as.data.frame(strsplit(gse[cord[1]:(cord[2]-1)],'\t'))
  for(icol in colnames(samp)){
    samp[,icol]=(gsub('"','',samp[,icol],fixed=T))
  }
    colnames(samp)=samp[1,]
    samp=samp[-1,]
    colnames(samp)=(gsub('"','',colnames(samp),fixed=T))
    colnames(samp)=(gsub('!Sample_','',colnames(samp),fixed=T))

    rownames(samp)=samp$geo_accession
#   Head(samp)
  cat('\t',datid,'contains phenotype data for',nrow(samp),'samples\n')



  expr=as.data.frame(strsplit(gse[(cord[2]+1):(cord[3]-1)],'\t'))
    colnames(expr)=as.character(expr[1,])
    rownames(expr)=as.character(expr[,1])
    expr=t(expr[-1,-1])


    if(nrow(expr)==0){
      cat('\t\tno expression data available\n')
      return(invisible(list(samp=samp,meta=meta)))
    }

      if(nrow(expr)>0){
  cat('\t',datid,'contains expression data for',ncol(expr),'samples\n')
# expr=as.data.frame(strsplit(gse[(cord[2]+1):(cord[3]-1)],'\t'))
#   colnames(expr)=as.character(expr[1,])
#   rownames(expr)=as.character(expr[,1])
#   expr=t(expr[-1,-1])

  expr=make.numeric(expr)
    rownames(expr)=(gsub('"','',rownames(expr),fixed=T))
    colnames(expr)=(gsub('"','',colnames(expr),fixed=T))
    Head(expr)


  overlap(rownames(samp),colnames(expr))

  samp=samp[colnames(expr),]
    colnames(expr)=gsub(' ','.',samp$title)


      cat('\t\texpression data available,',nrow(expr),' genes\n')
setwd(cur_dir)
      return(invisible(list(expr=expr,samp=samp,meta=meta)))
    }


}








empirical_mod_cons<-function(expr_cor_mat,mod_list,nperm=1000,do_plots=F,dat_descr=''){
  if(nrow(expr_cor_mat)!=ncol(expr_cor_mat)){
    cat('\n\tWARNING: expect a square matrix of gene correlations\n')
  }
  cat('\t')
  perm_stat=list()
  clus_stat=list()

  diag(expr_cor_mat)=NA
  expr_cor_mat=abs(expr_cor_mat)

  for(imod in 1:length(mod_list)){
    t2=Sys.time()
      cat("    --------- ",names(mod_list)[imod],length(mod_list[[imod]]),"genes","---------------------",imod,"of",length(mod_list)," \n")
      clus_stat[[names(mod_list)[imod]]]$mean.abscor_matrix=mean(expr_cor_mat[mod_list[[imod]],mod_list[[imod]]],na.rm=T)
#      clus_stat[[names(mod_list)[imod]]]$mean2abscor_matrix=mean(apply(expr_cor_mat[mod_list[[imod]],mod_list[[imod]]],1,mean,na.rm=T),na.rm=T)    # the results appear identical
      clus_stat[[names(mod_list)[imod]]]$clust.length=length(mod_list[[names(mod_list)[imod]]])

      for(iper in 1:nperm){
        perm.ids=sample(rownames(expr_cor_mat),size=clus_stat[[names(mod_list)[imod]]]$clust.length,replace=FALSE)
        perm.cor=as.matrix(expr_cor_mat[perm.ids,perm.ids])
        diag(perm.cor)=NA

      perm_stat[[names(mod_list)[imod]]][[as.character(iper)]]=mean(abs(perm.cor),na.rm=T)
        cat(round(iper/nperm,digits=2),"\r");flush.console()
      }

      clus_stat[[names(mod_list)[imod]]]$pval=(sum(perm_stat[[names(mod_list)[imod]]]>clus_stat[[names(mod_list)[imod]]]$mean.abscor_matrix)+1)/(nperm+1)
      cat('\temprirical P=',clus_stat[[names(mod_list)[imod]]]$pval,'\n')
      print(Sys.time()-t2)
  }

#### separating this out to optimise plotting axis across all plots

    if(do_plots){
#      x_lim=c((min(unlist(perm_stat),unlist(clus_stat))-0.2),(max(unlist(perm_stat),unlist(clus_stat))+0.2))
      x_lim=c(0,0.5)
      for(imod in 1:length(mod_list)){
        hist(perm_stat[[names(mod_list)[imod]]],breaks=30,xlim=x_lim,main=paste(names(mod_list)[imod],dat_descr,'\n',nperm,"permutations, p=",round(clus_stat[[names(mod_list)[imod]]]$pval,digits=2)),xlab="Average absolute correlation")
        abline(v=clus_stat[[names(mod_list)[imod]]]$mean.abscor_matrix,col="red")
      }
    }

  return(invisible(list(clust_stat=t(as.data.frame(lapply(clus_stat,unlist))),perm_stat=perm_stat)))
}























list.overlap<-function(alis,blis='',do.pcs=T,do.fet=T,verbose=T,do_plots=F,...){  ## integration required for T/F flags to change inpu
# if(do_plots){library(corrplot)}           ##  dont want to run the loop if breaks after nearly done 
  if(length(blis)==1){ if(blis==''){blis=alis}}   ##  convoluted to avoid the length>1 warning msg
  print(length(blis))
  matpc=as.data.frame(matrix(NA,nrow=(length(alis)),ncol=(length(blis))))
  print(dim(matpc))
    colnames(matpc)=names(blis)
    rownames(matpc)=names(alis)
  matpv=matpc
  k=1


  for(ilis in names(alis)){
    if(verbose){k=lcount(k,length(alis))}
    for(jlis in names(blis)){
      humpty=alis[[ilis]]
      dumpty=blis[[jlis]]
      inters=intersect(humpty,dumpty)
      unions=union(humpty,dumpty)

      if(do.pcs){
        matpc[ilis,jlis]=(length(inters)/length(unions))
#       matpc[ilis,jlis]=(length(inters)/length(dumpty))
      }

      if(do.fet){
        matpv[ilis,jlis]=fet(sampl=humpty,bkgrnd=unions,success=inters,counts=F,tail = "greater")$FETp
#       matpv[ilis,jlis]=fet(sampl=dumpty,bkgrnd=unions,success=inters,counts=F,tail = "greater")$FETp
      }

    }
  }

  if(do_plots){
    if(do.pcs){Heat(as.matrix(round(matpc,digits=2)*100),values=T,values.rm='0',Colv=F,Rowv=F,main='(intersect / union) *100   2sf',...)}
    if(do.fet){Heat(as.matrix(-log10(matpv)),values=T,Colv=F,Rowv=F,main='-log10(fet P-val)',...)}
  }

  if(do.pcs&do.fet){return(invisible(list(perc=matpc,pval=matpv)))}
  if(do.pcs){return(invisible(matpc))}
  if(do.fet){return(invisible(matpv))}

}




net.overlap<-function(alis,abg=NA,do_plots=T,rev_col=F,...){
# cat('\t USE:\t calculte overlaps between two lists')
# cat('\t USE:\t optional - add backgrounds for bg.common() - not implemented')
#cat('\t NOTE:\t bg_list - options: 'NA' - one2one human orthologous of mice genes used to build the list of cell class enriched genes by Zeisel et al 2015(Science)\n\n')

##===================================================================================================================================
## generate background as per options, if list provided (same length as modules)=================================

 options(warn=-1)
   if(!is.na(abg)){
    abg=bg.list(alis,abg)
   }
 options(warn=0)

    results_mat=as.data.frame(matrix(NA,nrow=length(alis),ncol=length(alis)))
      rownames(results_mat)=names(alis)
      colnames(results_mat)=names(alis)

    stats_mat=as.data.frame(matrix(NA,nrow=length(alis),ncol=3))
      rownames(stats_mat)=names(alis)
      colnames(stats_mat)=c('n.genes','n.overlap')

  if(class(abg)=='list'){
    cons_stat=list(
      pc=results_mat
      ,n.overlap=results_mat
      ,phyper=results_mat
      )
  }

  if(class(abg)!='list'){
    cons_stat=list(
      pc=results_mat
      ,n.overlap=results_mat
      )
  }

  for(amod in 1:length(alis)){
    for(bmod in 1:length(alis)){

      if(class(abg)=='list'){
        bg_comn=intersect(abg[[amod]],bbg[[bmod]])
          cat('\t\t background overlap: ',round(length(bg_comn)/length(abg[[amod]]),digits=2),round(length(bg_comn)/length(bbg[[bmod]]),digits=2),'\n')
        humpty=alis[[amod]][alis[[amod]]%in%bg_comn]
        dumpty=alis[[bmod]][alis[[bmod]]%in%bg_comn]

        comn=intersect(alis[[amod]][alis[[amod]]%in%bg_comn],alis[[amod]][alis[[amod]]%in%bg_comn])
      }

      if(class(abg)!='list'){
        comn=intersect(alis[[amod]],alis[[bmod]])
          cat('\t\t gene overlap: ',names(alis[amod]),round(length(comn)/length(alis[[amod]]),digits=2),'   \t|  ',names(alis[bmod]),round(length(comn)/length(alis[[bmod]]),digits=2),'\n')
        humpty=alis[[amod]]
        dumpty=alis[[bmod]]
      }


      cons_stat$pc[names(alis[amod]),names(alis[bmod])]=round(length(comn)/length(alis[[amod]]),digits=2)
      cons_stat$pc[names(alis[bmod]),names(alis[amod])]=round(length(comn)/length(alis[[bmod]]),digits=2)

      cons_stat$pc[names(alis[amod]),names(alis[bmod])]=round(length(comn)/length(alis[[amod]]),digits=2)
      cons_stat$pc[names(alis[bmod]),names(alis[amod])]=round(length(comn)/length(alis[[bmod]]),digits=2)


      if(class(abg)=='list'){
        cons_stat$phyper[names(alis[amod]),names(alis[bmod])]=phyper(
                                                q=length(comn)                              # n.success.sample
                                                ,m=sum(alis[[bmod]] %in% bg_comn)   # n.success.popn
                                                ,n=length(bg_comn)                  # n.population
                                                ,k=length(alis[[amod]])             # n.sample
                                                ,lower.tail=F)

        cons_stat$phyper[names(alis[bmod]),names(alis[amod])]=phyper(
                                                q=length(comn)                              # n.success.sample
                                                ,m=sum(alis[[amod]] %in% bg_comn)   # n.success.popn
                                                ,n=length(bg_comn)                  # n.population
                                                ,k=length(alis[[bmod]])             # n.sample
                                                ,lower.tail=F)
      }
    }
  }

  if(do_plots){
    Library('corrplot')
    diag(cons_stat$pc)=0.000001

    if(!rev_col){corrplot(make.numeric(cons_stat$pc),p.mat=make.numeric(cons_stat$pc)*100,sig.level=0.001,col=rev(c(colrb)),cl.align="l",tl.col="black",method='circle',is.corr=F,insig='p-value',...)}#,cl.lim=c(0,70)
    if(rev_col){corrplot(make.numeric(cons_stat$pc),p.mat=make.numeric(cons_stat$pc)*100,sig.level=0.001,col=c(colrb),cl.align="l",tl.col="black",method='circle',is.corr=F,insig='p-value',...)}#,cl.lim=c(0,70)
    
    diag(cons_stat$pc)=1
  }
  return(invisible(cons_stat))
}



net.cons<-function(alis,blis,abg=NA,bbg=NA,do_plots=T,p_thresh=0.01,main="Maximum % of overlap with p hyper > 0.01"){
# cat('\tUSE:\tcalculte overlaps between two lists')
# cat('\tUSE:\toptional - add backgrounds for bg.common() - not implemented')
#cat('\tNOTE:\tbg_list - options: 'NA' - one2one human orthologous of mice genes used to build the list of cell class enriched genes by Zeisel et al 2015(Science)\n\n')

##===================================================================================================================================
## generate background as per options, if list provided (same length as modules)=================================

 options(warn=-1)

   if(!is.na(abg)){
    abg=bg.list(alis,abg)

   }
   if(!is.na(bbg)){
    bbg=bg.list(blis,bbg)
   }

 options(warn=0)

    results_mat=as.data.frame(matrix(NA,nrow=length(alis),ncol=length(blis)))
      rownames(results_mat)=names(alis)
      colnames(results_mat)=names(blis)


  if(class(abg)=='list'){
    cons_stat=list(
      pc_test=results_mat
      ,phyper_test=results_mat
      ,pc_repl=results_mat
      ,phyper_repl=results_mat
      )
  }

  if(class(abg)!='list'){
    cons_stat=list(
      pc_test=results_mat
      ,pc_repl=results_mat
      )
  }


  for(amod in 1:length(alis)){
    for(bmod in 1:length(blis)){
#       cat(amod,'  |  ',bmod,'\n')

      if(class(abg)=='list'){
        bg_comn=intersect(abg[[amod]],bbg[[bmod]])
#          cat('\t\t background overlap: ',round(length(bg_comn)/length(abg[[amod]]),digits=2),round(length(bg_comn)/length(bbg[[bmod]]),digits=2),'\n')
        alis[[amod]]=alis[[amod]][alis[[amod]]%in%bg_comn]
        blis[[bmod]]=blis[[bmod]][blis[[bmod]]%in%bg_comn]
        comn=intersect(alis[[amod]],blis[[bmod]])
          cat('\t\t gene overlap: ',names(alis[amod]),round(length(comn)/length(alis[[amod]]),digits=2),'  |  ',names(blis[bmod]),round(length(comn)/length(blis[[bmod]]),digits=2),'\n')
      }

      if(class(abg)!='list'){
        comn=intersect(alis[[amod]],blis[[bmod]])
          cat('\t\t gene overlap: ',names(alis[amod]),round(length(comn)/length(alis[[amod]]),digits=2),'  |  ',names(blis[bmod]),round(length(comn)/length(blis[[bmod]]),digits=2),'\n')
      }


      cons_stat$pc_test[names(alis[amod]),names(blis[bmod])]=round(length(comn)/length(alis[[amod]]),digits=2)
      cons_stat$pc_repl[names(alis[amod]),names(blis[bmod])]=round(length(comn)/length(blis[[bmod]]),digits=2)

      if(class(abg)=='list'){
        cons_stat$phyper_test[names(alis[amod]),names(blis[bmod])]=phyper(
                                                q=length(comn)                      # n.success.sample
                                                ,m=sum(blis[[bmod]] %in% bg_comn)   # n.success.popn
                                                ,n=length(bg_comn)                  # n.population
                                                ,k=length(alis[[amod]])             # n.sample
                                                ,lower.tail=F)

        cons_stat$phyper_repl[names(alis[amod]),names(blis[bmod])]=phyper(
                                                q=length(comn)                      # n.success.sample
                                                ,m=sum(alis[[amod]] %in% bg_comn)   # n.success.popn
                                                ,n=length(bg_comn)                  # n.population
                                                ,k=length(blis[[bmod]])             # n.sample
                                                ,lower.tail=F)
      }
    }
  }

  cons_stat$pc_max=pmax(cons_stat$pc_test,cons_stat$pc_repl)
  if(class(abg)=='list'){
  cons_stat$phyper_min=pmin(cons_stat$phyper_test,cons_stat$phyper_repl)
  }

  if(do_plots){
    Library('corrplot')

    if(class(abg)!='list'){
      corrplot(make.numeric(cons_stat$pc_max),method="circle",is.corr=FALSE
        ,cl.lim=c(0, 1),col=rev(c(colrb)),cl.align="l",tl.col="black"
        ,mar=c(0,0,4,0)
        ,title=main
        )    
    }

    if(class(abg)=='list'){
      corrplot(make.numeric(cons_stat$pc_max),method="circle", is.corr=FALSE
        ,p.mat=make.numeric(cons_stat$phyper_min), insig="blank",sig.level=p_thresh
        ,cl.lim=c(0, 1),col=rev(c(colrb)),cl.align="l",tl.col="black"
        ,mar=c(0,0,4,0)
        ,title=main
        )
    }
  }
  return(invisible(cons_stat))
}




linear<-function(yvar,xvar,verbose=F){
##  linear model (assume same order for yvar & xvar)
##  yvar - numeric vector (can not be factor)
##  xvar - vector or data.frame - factor or numeric
  
  tester=data.frame(yvar,xvar)  ##  faster than merge & checking for correct order beforehand
  tester=(tester[complete.cases(tester),])
  # str(tester)
  tesn=nrow(tester)
  # print(str(tester))
  tester=lm(tester[,1]~.,data=tester[,2:ncol(tester),drop=F])

  holder=summary(tester)
  tesvp=holder$coefficients[,"Pr(>|t|)"][-1]
  tesvr=holder$coefficients[,"Estimate"][-1]
##  if adjusted R.sqared <0 => is worse than no model, sign assigned later to determine direction of "correlation"
  tesr=holder$adj.r.squared
  if(tesr<0){tesr=0}          ##  negative adjuster R squared - model is worse than no model
  tesp=holder$fstatistic
  tesp=pf(tesp[1],tesp[2],tesp[3],lower.tail=F)
  attributes(tesp)=NULL   ##  p-value is more accurate than one from lm() since lm < 2.2e-16 

  return(list(pval=tesp,rsq=tesr,n=tesn,indpval=tesvp,indrsq=tesvr,lm=tester))
}




####  mostly fininshed function - get dtb of ID conversions from HGNC and re-annotate a list of gene ids to hgnc or ensg
####   * ensg - sometimes 2 ids are mapped - concatenated using ';' -> need to expand bk if using idtyp='ensg'
####   * loop to convert genlis - not tested
####   * check for and eliminate double-mapping inconsistencies (ie 2idsTo1, 1idTo2 etc) (ensg && hgnc)
####   * pontentiall set up a dtb folder - check if dtb exists before trying to connect and compile a new one - takes ~15 mins
####   * add more ids as required, currently only retrieves gene_names, hgnc, ensg, entrez
# geni<-function(genlis,idtyp='hgnc',org='hsap'){
# ##   genlis=list(list1=list1,list1=list2, ..)  list of character vectors (>=1) of gene names to be converted
# ##   org=c('hsap','mmus')
# ##   idtyp=c('hgnc','ensg')
#   if(class(genlis))

#   if(org=='hsap'){
#      cat('\n\tretrieve gene info for homo sapiens\n')
# ## dummy=listAttributes(useDataset('hsapiens_gene_ensembl',mart=useMart('ENSEMBL_MART_ENSEMBL')) ##  list all available annotations - add $name below as required
#   attrib=getBM(attributes=c('ensembl_gene_id','entrezgene','external_gene_name','hgnc_symbol','gene_biotype','chromosome_name','start_position','end_position','strand','band','percentage_gene_gc_content')
#             # ,'mmusculus_homolog_associated_gene_name','mmusculus_homolog_orthology_type')#,'cds_length')    ##  Query ERROR: caught BioMart::Exception::Usage: Attributes from multiple attribute pages are not allowed
#     ,mart=useDataset('hsapiens_gene_ensembl',mart=useMart('ENSEMBL_MART_ENSEMBL')))
#   }
#   if(org=='mmus'){
#      cat('\n\torganism :\tmus musculus\n')
#     org='mmusculus_gene_ensembl'
#   }
  

#    cat('\n\t\n')

#   gnam=unique(attrib[,c('ensembl_gene_id','entrezgene','external_gene_name','hgnc_symbol')])


#   # Table(attrib$external_gene_name==attrib$hgnc_symbol)  ## 64% same

#   unique(attrib$hgnc_symbol)

#    cat('\n\tconcatenate idmap database\n')
#   ugen=unique(attrib$hgnc_symbol)
#   ugen=ugen[ugen!='']
#   ugen=ugen[!is.na(ugen)]
#   k=1
#   for(igen in ugen){

#     humpty=gnam[gnam$hgnc_symbol==igen,]
#     dumpty=unique(unlist(humpty))
#     dumpty=dumpty[!is.na(dumpty)]
#     dumpty=dumpty[dumpty!='']

#     holder=as.data.frame(dumpty)
#       colnames(holder)='ids'
#     holder$hgnc=igen
#     # if(!is.na(humpty[,'ensembl_gene_id'])){
#       holder$ensg=paste(humpty[,'ensembl_gene_id'],collapse=';')
#     # }
#     if(k==1){idmap=holder}
#     if(k>1){idmap=rbind(idmap,holder)}
#      k=lcount(k,length(ugen))
#   }

#   for(ilis in names(genlis)){
#     genlis[[ilis]]=idmap[idmap$ids%in%genlis[[ilis]],idtyp]
#   }

# }

duplicates<-function(dat_vec,...){
  return(dat_vec[duplicated(dat_vec)])
}

rm.duplicates<-function(matrix,colName,verbose=T){
  clean=matrix[!duplicated(matrix[,which(colnames(matrix)==colName)]),]
    if(verbose){cat("     ",sum(duplicated(matrix[,which(colnames(matrix)==colName)]))," duplicates removed  || ",round(nrow(clean)/nrow(matrix),digits=3)*100,"% of data remaining || ",sum(duplicated(clean[,which(colnames(clean)==colName)])),"dupilicates remaining \n")}

  return(invisible(clean))
}


group.col<-function(dat_groups){
  options(warn=-1)
  if(length(dat_groups)>1){
    datleg=Table(dat_groups)[,1:2]
      colnames(datleg)=c('name','n')
    datleg$color=colmix[1:nrow(datleg)]
    datleg$point=pchmix[1:nrow(datleg)]

    datcol=rep('magenta',length(dat_groups))
    datpch=rep(10,length(dat_groups))
  for(idat in 1:nrow(datleg)){
    datcol[dat_groups==datleg$name[idat]]=datleg$color[idat]
    datpch[dat_groups==datleg$name[idat]]=datleg$point[idat]
    # datcol=c(datcol,rep(datleg$color[idat],datleg$n[idat]))
    # datpch=c(datpch,rep(datleg$point[idat],datleg$n[idat]))
  }
    # datcol=datcol[-1]
    datpch=as.numeric(datpch[-1])
    dat_is_list=T
  }
  options(warn=0)
  return(datcol)
}


eefnr<-function(dat_lis,dtb='default',phen=''){ #bkg,
##  dtb - required list - DNM in 'conrols' ie healthy parents & offspring
##   dnmDB - pre-made dataset, part of "adds" package, downloaded from http://denovo-db.gs.washington.edu/denovo-db/  ## mapped to HUGO gene ids, genes with DNM in controls removed from DNM in
  if(dtb[1]=='default'){
    # cat('\tLoad(~/Dropbox/PROJ/ednm/dtb/denovo-db.variants.v.1.5__frameshift_missense_stopgain.Rdata)\n')
    Load('~/Dropbox/PROJ/epitar/methyl_51/dtb/ref/scz_GSE89702_3_5_6.mVals_bVals.GenomicRatioSet.Rdata')
    print(str(efn))
    # Load('~/Dropbox/PROJ/ednm/dtb/denovo-db.variants.v.1.5__frameshift_missense_stopgain.Rdata')
    dtb=efn
  }      ##  load pre-made dataset, part of "adds" package
  if(!('control'%in%names(dtb))){stop('"control" - list of DNM in healthy controls && offspring (named "control") is required')}


   cat('\tcheck DNM dtb and background compatibility\n')
  # bkg=overlap(unlist(dtb),bkg)

  # bkg=c(bkg$inter,bkg$inb)  ##  background is specified as overap && 'expressed' - ie have significant signal in the dataset used to derive the list
  ## the above definition is the same as the "bkg" input by definiton..

  dtb_contr=dtb$control
  dtb=dtb[names(dtb)!='control']

   cat('\n')
  # str(bkg)
  if(length(intersect(unlist(dtb),unlist(dat_lis)))==0){stop('check that dat_lis and bkg IDs are HUGO gene names OR match the provided dtb')}
  # dtb=lapply(dtb,function(x){x[x%in%bkg]})
  dnmen=list()
  fetp=list()
  cat('\n\nperform FET enrichment\n')
  for(idnm in names(dtb)){
    cat('\t',idnm)
    holder=list()
    for(idat in names(dat_lis)){
      holder[[idat]]=unlist(fet(
              # sampl=dat_lis[[idat]]
              # ,bkgrnd=bkg
              # ,success=dtb[[idnm]]
              # ,
              counts=T
              ,samp.success=sum(dtb[[idnm]]%in%dat_lis[[idat]])
              ,bkgrnd.success=sum(dtb_contr%in%dat_lis[[idat]])
              ,samp.fail=sum(!(dtb[[idnm]]%in%dat_lis[[idat]]))
              ,bkgrnd.fail=sum(!(dtb_contr%in%dat_lis[[idat]]))
              ,tail='greater'
              ))

    }
    dnmen[[idnm]]=as.data.frame(t(as.data.frame(holder)))
    fetp[[idnm]]=dnmen[[idnm]]$FETp

  }
    cat('\n\n')

  fetp=as.data.frame(fetp)
    rownames(fetp)=names(dat_lis)

  return(list(fetp=fetp,dnmen=dnmen))
}

