getmonitor <- function(id, directory, summarize = FALSE) {
	if(is.numeric(id)==FALSE){
	idy = as.numeric(id)
	}
	else{idy = id}
	idx = sprintf("%03d",idy)
	openx = paste(as.character(directory),idx,sep='/')
 	peny = paste(openx,'.csv',sep='')
 	x = read.csv(peny)
	if(summarize==TRUE){print(summary(x))}
	return(x)
	}
