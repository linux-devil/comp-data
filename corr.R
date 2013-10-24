corr <- function(directory, threshold=0) {
	result =vector('numeric')
	count = 0
	for(i in 1:332){
	idx = sprintf("%03d",i)
	openx = paste(as.character(directory),idx,sep='/')
 	peny = paste(openx,'.csv',sep='')
 	x = read.csv(peny)
	z = x[complete.cases(x),]
	cz = nrow(z)
	if(cz>=threshold){
		count = count+1
		chico = cor(z$sulfate,z$nitrate)
		result[count] = chico
		}
	}
	return(result)
	}

