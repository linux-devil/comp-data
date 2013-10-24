complete <- function(directory, id=1:332) {
	N <-500
	df = data.frame(id=rep(NA,N),nobs=rep(NA,N))
	count = 0
	for(i in id){
	count=count+1
	idx = sprintf("%03d",i)
	openx = paste(as.character(directory),idx,sep='/')
 	peny = paste(openx,'.csv',sep='')
 	x = read.csv(peny)
	z = x[complete.cases(x),]
	cz = nrow(z)
	df[count, ] <- c(i,cz)
	}
	final = df[complete.cases(df),]
	print(final)
	}

