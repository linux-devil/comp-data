best <- function(state , out_name){
	outcome = read.csv("outcome-of-care-measures.csv", colClasses = "character")
	outcome[, 11] <- as.numeric(outcome[, 11])
	outcome[, 17] <- as.numeric(outcome[, 17])
	outcome[, 23] <- as.numeric(outcome[, 23])
	check1 = 0
	check2 = 0
	statex = outcome[,7]
	for(i in state){
	if(i==state){
	check1 =1
	lol <- subset(outcome, outcome[,7]==state)
		}
	}
	
	#out_cond = c("heart attack","heart failure","pneumonia")
	#for(i in out_cond){if(i==out_name){check2=1}}

	if(out_name=="heart attack"){ 
	index = 11
	check2=1	
		}
	else if(out_name=="heart failure"){ 
	index =17 
	check2=1
		}
	else if(out_name=="pneumonia"){	
	index = 23 
	check2=1
		}

	if(check1==1 && check2 ==1){
		#indix = which.min(lol[,index])
		val = min(lol[,index],na.rm=1)
		z = lol[lol[,index]==val,]
		hosp = unique(z[complete.cases(z[,2]),])
		vec = hosp[,2]
		ko = sort(vec)
		#if(which(is.na(ko))==1){stop("invalid outcome")}
		if(length(ko)==0){stop("invalid outcome")}		
		else{
		return(ko[1])}
	# find lowet mortality for some condition	
	}
	else{
		stop("invalid outcome")
	##error
	}
}
