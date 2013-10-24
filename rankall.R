rankall <- function(out_name,num="best"){
	outcome = read.csv("outcome-of-care-measures.csv", colClasses = "character")
	outcome[, 11] <- as.numeric(outcome[, 11])
	outcome[, 17] <- as.numeric(outcome[, 17])
	outcome[, 23] <- as.numeric(outcome[, 23])
	check2 = 0
	statex = outcome[,7]
	uni_state = unique(statex)
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

	emptydf <- data.frame()
	if(check2==1){		
		for(i in uni_state){			
			lol <- subset(outcome, outcome[,7]==i)
			lolz = lol[order(lol[,index],lol[,2]),]
			empty = which(is.na(lolz[,index]))
			filld = which(!is.na(lolz[,index]))
			len_fill = length(filld)
			na_ind = empty[1]
			if(num=="best"){
					hosp = lolz[1,2]
					point = lolz[1,index]		
					}
			else if(num=="worst"){
					if(na_ind-1 != 0){
						hosp = lolz[na_ind-1,2]
						point = lolz[na_ind-1,index]
						}
					else{ hosp = NA 
						point =NA
					}		
				}
			else if(is.na(na_ind) && num>len_fill){
				hosp = NA
				point = NA
				}
			else if(num<= len_fill){
				hosp = lolz[num,2]
				point = lolz[num,index]			
				}
			else{hosp = NA
				point = NA}
		 vec = c(hosp,i)	
		 emptydf <- rbind(emptydf, data.frame(hospital= hosp, state = i))		
			}
		res = emptydf[order(emptydf[,1],emptydf[,2]),]
		res = c(outcome$hosp,outcome$state)	
		}
	return(emptydf)
}
