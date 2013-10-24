rankhospital <- function(state,out_name,num){
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

	if(check1==1 && check2==1){
		lolz = lol[order(lol[,index],lol[,2]),]
		empty = which(is.na(lolz[,index]))
		na_ind = empty[1]		
		if(num=="best"){
				return(lolz[1,2])			
				}
		else if(num=="worst"){
				if(na_ind-1 != 0){print(na_ind-1)
					return(lolz[na_ind-1,2])
					}
				else{return(NA)# stop("invalid outcome")
				}		
			}
		else if(num>=na_ind){
			return(NA)
			#stop("invalid outcome")		
			}
		else {
			return(lolz[num,2])			
			}
		}
	else{
		stop("invalid outcome")
		}

}
