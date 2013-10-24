count <- function(cause=NULL){

	#if(cause == NULL){ stop("null not allowed")}
	chek1 = 0
	out_cause = c("asphyxiation","blunt force","other","shooting","stabbing","unknown")
	
	for(i in out_cause){if(i==cause){chek1=1}}
	
	if(chek1==1){
		homicides <- readLines("homicides.txt")
		if(cause=="shooting"){
		num <- length(grep("[Cc]ause: [Ss]hooting",homicides))
		return(num)
		}
		else if(cause=="asphyxiation"){
		num <- length(grep("[Cc]ause: [Aa]sphyxiation",homicides))
		return(num)
		}
		else if(cause=="blunt force"){
		num <- length(grep("[Cc]ause: [Bb]lunt [Ff]orce",homicides))
		return(num)
		}
		else if(cause=="other"){
		num <- length(grep("[Cc]ause: [Oo]ther",homicides))
		return(num)
		}
		else if(cause=="unknown"){
		num <- length(grep("[Cc]ause: [Uu]nknown",homicides))
		return(num)
		}
	else{
		num <- length(grep("[Cc]ause: [Ss]tabbing",homicides))
		return(num)
		}	
	}
	else{stop("error")}
}
