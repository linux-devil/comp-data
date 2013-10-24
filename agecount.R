agecount <- function(age = NULL){
	agex = as.numeric(age)
	if (agex <= 150 && agex>=0){
	homicides <- readLines("homicides.txt")
	x <- "([0-9]+) years old"
	r <- regexec(x,homicides)
	m <- regmatches(homicides,r)
	age <- sapply(m, function(x) x[2])
	age = as.numeric(age)
	count = 0
	for(i in age){ 
		if((!is.na(i)) &&i==agex ){count = count+1}}
	return(count)
	}

	else{stop("error in age")}
}
