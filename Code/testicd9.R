###############################################################################
#
# Project: 	ACO Projects
# Script:	testicd9.R
# Version:	
# Created:	
# Updated:	Dec 3, 2014
# Author: 	ry5t
###############################################################################
library(icd9)

x <- c("276","0276","2760","00276","02760","27600","000276","002760","027600","276000","0000276","0002760","0027600","0276000","2760000")

for (i in 1:length(x)){
	print(x[i])
	z <- (icd9RealDecimal(x[i]))
	print(z)
	if (z){
		y <- icd9DecimalToShort(x[i])
		print(y)
		print(icd9ExplainShort(y))
		
	}
}



x <- c("9","09","90","009","090","900","0009","0090","0900","9000","00009","00090","00900","09000","90000")

for (i in 1:length(x)){
	print(x[i])
	z <- (icd9RealDecimal(x[i]))
	print(z)
	if (z){
		y <- icd9DecimalToShort(x[i])
		print(y)
		print(icd9ExplainShort(y))
		
	}
}


x <- c("276","0276","2760","00276","02760","27600")

for (i in 1:length(x)){
	print(x[i])
	z <- (icd9RealDecimal(x[i]))
	print(z)
	if (z){
		y <- icd9DecimalToShort(x[i])
		print(y)
		print(icd9ExplainShort(y))
		
	}
}


