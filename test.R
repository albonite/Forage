e <- function(){
myData <- read.table("data.txt",header = TRUE)
trie <-myData[order(myData[,2]),]
myres <- data.frame(matrix(ncol=7,nrow=10))
colnames(myres) <- c("id","A1","A2","A3","A4","A5","A6")
rownames(myres) <- c("cl-1","cl-2","cl-3","cl-4","cl-5","cl-6","cl-7","cl-8","cl-9","cl-10")
matrixRow <- nrow(myData) 
i<-1
j<-i
while (i<=100){
  res = trie[i:(i+9),]
  somme <- colSums(res)
  for(k in 2:7){
  myres [j,k]<- somme[k]/10
  }
  i<-i+10
  j<-j+1
}
myres[,2:7]
}
f <- function(){
	mydata = read.table("data.txt", header = TRUE)
	attach(mydata)
	nbSegment = 10
	nbCol = ncol(mydata)
	nbLi = nrow(mydata)
	#Distance de mahalanobis entre chaque point de la matrice et le centre 
	dist = mahalanobis(mydata[,2:7],colMeans(mydata[,2:7]),cov(mydata[,2:7]))
	#Trier dans l'ordre décroissant pour récupérer les 2 plus grandes valeurs
	trier = sort(dist,decreasing=TRUE)
	values = trier[1:2]
	
	index = match(values[1:2],dist)
	print(mydata[index[1],1])
	print(mydata[index[2],1])
}

g <- function(){
	mydata = read.table("data.txt", header = TRUE)
	myclasse = read.table("classe.txt", header = TRUE)
	myDataClasse = cbind(mydata,myclasse[,2])
	mySortData <- myDataClasse[order(myDataClasse[,3]),]
	mySortData
	m <- matrix(nrow = 1,ncol = 3)
	m[1,1] = sum(mySortData[,8]=="c1")
	m[1,2] = sum(mySortData[,8]=="c2")
	m[1,3] = sum(mySortData[,8]=="c3")
	m
	entropyRef = -((m[1,1]/sum(m))*log2((m[1,1]/sum(m))))
	entropyRef
	#creation segment 
	S1 <- mySortData[1:30,]
	S2 <- mySortData[31:60,]
	S3 <- mySortData[61:100,]
	#entropy / segment
	m1 <- matrix(nrow = 1,ncol = 3)
	m2 <- matrix(nrow = 1,ncol = 3)
	m3 <- matrix(nrow = 1,ncol = 3)

	m1[1,1] = sum(S1[,8]=="c1")
	m1[1,2] = sum(S1[,8]=="c2")
	m1[1,3] = sum(S1[,8]=="c3")
	m2[1,1] = sum(S2[,8]=="c1")
	m2[1,2] = sum(S2[,8]=="c2")
	m2[1,3] = sum(S2[,8]=="c3")
	m3[1,1] = sum(S3[,8]=="c1")
	m3[1,2] = sum(S3[,8]=="c2")
	m3[1,3] = sum(S3[,8]=="c3")
	m1
	m2
	m3

	entropyS1 = -((m1[1,1]/sum(m1))*log2((m1[1,1]/sum(m1))))
	entropyS2 = -((m2[1,1]/sum(m2))*log2((m2[1,1]/sum(m2))))
	entropyS3 = -((m3[1,1]/sum(m3))*log2((m3[1,1]/sum(m3))))

	print(entropyS1 )
	print(entropyS2 )
	print(entropyS3 )
	entropyM<-mean(entropyS1,entropyS2,entropyS3)
	print(entropyM)
	while(entropyM>entropyRef*0.75){
		if(length(S1)>1){
		append(tail(S1, n=1),S2)
		S1<-S1[1:length(S1)-1,]
		m1[1,1] = sum(S1[,8]=="c1")
	m1[1,2] = sum(S1[,8]=="c2")
	m1[1,3] = sum(S1[,8]=="c3")
	m2[1,1] = sum(S2[,8]=="c1")
	m2[1,2] = sum(S2[,8]=="c2")
	m2[1,3] = sum(S2[,8]=="c3")
	m3[1,1] = sum(S3[,8]=="c1")
	m3[1,2] = sum(S3[,8]=="c2")
	m3[1,3] = sum(S3[,8]=="c3")
	entropyS1 = -((m1[1,1]/sum(m1))*log2((m1[1,1]/sum(m1))))
	entropyS2 = -((m2[1,1]/sum(m2))*log2((m2[1,1]/sum(m2))))
	entropyS3 = -((m3[1,1]/sum(m3))*log2((m3[1,1]/sum(m3))))
	entropyM<-mean(entropyS1,entropyS2,entropyS3)
	print(entropyM)

	}else{
	break
}
		print(S1)
		print(S2)
		print(S3)
	
}

	

	
	
}

z <- function(x){
	mydata = read.table("data.txt", header = TRUE)
	myclasse = read.table("classe.txt", header = TRUE)
	myDataClasse = cbind(mydata,myclasse[,2])
	mySortData <- myDataClasse[order(myDataClasse[,3]),]
	mySortData
	m <- matrix(nrow = 2,ncol = 3)
	test<-0
	entropyMeanO<- Inf
	entropyMean<- 0
	indexReturn <-1
	for(i in x:99){
		m[1,1] = sum(mySortData[1:i,8]=="c1")
		m[1,2] = sum(mySortData[1:i,8]=="c2")
		m[1,3] = sum(mySortData[1:i,8]=="c3")
		j<-i+1
		m[2,1] = sum(mySortData[j:100,8]=="c1")
		m[2,2] = sum(mySortData[j:100,8]=="c2")
		m[2,3] = sum(mySortData[j:100,8]=="c3")
		if(m[1,1]==0){
			lg1<- 0
		}else{
			lg1<- log2((m[1,1]/100))
		}
		if(m[1,2]==0){
			lg2<- 0
		}else{
			lg2<- log2((m[1,2]/100))
		}
		if(m[1,3]==0){
			lg3<- 0
		}else{
			lg3<- log2((m[1,3]/100))
		}

		if(m[2,1]==0){
			lg21<- 0
		}else{
			lg21<- log2((m[2,1]/100))
		}
		if(m[2,2]==0){
			lg22<- 0
		}else{
			lg22<- log2((m[2,2]/100))
		}
		if(m[2,3]==0){
			lg23<- 0
		}else{
			lg23<- log2((m[2,3]/100))
		}

		entropyNewL1 = -((m[1,1]/100)*lg1+
				(m[1,2]/100)*lg2+
				(m[1,3]/100)*lg3)
		entropyNewL2 = -((m[2,1]/100)*lg21+
				(m[2,2]/100)*lg22+
				(m[2,3]/100)*lg23)

		entropyMean = ((sum(m[1,])/100)*entropyNewL1+(sum(m[2,])/100)*entropyNewL2)
		print(sum(m[1,]))
		print(sum(m[2,]))
		test<-c(test,entropyMean)
		print("------")
		print(entropyNewL1)
		print(entropyNewL2)
		print(entropyMean )
		print("------")
		if(entropyMeanO>entropyMean){
			entropyMeanO= entropyMean	
			indexReturn=indexReturn+1
		}else{
			print(m)
			return(c(indexReturn-1,entropyMeanO))
		}
		
	}
	plot(test[2:100])
  print(min(test[2:100]))	
}
h <- function(){
	mydata = read.table("data.txt", header = TRUE)
	myclasse = read.table("classe.txt", header = TRUE)
	myDataClasse = cbind(mydata,myclasse[,2])
	mySortData <- myDataClasse[order(myDataClasse[,8]),]
	for(i in 1:100){
		if(myDataClasse[i,8]=='c1'){
			
		}
		if(myDataClasse[i,8]=='c2'){
			
		}
		if(myDataClasse[i,8]=='c3'){

		}
	}	
	mySortData 



}	
