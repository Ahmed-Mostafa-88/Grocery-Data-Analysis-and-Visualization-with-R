library(readr)
datasetPath<- readline("Enter path of csv file: ")                  
tabledata<-read_csv(datasetPath)




summary(grc)
head(grc)
tail(grc)





table(tabledata$paymentType)   
#to get details by number#

barplot(
  height =table(tabledata$paymentType),
  col = "red",
  main = "Compare cash and credit totals",
  xlab = "paymenttype",
  ylab = "totals") 



#by using pie #
pie(
  x = table(tabledata$paymentType),
  main = "Compare cash and credit totals")









library(dplyr)
agepertotalspending <- group_by(tabledata,age)
agepertotalspending <- summarise(agepertotalspending,totalspending=sum(total))
agepertotalspending    
#to show numric details #
plot(agepertotalspending)         #to scaterplot it#

pie(
  x =agepertotalspending$totalspending,
  labels = agepertotalspending$age,        # to piechar it#
  main = "Compare age and total spending")








library(dplyr)
cityperspending<- group_by(tabledata,city)
cityperspending <- summarise(cityperspending,totalspending=sum(total))
cityperspending

citiesSpending<- sort(cityperspending$totalspending, decreasing=TRUE)


pie(  x = citiesSpending,
      labels = cityperspending$city,                                                                      
      main = "Compare age and total spending")










boxplot(
  x = tabledata$total,
  main = "Distribution of total 
spending",
  xlab = "total spending"
) 










par(mfrow=c(2,3))

barplot(
  height =table(tabledata$paymentType),
  col = "red",
  main = "Compare cash and credit totals",
  xlab = "paymenttype",
  ylab = "totals") 

plot(agepertotalspending)

pie(  x = citiesSpending,
      labels = cityperspending$city,                                                                      
      main = "Compare age and total spending")



boxplot(
  x = tabledata$total,
  main = "Distribution of total 
spending",
  xlab = "total spending"
) 


















groupedData<- group_by(grc,age)
groupedData<- summarise(groupedData,sum=sum(total))
numOfClusters <- (readline(prompt = "Enter the number of clusters from 2 to 4: "))
kmeans <- kmeans(groupedData,centers = numOfClusters)

if(numOfClusters >= 2 & numOfClusters <= 4){
  print(kmeans)
}else 
  print("Wrong input")
customers<- grc$customer
ages<- grc$age
totals<- grc$total
clusterNumber<- kmeans$cluster

newtable<- data.frame(customers, ages, totals, clusterNumber)
colnames(newtable) <- c("customer","age","total","cluster number")
newtable
newtable<- data.frame(customers, ages, totals)






















library(arules)
dataItems<-read.transactions("C:/Users/Enter Store/Downloads/grcDataset-master/grc.csv", sep=',') 
inspect(dataItems)
minConfidence<- as.numeric(readline(" Enter confidence here from 0.001 to 1: "))
minSupport<- as.numeric(readline("Enter Support here from 0.001 to 1: "))
apriori_rules<- apriori(dataItems, parameter=list(support=minSupport, confidence=minConfidence, minlen=2))
inspect(apriori_rules)





