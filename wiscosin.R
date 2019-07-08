
AUTHOR: NAVEEN BALARAJU



############################################################################################
################################## LOADING DATASET #########################################
############################################################################################


#This breast cancer databases is obtained from the University of Wisconsin Hospitals, 
# Madison from Dr. William H. Wolberg.
#1. Sample code number: id number 
#2. Clump Thickness: 1 - 10 
#3. Uniformity of Cell Size: 1 - 10 
#4. Uniformity of Cell Shape: 1 - 10 
#5. Marginal Adhesion: 1 - 10 
#6. Single Epithelial Cell Size: 1 - 10 
#7. Bare Nuclei: 1 - 10 
#8. Bland Chromatin: 1 - 10 
#9. Normal Nucleoli: 1 - 10 
#10. Mitoses: 1 - 10 
#11. Class_type (2 for benign, 4 for malignant)
BC_data=read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/breast-cancer-wisconsin.data",header = F)
summary(BC_data)

############################################################################################
################################ DATA PREPROCESSING ########################################
############################################################################################

## Renaming the columns according to original data set

names(BC_data)[1]="Sample code number"
names(BC_data)[2]="Clump Thickness"
names(BC_data)[3]="Uniformity of Cell Size"
names(BC_data)[4]="Uniformity of Cell Shape"
names(BC_data)[5]="Marginal Adhesion"
names(BC_data)[6]="Single Epithelial Cell Size"
names(BC_data)[7]="Bare Nuclei"
names(BC_data)[8]="Bland Chromatin"
names(BC_data)[9]="Normal Nucleoli"
names(BC_data)[10]="Mitoses"
names(BC_data)[11]="Class_type"

## Since "Bare Nuclei" constains 16 observations with unknown levels indicated by "?", I have
## reordered the levels from 1 through 10 and deleted those missing observations
BC_data$`Bare Nuclei`=factor(BC_data$`Bare Nuclei`,levels = c(1,2,3,4,5,6,7,8,9,10))
ind=which(is.na(BC_data$`Bare Nuclei`))
BC_data=BC_data[-c(ind),]
BC_data$`Bare Nuclei`=as.numeric(BC_data$`Bare Nuclei`)
summary(BC_data)



###########################################################################################
############################### EXPOLRATORY DATA ANALYSIS #################################
###########################################################################################
## Here I have used "Piecharts" to analyze the variables in the data set

## To Analyze Clump Thickenss
quartz()
per=round((table(BC_data$`Clump Thickness`)/683)*100)
lab=paste(names(table(BC_data$`Clump Thickness`)),"[",per,"%]")
pie(table(BC_data$`Clump Thickness`),labels = lab,main = "PIE CHART FOR CLUMP THICKNESS",
    col = rainbow(14),cex=0.6)
## From the pie chart I can infer that 20% of the observations have clump thickness
## of 1,7% have thickness of 2,15% have thickness of 3,12% have thickness of 4,
## 19% have thickness 5,5% have thickness of 6,3% have thickness 7,6% have thickness 8,
## 2% have thickness 9 and the rest 10% have clump thickness  of 10.

## To Analyze Uniformity of Cell Size
quartz()
per1=round((table(BC_data$`Uniformity of Cell Size`)/683)*100)
lab1=paste(names(table(BC_data$`Uniformity of Cell Size`)),"[",per1,"%]")
pie(table(BC_data$`Uniformity of Cell Size`),labels = lab1,main = "PIE CHART FOR UNIFORMITY OF CELL SIZE",
    col = rainbow(14),cex=0.6)
## From the Pie chart I can infer that 55% of the observations have uniformity cell size
## of 1,7% have cell size of 2,8% have cell size 3,6% have cell size 4, 4% have cell size
## cell size 5, 4% have cell size 6,3% have cell size 7,4% have cell size 8,1% have cell
## size 9 and rest 10% have size of 10


## To Analyze Uniformity of Cell Shape
quartz()
per2=round((table(BC_data$`Uniformity of Cell Shape`)/683)*100)
lab2=paste(names(table(BC_data$`Uniformity of Cell Shape`)),"[",per2,"%]")
pie(table(BC_data$`Uniformity of Cell Shape`),labels = lab2,main = "PIE CHART FOR UNIFORMITY OF CELL SHAPE",
    col = rainbow(14),cex=0.6)
## From the Pie chart I can infer that 51% of the observations have uniformity cell Shape
## of 1,8% have cell shape of 2,8% have cell shape of 3,6% have cell shape of 4, 5% have cell 
## cell shape  of 5, 4% have cell shape of 6,4% have cell shape of 7,4% have cell shape of 8,1% have cell
## shape of 9 and rest 8% have uniformity cell shape of 10


## To Analyze Marginal Adhesion
quartz()
per3=round((table(BC_data$`Marginal Adhesion`)/683)*100)
lab3=paste(names(table(BC_data$`Marginal Adhesion`)),"[",per3,"%]")
pie(table(BC_data$`Marginal Adhesion`),labels = lab3,main = "PIE CHART FOR MARGINAL ADHESION",
    col = rainbow(14),cex=0.6)
## From the Pie chart I can infer that 58% of the observations have Marginal Adhesion(MA) of 1,
## 8% have MA of 2,8% have MA of 3,5% have MA of 4, 3% have MA of 5, 3% have MA of 6,
## 2% have MA of 7,4% have MA of 8,1% have MA of 9 and rest 8% have Marginal Adhesion(MA) of 10.


## To Analyze Single Epithelial cell size
quartz()
per4=round((table(BC_data$`Single Epithelial Cell Size`)/683)*100)
lab4=paste(names(table(BC_data$`Single Epithelial Cell Size`)),"[",per4,"%]")
pie(table(BC_data$`Single Epithelial Cell Size`),labels = lab4,main = "PIE CHART FOR SINGLE EPITHELIAL CELL SIZE",
    col = rainbow(14),cex=0.6)
## From the Pie chart I can infer that 6% of the observations have Single Epithelial cell size
## of 1,55% have cell size of 2,10% have cell size of 3,7% have cell size of 4, 6% have cell 
## cell size of 5, 6% have cell size of 6,2% have cell size of 7,3% have cell size of 8,
## and 5% have Single Epithelial cell size of 10.

## To  Analyze Bare Nuclei
quartz()
per5=round((table(BC_data$`Bare Nuclei`)/683)*100)
lab5=paste(names(table(BC_data$`Bare Nuclei`)),"[",per5,"%]")
pie(table(BC_data$`Bare Nuclei`),labels = lab5,main = "PIE CHART FOR BARE NUCLEI",
    col = rainbow(14),cex=0.6)
## From the Piechart, I can infer that 59% of the observation have Bare Nuclei(BN)
## size of 1,4% have BN of 2,4% have BN of 3, 3% have BN of 4,4% have BN of 5,
## 1% have BN of 6,1% have BN of 7,3% have BN of 8,1% have BN of 9 and rest 19%
## have Bare Nuclei(BN) of 10.
 

## To Analyze Bland Chromatin
quartz()
per6=round((table(BC_data$`Bland Chromatin`)/683)*100)
lab6=paste(names(table(BC_data$`Bland Chromatin`)),"[",per6,"%]")
pie(table(BC_data$`Bland Chromatin`),labels = lab6,main = "PIE CHART FOR BLAND CHROMATIN",
    col = rainbow(14),cex=0.6)
## From the Piechart, I can infer that 22% of the observation have Bland Chromatin(BC)
## of 1,23% have BC of 2,24% have BC 3, 6% have BC of 4,5% have BC of 5, 1% have BC of 6,
## 10% have BC of 7,4% have BC of 8, 2% have BC of 9 and rest 3% have Bland Chromatin(BC) of 10


## To Analyze Normal Nucleoli
quartz()
per7=round((table(BC_data$`Normal Nucleoli`)/683)*100)
lab7=paste(names(table(BC_data$`Normal Nucleoli`)),"[",per7,"%]")
pie(table(BC_data$`Normal Nucleoli`),labels = lab7,main = "PIE CHART FOR NORMAL NUCLEOLI",
    col = rainbow(14),cex=0.6)
##From The Piechart, I can infer that 63% of the observations have Normal Nucleoli(NN)
## size of 1, 5% have NN of 2,6% have NN 3,3% have NN of 4,3% have NN of 5, 3% have NN of 6,
## 2% have NN of 7, 3% have NN of 8,2% have NN of 9 and the rest 9% have Normal Nucleoli(NN)
## of size 10

## To Analyze Mitoses
quartz()
per8=round((table(BC_data$Mitoses)/683)*100)
lab8=paste(names(table(BC_data$Mitoses)),"[",per8,"%]")
pie(table(BC_data$Mitoses),labels = lab8,main = "PIE CHART FOR MITOSES",
    col = rainbow(14),cex=0.6)
## From the Piechart, I can infer that 82% of the observations have the Mitoses value of 1,
## while the rest 19% of the observations have other values for mitoses

## To Analyze Class_type(2-Benign and 4 Malignant)
quartz()
per9=round((table(BC_data$Class_type)/683)*100)
lab9=paste(names(table(BC_data$Class_type)),"[",per9,"%]")
pie(table(BC_data$Class_type),labels = lab9,main = "PIE CHART FOR BENIGN(2) & MALIGNANT(4)",
    col = rainbow(14),cex=0.6)
## From the pie chart, I can infer that 65% of the observation are Benign
## and rest 35% is malignant


########################################################################################
############################## SELF ORGANIZING MAPS ####################################
########################################################################################

library(kohonen)
set.seed(666)
BC_data[["Sample code number"]]=NULL

## Scaling the dataset
BC.scale=scale(BC_data)
BC_som_grid=somgrid(xdim = 5,ydim = 5,topo = "hexagonal")
BC_som=som(BC.scale,grid=BC_som_grid,rlen=2000)

quartz()
plot(BC_som,main = "BREAST CANCER DATA",palette.name = rainbow)


quartz()
plot(BC_som,type="changes",main = "CHANGES")


quartz()
plot(BC_som,type="count",main="OBSERVATIONS IN EACH NODE",palette.name = rainbow)

quartz()
plot(BC_som,type="mapping",main="NUMBER OF OBSERVATIONS IN EACH NODE")

## U-Matrix
quartz()
plot(BC_som, type = "dist.neighbours",main = "U-MATRIX",palette.name = rainbow)

code=BC_som$codes[[1]]
##components plot
for (i in 1:10){
  quartz()
  plot(BC_som, type = "property", property=code[,i], main = colnames(code)[i])
}



## Hierarchial clustering
BC_cluster=hclust(dist(code),method = "complete")
quartz()
plot(BC_cluster,main = "DENDROGRAM OF BREAST CANCER DATA")
BC_cluster_cut = cutree(BC_cluster,h=6.5)
rect.hclust(BC_cluster,h=6.5,border = "green")
## performing hierarchical clustering on the codes generated from  Batch SOM analysis
## and cutting the dendro gram at h=6.5 results in two healthy looking clusters 
## representing the Benign and Malignant cancer


## plotting SOM with found clusters
the_col=c("blue","green")
the_bac=the_col[BC_cluster_cut]
quartz()
plot(BC_som, type = "mapping", col = "white", bgcol = the_bac,main = "SOM WITH CLUSTERS:MALIGNANT(blue),BENIGN(Green)")
add.cluster.boundaries(BC_som, BC_cluster_cut)


## In dendrogram,When I make a cut at h=6.5 it results there are two healthy 
## looking clusters corresponding to Benign and Malignant tumors.And plotting
## SOM using the resluts obtained from hierarchial clusters produces a good 
## demarkation between the Malignant(BLUE) and Benign(GREEN)
