getwd()
setwd("C:/Users/kubra/OneDrive - Aydin Adnan Menderes University/Belgeler/RHomework/RHomework")
data<-read.table("dataset.txt",header = TRUE)
View(data)


#*********************************************************BARPLOT KISMI***********************************************************


female<-0
for(gender in data$Gender){if(gender =="Female"){female<-female+1}}
male<-0                                                         
for(gender in data$Gender){if(gender =="Male"){male<-male+1}}


group1<-0
for(grp in data$Group){if(grp==  "Group1"){group1<-group1+1}}
group2<-0
for(grp in data$Group){if (grp =="Group2"){group2<-group2+1}}
group3<-0                                                           
for(grp in data$Group){if( grp =="Group3"){group3<-group3+1}}
group4<-0
for(grp in data$Group){if (grp =="Group4"){group4<-group4+1}}



writebarplot <- function(data,column,color,title,xlab,ylab,xlim,ylim,plot=TRUE){
  if(plot==TRUE){
    plot.new()
    dev.new()
  } 
  uni = unique(data[column])
  plot(0:ylim,axes = FALSE, type= "n", xlab = xlab, ylab = ylab)
  title(title)
  axis(side = 2,c(0,25,50,ylim))
  for(x in 1:nrow(uni)-1){
    value=0
    do.call("=",list(uni[[1]][x+1], for(values in data[[column]]){if (values==uni[[1]][x+1]){value <- value +1}}))
    rect(x*27,value,x*27+20,0,col=color)
    text(27*x+7,-2,uni[[1]][x+1])}}


writebarplot(data,3,"pink","GENDER","x axis","y axis",0,100)
writebarplot(data,2,"yellow","GROUPS","x axis","y axis",0,100)

twice <- function(){
  plot.new()
  dev.new()
  par(mfcol=c(1,2),mai=c(0.5,0.5,0.5,0.3))
  writebarplot(data,3,"purple","GENDER","xlib","ylib",0,100,FALSE)
  writebarplot(data,2,"green","GROUPS","xlib","ylib",0,100,FALSE)
}
twice()










# ****************************************************HİSTOGRAM PART**********************************************************************



writehist <- function(data,column,color,title,xlab,ylab,xlim,ylim,plot=TRUE){
  if(plot==TRUE){
    plot.new()
    dev.new()
  } 
  plot(0:ylim,axes = FALSE, type= "n", xlab = xlab, ylab = ylab)
  title(title)
  axis(side = 2,c(0,10,20,30,40,50,60,70,80,90,ylim))
  # axis(side = 1,c(0,xlim))
  min = min(data[column])
  max = max(data[column])
  b=0
  i=0
  k=0
  e=0
  r=0
  for(x in 1:nrow(data[column])-1){
    if(data[[column]][x+1]>min & data[[column]][x+1]<=min+(max-min)/8){
      b <- b+1
    }
    else if(data[[column]][x+1]>min+0.12 & data[[column]][x+1]<=min +(max-min)/6){
      i <- i+1
    }
    else if(data[[column]][x+1]>min+0.16 & data[[column]][x+1]<= min +(max-min)/4){
      k <- k+1
    }
    else if(data[[column]][x+1]>min+0.25 & data[[column]][x+1]<= min +(max-min)/2){
      e <- e+1
    }
    else (data[[column]][x+1]>min+0.5 & data[[column]][x+1]<=max)
      r <- r+1
    
  }  
  
  rect(0,b, 20,0,col=color)
  rect(20,i, 40,0,col=color)
  rect(40,k, 60,0,col=color)
  rect(60,e, 80,0,col=color) 
  rect(80,r, 100,0,col=color)
  

  a = sprintf("%.2f - %.2f",min,min+(max-min)/8)
  text(8,15,a,srt=90)
  a = sprintf("%.2f - %.2f",min+(max-min)/8,min+(max-min)/6)
  text(30,15,a,srt=90)
  a = sprintf("%.2f - %.2f",min+(max-min)/6,min+(max-min)/4)
  text(52,15,a,srt=90) 
  a = sprintf("%.2f - %.2f",min+(max-min)/4,min+(max-min)/2)
  text(70,15,a,srt=90)
  a = sprintf("%.2f - %.2f",min+(max-min)/2,max)
  text(85,15,a,srt=90)
  
}


writehist(data,4,"pink","Histogram for Var1","x axis","y axis",120,100)
writehist(data,5,"yellow","Histogram for Var2 ","x axis","y axis",120,100)
writehist(data,6,"brown","Histogram for Var3","x axis","y axis",120,100)
writehist(data,7,"blue","Histogram for Var4 ","x axis","y axis",120,100)
writehist(data,8,"orange","Histogram for Var5 ","x axis","y axis ",120,100)
writehist(data,9,"purple","Histogram for Var6 ","x axis","y axis",120,100)
writehist(data,10,"green","Histogram for Var7 ","x axis","y axis",120,100)
writehist(data,11,"grey","Histogram for Var8 ","x axis","y axis",120,100)

multiple <- function(){
  plot.new()
  dev.new()
  par(mfcol=c(1,8),mai=c(0.3,0.3,0.3,0.3))
  writehist(data,4,"pink","Histogram for Var1","x","y",100,100,FALSE)
  writehist(data,5,"yellow","Histogram for Var2","x","y",100,100,FALSE)
  writehist(data,6,"brown","Histogram for Var3","x","y",100,100,FALSE)
  writehist(data,7,"blue","Histogram for Var4","x","y",100,100,FALSE)
  writehist(data,8,"orange","Histogram for Var5 ","x axis","y axis ",120,100,FALSE)
  writehist(data,9,"purple","Histogram for Var6 ","x axis","y axis",120,100,FALSE)
  writehist(data,10,"orange","Histogram for Var7","x","y",100,100,FALSE)
  writehist(data,11,"grey","Histogram for Var8 ","x axis","y axis",120,100,FALSE)
}
multiple()




















#************************BAXPLOT FUNCTIONS***********************************






sumfunc = function(x, na.rm = FALSE) {
  my_sum = 0
  for (i in seq_along(x)) {my_sum=x[i]+my_sum                                                                        }
  return(my_sum)
}

sumfunc(na.omit(data$Var1))


meanfunc = function(x, na.rm = FALSE) {
  my_average = sumfunc(x)/length(x) 
  return(my_average)
}

meanfunc(na.omit(data$Var1))



medianfunc = function(x, na.rm = FALSE) {
  a <- length(x)
  b <- sort(x)
  ifelse(a%%2==1,b[(a+1)/2],meanfunc(b[a/2+0:1]))
}

medianfunc(na.omit(data$Var1))



SumOfSquaresfunc = function(x){
  
  average <- meanfunc(x)
  difference <- average - x  
  difference_squared <- difference^2
  sum_squares <- sumfunc(difference_squared , na.rm = TRUE)
  output <- sum_squares
  output
}

SumOfSquaresfunc(na.omit(data$Var1))

varfunc = function(x){
  variance = SumOfSquaresfunc(x)/(length(x)-1) 
  variance
}

varfunc(na.omit(data$Var1))




sdfunc = function(x){
  sqrt(varfunc(x))
}

sdfunc(na.omit(data$Var1))



































