library(caret)
library(forecast)
dir.create("transformation_plots",showWarnings=FALSE)
df<-read.csv("House Price Prediction Dataset.csv")
X<-df[,c("Area","Bedrooms","Bathrooms","Floors","YearBuilt","Location","Condition","Garage")]
y<-df$Price
preProc<-preProcess(X,method=c("center","scale"))
X_scaled<-predict(preProc,X)
transforms<-list(
  none=function(y)y,
  log=function(y)log1p(y),
  sqrt=function(y)sqrt(y),
  boxcox=function(y){
    y_pos<-y+1e-6
    lambda<-BoxCox.lambda(y_pos)
    BoxCox(y_pos,lambda)
  }
)
for(name innames(transforms)){
  y_trans<-transforms[[name]](y)
  model<-lm(y_trans~.,data=as.data.frame(X_scaled))
  y_pred<-predict(model,as.data.frame(X_scaled))
  png(filename=file.path("transformation_plots",paste0(name,"_diagnostics.png")),width=1500,height=400,res=150)
  par(mfrow=c(1,3))
  hist(y_trans,breaks=30,border="black",main=paste(name,"target distribution"),xlab="Transformed Price")
  resid<-y_trans-y_pred
  plot(y_pred,resid,pch=20,main="Residuals vs. Fitted",xlab="Fitted values",ylab="Residuals")
  abline(h=0,lwd=1)
  qqnorm(resid,main="Q–Q plot of residuals")
  qqline(resid,col="black",lwd=1)
  dev.off()
}
