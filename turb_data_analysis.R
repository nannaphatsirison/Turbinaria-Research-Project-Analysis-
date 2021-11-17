library(FSA)
#library (dplyr)
#library (ggplot2)

Clod.cards <- read.csv("C:/Users/burne/Dropbox/Turbinaria/Clod cards.csv")

final.data <- read.csv("C:/Users/burne/Dropbox/Turbinaria/final data.csv")


#Remember to import your "clod card" nd "final data" data sets!

## Set up our initial plot window - 2x2 panels - Comment this line out to have individual plots, or just change to par(mfrow=c(1,1))
    par(mfrow=c(1,4))

#Clod card initial analysis
  #Chang column names if necessary
  colnames(Clod.cards)[1]=c("Site") #Nick's computer was reading in the first column name weird. Just specifying this for ease

    #Define x and y variables and axis titles
    x=Clod.cards$Site 
    y=Clod.cards$Weight.loss..grams.
    
    #Make short cuts for adjusting plot axes titles
    y_title= "Weight loss (g)"
    x_title= "Site"
    
            kruskal.test(y~x) #Kruskal-Wallis test
            dunnTest(y~x,method="bonferroni") #Post-hoc dunn's Test with bonferroni p-value adjustments
            dfplot = data.frame(y,x); dfplot$x = factor(dfplot$x, levels=c("Fringing Reef","Back Reef A","Back Reef B"))
            plot(y~as.factor(x),data=dfplot,xlab=x_title,ylab=y_title,ylim=range(8,18)) #Make boxplots
            
            #Finding out median and quantiles 
            tapply(y,x,function(j){quantile(j,0.25)})
            tapply(y,x,function(k){quantile(k,0.75)})
            tapply(y,x,median)

#############
          
#FINAL DATA ANALYSIS
#data= subset(final_data,final_data$Validity==1) #Use this line if your computer called dataset "final_data" vs. "final.data"
            data= subset(final.data,final.data$Validity==1)   #Changing final.data to "data" for ease and using only good data points
         
            #Chang column names if necessary               
              colnames(data)[1]=c("Site")
              colnames(data)[2]=c("BreakingLoad")
              colnames(data)[4]=c("NumberOfBlades")
              colnames(data)[6]=c("PlanformArea")
              
              
            
#Planform Area initial analysis
              
              #Define x and y variables and axis titles
                x=as.factor(data$Site)
                y=pi*(0.008^2)*as.numeric(data$NumberOfBlades)/2
                
              #Make short cuts for adjusting plot axes titles
                y_title= "Planform Area (cm2)"
                x_title= "Site"

                   kruskal.test(y~x) #Kruskal-Wallis test
                   dunnTest(y~x,method="bonferroni") #Post-hoc dunn's Test with bonferroni p-value adjustments
                   dfplot = data.frame(y,x); dfplot$x = factor(dfplot$x, levels=c("Fringing Reef","Back Reef A","Back Reef B"))
                   plot((y*10000)~x,data=dfplot,xlab=x_title,ylab=y_title,ylim=range(20,50)) #Make boxplots
            
                  #Finding out median and quantiles 
                     tapply(y,x,function(j){quantile(j,0.25)})
                    tapply(y,x,function(k){quantile(k,0.75)})
                    tapply(y,x,median)
                    
    #Breaking load
                    
                  #Define x and y variables and axis titles
                    x=as.factor(data$Site)
                    y=as.numeric(data$BreakingLoad)
                    
                  #Make short cuts for adjusting plot axes titles
                    y_title= "Breaking Load (N)"
                    x_title= "Site"
                    
                    kruskal.test(y~x) #Kruskal-Wallis test
                    dunnTest(y~x,method="bonferroni") #Post-hoc dunn's Test with bonferroni p-value adjustments
                    dfplot = data.frame(y,x); dfplot$x = factor(dfplot$x, levels=c("Fringing Reef","Back Reef A","Back Reef B"))
                    plot(y~x,data=dfplot,xlab=x_title,ylab=y_title,ylim=range(0,50)) #Make boxplots
                    
                  #Finding out median and quantiles 
                    tapply(y,x,function(j){quantile(j,0.25)})
                    tapply(y,x,function(k){quantile(k,0.75)})
                    tapply(y,x,median)                    
                    
##
##                    
##            
##
#calculating ESF using range of velocity values 
      df=data.frame() #Make empty data frame to fill with the loop
          for(i in seq(0.5,2.0,0.5)){
           
          velocity=i 
          Cd = 0.15 #From Blanchette (1997) with Fucus gardneri
          Rho = 1025 #Density of seawater (kg/m3)
          Aplan = data$NumberOfBlades*pi*(0.008^2)
          VelocityExp = 1.5 #from Gaylord et al. 2008
          
          drag = 0.5*Cd*Rho*Aplan*velocity^VelocityExp
          ESF= (data$BreakingLoad)/drag
            
            rowsFR= which(data$Site=="Fringing Reef") #Which rows reference Fringing Reef?
              meanFR= mean(ESF[rowsFR])
              sdFR= sd(ESF[rowsFR])
  
          rowsBRA= which(data$Site=="Back Reef A") #Which rows reference Back Reef A?
              meanBRA= mean(ESF[rowsBRA])
              sdBRA= sd(ESF[rowsBRA])
  
          rowsBRB= which(data$Site=="Back Reef B") #Which rows reference Back Reef B?
              meanBRB= mean(ESF[rowsBRB])
              sdBRB= sd(ESF[rowsBRB])
          df=rbind(df,c(velocity,meanFR, sdFR, meanBRA, sdBRA, meanBRB, sdBRB))}
            names(df)= c("velocity", "meanFR", "sdFR", "meanBRA", "sdBRA", "meanBRB", "sdBRB") #Change names of dataframe's columns

          #Filter df just for plotting - Zooming in on region where ESF approaches/passes 1
            df = subset(df,df$velocity>=0.3 & df$velocity<2.5)
            
            
            kruskal.test(c(meanFR,meanBRA,meanBRB))
            
            #Calculate ESF with specific water velocities at appropriate for each site
            Cd = 0.15 #From Blanchette (1997) with Fucus gardneri
            Rho = 1025 #Density of seawater (kg/m3)
            VelocityExp = 1.5 #from Gaylord et al. 2008
            
            dataTemp = subset(data,data$Site=="Fringing Reef")
            velocity = 3.0 #m/s
            Aplan = dataTemp$NumberOfBlades*pi*(0.008^2)/2
            drag = 0.5*Cd*Rho*Aplan*velocity^VelocityExp
            FR_ESF= (dataTemp$BreakingLoad)/drag
            FR_N = NROW(FR_ESF)
            
            dataTemp = subset(data,data$Site=="Back Reef A")
            velocity = 3.6 #m/s
            Aplan = dataTemp$NumberOfBlades*pi*(0.008^2)/2
            drag = 0.5*Cd*Rho*Aplan*velocity^VelocityExp
            BA_ESF= (dataTemp$BreakingLoad)/drag
            BA_N = NROW(BA_ESF)
            
            dataTemp = subset(data,data$Site=="Back Reef B")
            velocity = 4.0 #m/s
            Aplan = dataTemp$NumberOfBlades*pi*(0.008^2)/2
            drag = 0.5*Cd*Rho*Aplan*velocity^VelocityExp
            BB_ESF= (dataTemp$BreakingLoad)/drag
            BB_N = NROW(BB_ESF)
            
            ESF_Vals = c(FR_ESF,BA_ESF,BB_ESF)
            ESF_Labs = c(rep("FR",each=FR_N),rep("BA",each=BA_N),rep("BB",each=BB_N))
            ESF_DF = data.frame(ESF_Labs,ESF_Vals)
            
                ESF_DF$ESF_Labs = factor(ESF_DF$ESF_Labs, c("FR","BA","BB"))
            
            boxplot(ESF_Vals~ESF_Labs,data=ESF_DF,ylim=range(0,40),ylab="ESF",xlab="Site")
            
            kruskal.test(ESF_Vals~ESF_Labs,data=ESF_DF)
            
            tapply(ESF_Vals,ESF_Labs,median)
            
            


