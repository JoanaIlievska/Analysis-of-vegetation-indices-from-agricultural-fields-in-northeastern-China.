install.packages('readxl')
library("readxl") 
excel_sheets("C:/Users/Lenovo/Desktop/FINKI/2/VVEI/seminarska/201032.xlsx")
 
LAI<-read_excel("C:/Users/Lenovo/Desktop/FINKI/2/VVEI/seminarska/201032.xlsx", sheet="LAI_RS")
View(LAI)
GAI<-read_excel("C:/Users/Lenovo/Desktop/FINKI/2/VVEI/seminarska/201032.xlsx", sheet="GAI_Dest")
YAI<-read_excel("C:/Users/Lenovo/Desktop/FINKI/2/VVEI/seminarska/201032.xlsx", sheet="YAI_Dest")
PAI<-read_excel("C:/Users/Lenovo/Desktop/FINKI/2/VVEI/seminarska/201032.xlsx", sheet="PAI_Dest")
FC<-read_excel("C:/Users/Lenovo/Desktop/FINKI/2/VVEI/seminarska/201032.xlsx", sheet="FCOVER_LAI2200")
 View(GAI)
 View(LAI)
 View(YAI)
 View(FC)

 #sekoj list od excel tabelata e smesten vo soodvetna tabela spored imeto na indeksot.
 
 
 str(LAI)
 str(GAI)
 str(YAI)
 str(FC)
 
 #stuktura na sekoj od podatocnite elementi 
 
 prosek<-LAI[, 6] # prosekot na LAI indeksot
 std<- LAI[, 7] #standardna devijacija na km
 View(prosek)
 View(std)
 
 
 
 FC$Mean...6 <- factor(FC$Mean...6)
 FC$Std...7 <- factor(FC$Std...7)
 summary(FC)
 nlevels(FC$Mean...6)
 
 LAI$Mean...6 <- factor(LAI$Mean...6)
 LAI$Date...5 <- factor(LAI$Date...5)
 summary(LAI)
 nlevels(LAI$Mean...6)
 
 GAI$Mean...6 <- factor(GAI$Mean...6)
 GAI$Date...5<- factor(GAI$Date...5)
 summary(GAI)
 nlevels(GAI$Mean...6)
 
 YAI$Mean...6 <- factor(YAI$Mean...6)
 YAI$Std...7 <- factor(YAI$Std...7)
 summary(YAI)
 nlevels(YAI$Mean...6)
 
 #koristenje na factor i sumarizacija
 
 
 summary(LAI$`Mean(3kmx3km)...12`) ; summary(GAI$Mean...102); summary(YAI$Mean...102); summary(PAI$Mean...6); summary(prosek)

 filter(LAI, Date...5 == "2012/06/19")  #filter na odreden datum vo tabelite
 
 library(dplyr)
 
   prosek %>%
   mutate(prosek = prosek * 100) %>% #pretvaranje vo procenti so pomosh na piping 
   head()

   
    std %>%
    mutate(std = mean(std)) %>% #pretvaranje vo procenti so pomosh na piping 
    head()
 
    Paicomplete <- PAI %>%
      filter(!is.na(`Mean(3kmx3km)...6`),         
             !is.na(Date...5),  
             !is.na(`Std(1km)...7`))  
 
    LAI %>%
    filter(!is.na(Lat_cent)) %>%
    mutate(Lat_cent = mean(Lat_cent)) %>%
    head()
    
    
     LAI_vo  <- LAI %>%
    filter(!is.na(`Mean(3kmx3km)...6`)) %>%
    mutate( `Mean(3kmx3km)...6` * 100) %>%
    select(`Mean(3kmx3km)...6`) 
     
     
      GAI %>%
       group_by(Mean...6, Std...7) %>%
       summarize(sredna = mean(Mean...6, na.rm = TRUE)) %>%
       tail()
      
      
        GAI %>%
        filter(!is.na(Mean...6)) %>% #piping
        group_by(Mean...6) %>%
        summarize(mean = (Mean...6),
                 min_ = min(Mean...6))
        
        
        
        PAI %>%
          count(Date...5, sort = TRUE)
        GAI %>%   #broenje so piping 
          count(Date...5, sort = TRUE)
     
        
        
        LAIcomplete <- LAI %>%
          filter(!is.na(`Mean(3kmx3km)...6`),         
                 !is.na(Date...5),  
                 !is.na(`Std(1km)...7`))  
        
        
        GAIcomplete <- GAI %>%
          filter(!is.na(`Mean(3kmx3km)...6`),         
                 !is.na(Date...5),  
                 !is.na(`Std(1km)...7`))  
        
        
        
        library(ggplot2)
        ggplot(data = LAI)     

     
        
        ggplot(data = FC, mapping = aes(x = Date...5, y = Mean...6)) +
          geom_boxplot(alpha = 0) +
          geom_jitter(alpha = 0.3, color = "tomato")
        
        
        
        ggplot(data = GAI, mapping = aes(x = Date...5, y = Mean...6)) +
          geom_boxplot(alpha = 0) +
          geom_jitter(alpha = 0.3, color = "magenta")
        
        
        ggplot(data = YAI, mapping = aes(x = Date...5, y = Mean...6)) +
          geom_boxplot(alpha = 1) +
          geom_jitter(alpha = 0.9, color = "tomato")
        
        
        
        ggplot(data = PAI, mapping = aes(x = Date...5, y = Mean...6)) +
          geom_boxplot(alpha = 0.5) +
          geom_jitter(alpha = 0.7, color = "red")
        
        
        
        
        ggplot(data = GAI, aes(x = Date...5, y = Mean...6)) +
          geom_line()
        
        
     