library(pxweb)
library(tidyverse)
library(plotly)

#this is another example of using the UNECE API query to download a few different datasets, combine them and create an interactive plotly bubble graph
#the next line can be run to create code for any query you like
#pxweb_interactive('w3.unece.org')
####gdp per capita####
pxweb_query_gdp <- 
  list("Expenditure"=c("ME.1041"),
       "Measurement"=c("c0000719"),
       "Country/Region"=c("008","020","051","040","031","112","056","070","100","124","191","196","203","208","233","246","250","268","276","300","348","352","372","376","380","398","417","428","438","440","442","470","492","499","528","807","578","616","620","498","642","643","674","688","703","705","724","752","756","762","792","795","804","826","840","860"),
       "Year"=c("3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30"))

px_data_gdp <- 
  pxweb_get(url = "https://w3.unece.org/PXWeb2015/api/v1/en/STAT/20-ME/2-MENA/01_en_MECCGDPExpPerCapY_r.px",
            query = pxweb_query_gdp)
px_data_gdp <- as.data.frame(px_data_gdp, column.name.type = "text", variable.value.type = "text") %>%
  na.omit() %>%
  select("Country/Region",Year,'US$, at prices and PPPs of 2010')
colnames(px_data_gdp)<-c("Country","Year", "GDPconstant")

ggplot(px_data_gdp)+
aes(x=Year,y=GDPconstant,color=Country,group=Country)+
  geom_line()

####deaths per capita####
pxweb_query_list_death <- 
  list("Topic"=c("TR.14"),
       "Measurements"=c("c0001210"),
       "Country"=c("008","020","051","040","031","112","056","070","100","124","191","196","203","208","233","246","250","268","276","300","348","352","372","376","380","398","417","428","438","440","442","470","492","499","528","807","578","616","620","498","642","643","674","688","703","705","724","752","756","762","792","795","804","826","840","860"),
       "Year"=c("0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27"))
px_data_deaths <- 
  pxweb_get(url = "https://w3.unece.org/PXWeb2015/api/v1/en/STAT/40-TRTRANS/01-TRACCIDENTS/01_en_TRAccTotal_r.px",
            query = pxweb_query_list_death)
px_data_frame_deaths <- as.data.frame(px_data_deaths, column.name.type = "text", variable.value.type = "text") %>%
  na.omit() %>%
  select("Country","Year","Per million inhabitants")
colnames(px_data_frame_deaths)<-c("Country","Year","Deaths")

ggplot(px_data_frame_deaths)+
  aes(x=Year,y=Deaths,color=Country,group=Country)+
  geom_line()
####population####
pxweb_query_pop <- 
  list("Indicator"=c("c0000209"),
       "Country"=c("008","020","051","040","031","112","056","070","100","124","191","196","203","208","233","246","250","268","276","300","348","352","372","376","380","398","417","428","438","440","442","470","492","499","528","807","578","616","620","498","642","643","674","688","703","705","724","752","756","762","792","795","804","826","840","860"),
       "Year"=c("3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30"))

# Download data 
px_datapop <- 
  pxweb_get(url = "https://w3.unece.org/PXWeb2015/api/v1/en/STAT/10-CountryOverviews/01-Figures/ZZZ_en_CoSummary_r.px",
            query = pxweb_query_pop)

# Convert to data.frame 
px_data_pop <- as.data.frame(px_datapop, column.name.type = "text", variable.value.type = "text") %>%
  na.omit()
colnames(px_data_pop)<-c("Country","Year","Population")

ggplot(px_data_pop)+
  aes(x=Year,y=Population,color=Country,group=Country)+
  geom_line()

####combine it all up####
combined<-left_join(px_data_gdp,px_data_frame_deaths,by=c("Country","Year")) %>%
  left_join(px_data_pop)%>%
  filter(Year>1999&Year<2020) %>%
  #filter out incomplete countries
  filter(Country!="Monaco"&Country!="Liechtenstein"&Country!="San Marino"&Country!="Andorra"&Country!="Bosnia and Herzegovina"&Country!="Montenegro"&Country!="Republic of Moldova"&Country!="Russian Federation"&Country!="Turkmenistan"&Country!="Ukraine"&Country!="United Kingdom") %>%
  na.omit() 

#now create a ggplot and pass it to plotly
a<-ggplot(combined,aes(x=GDPconstant,y=Deaths,size=Population*3,color=Country,text=Country,alpha=0.4))+
  geom_point(aes(frame=Year))+
  theme(legend.position = "none")+
  xlab("GDP per capita %(in 2010 USD using PPPs%)")+
  ylab("Road fatalities per million inhabitants")
ggplotly(a,tooltip = "text")

