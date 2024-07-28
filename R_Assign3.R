library(lessR)
library(highcharter)
library(dplyr)
library(car)
library(visdat)
library(lubridate)
library(gridExtra)

# timeseries
library(forecast)
library(tseries)
library(fpp2)
library(lmtest)
library(tidyr)
library(padr)
library(imputeTS)
# heatmap
library(ggplot2)
library(sf)
library(rvest)
library(plotly)
library(viridis)
library(ggrepel)
library(ggthemes)

#cowplot
library(cowplot)
#WordCloud
library(ggwordcloud)

conflict= read.csv('C:/Users/12par/OneDrive/Desktop/isr_pse_conflict_20_copy.csv')

############################ Data Cleaning- Manipulation #####################################
#Converting blank values to NA values

conflict[conflict == ''] <- NA
  #View(conflict)
vis_miss(conflict)
#na_count <- sum(is.na(conflict))
#print(na_count)
#colSums(is.na(conflict))
#rowSums(is.na(conflict))

conflict <- conflict %>%
  select(-ammunition, -took_part_in_the_hostilities)

#colSums(is.na(data1))

conflict <- conflict[complete.cases(conflict), ]
#View(data_clean)
#na_count <- sum(is.na(data_clean))
#print(na_count)

vis_miss(conflict)
#Total values omitted  457

#Convert date format from dmy to Y
df <- conflict %>% 
  mutate(NEW_FORMAT = parse_date_time(date_of_event, 
                                      orders = c("mdy", "dmy", "ymd"))) %>% 
  mutate(DESIRED_FORMAT = format.Date(NEW_FORMAT,  "%Y"))

  #View(df)


# Month Year format
df2 <- df %>% 
  mutate(NEW_FORMAT = parse_date_time(date_of_event, 
                                      orders = c("mdy", "dmy", "ymd"))) %>% 
  mutate(DESIRED_FORMAT2 = format.Date(NEW_FORMAT,  "%b %Y"))

  #View(df2)




# Spliting age into age groups
df3= df2 %>% 
  mutate(
    # Create categories
    age_group = dplyr::case_when(
      age <= 18            ~ "0-18",
      age > 18 & age <= 36 ~ "19-36",
      age > 36 & age <= 54 ~ "37-54",
      age > 54             ~ "> 54" 
      ) ,
    # Convert to factor
    age_group = factor(
      age_group,
      level = c("0-18", "19-36","37-54", "> 54")
    )
  )
  #View(df3)

# Omitting the NA valued data rows
df_clean= select(df3,-c(NEW_FORMAT))
  #View(df_clean)   
  #vis_miss(df_clean)

# Exporting a clean excel file for future reference

 #library(writexl)
  #write_xlsx( df_clean,"C:/Users/12par/Downloads/maindf.xlsx")

################################ Data Analysis #################################

custom_colors <- c("#454568", "#785956")

df_clean%>%
  group_by(gender) %>%
  summarise(count=n()) %>%
  # arrange(desc(Rating)) %>%  # Sort by count in descending order
  # head(5) %>%  
  hchart('pie', hcaes(x = gender, y = count, color = custom_colors)) %>%
  hc_add_theme(hc_theme_google()) %>%
  hc_tooltip(pointFormat = '<b> Average Rating </b>: {point.y} <br>') %>%  
  hc_title(text = "Genderwise Count")

df_clean%>%
  group_by(age_group) %>%
  summarise(count=n()) %>%
  # arrange(desc(Rating)) %>%  # Sort by count in descending order
  # head(5) %>%  
  hchart('column', hcaes(x = age_group, y = count, color = count)) %>%
  hc_add_theme(hc_theme_google()) %>%
  hc_tooltip(pointFormat = '<b> Average Rating </b>: {point.y} <br>') %>%  
  hc_title(text = "Age Groups")


custom_colors <- c("#005EB8", "#000000")

df_clean%>%
  group_by(citizenship) %>%
  summarise(count=n()) %>%
  # arrange(desc(Rating)) %>%  # Sort by count in descending order
  # head(5) %>%  
  hchart('column', hcaes(x = citizenship, y = count, color = custom_colors)) %>%
  hc_add_theme(hc_theme_google()) %>%
  hc_tooltip(pointFormat = '<b> Average Rating </b>: {point.y} <br>') %>%  
  hc_title(text = "Countrywise Citizenship Count")

custom_colors <- c("#EE2A35","#005EB8", "#000000")

df_clean%>%
  group_by(killed_by) %>%
  summarise(count=n()) %>%
  # arrange(desc(Rating)) %>%  # Sort by count in descending order
  # head(5) %>%  
  hchart('column', hcaes(x = killed_by, y = count, color = custom_colors)) %>%
  hc_add_theme(hc_theme_google()) %>%
  hc_tooltip(pointFormat = '<b> Average Rating </b>: {point.y} <br>') %>%  
  hc_title(text = "Killed By Counts")


df_clean%>%
  group_by(DESIRED_FORMAT) %>%
  summarise(count=n()) %>%
  # arrange(desc(Rating)) %>%  # Sort by count in descending order
  # head(5) %>%  
  hchart('column', hcaes(x = DESIRED_FORMAT, y = count, color = count)) %>%
  hc_add_theme(hc_theme_google()) %>%
  hc_tooltip(pointFormat = '<b> Average Rating </b>: {point.y} <br>') %>%  
  hc_title(text = "Year Wise Count of Deaths")


df_clean%>%
  group_by(type_of_injury) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%  
  head(5) %>%  
  hchart('pie', hcaes(x =type_of_injury, y = count, color = count)) %>%
  hc_add_theme(hc_theme_google()) %>%
  hc_tooltip(pointFormat = '<b> Count </b>: {point.y} <br>') %>%  
  hc_title(text = "Type of Injuries")

set.seed(1000)
w = df_clean %>% group_by(type_of_injury) %>%
  summarize(count = n())
#View(w)
ggplot(w, aes(label = w$type_of_injury, size = w$count, color= w$count)) +
  geom_text_wordcloud() +
  scale_size_area(max_size = 50) +
  theme_minimal() +
  scale_color_gradient(low = "darkred", high = "maroon")


################################## HEAT-MAP ####################################
#Importing Shape Files
is.sh1= read_sf('C:\\Users\\12par\\Downloads\\ISR_adm\\ISR_adm1.shp')
pa.sh1= read_sf('C:\\Users\\12par\\Downloads\\PSE_adm\\PSE_adm2.shp')
#View(is.sh1)
#View(pa.sh1)
# Changing Pivot table column names to shape file column names


###PALESTINE DEATHS

#                 Total Palestine Deaths
prw=  df_clean %>% group_by(event_location_region, event_location_district) %>%
  summarize(count = n())

#View(prw)

#colnames(prw)[1] <- "NAME_1"
colnames(prw)[2] <- "NAME_2"
#View(prw)

#Merging Palestine shape file with Pivot table
map.prw= merge(pa.sh1, prw, by= "NAME_2" )
#View(map.prw)

points.prw= cbind(map.prw, st_coordinates(st_centroid(map.prw$geometry)))



#               Israeli Citizens Dead in Palestine
pa.icp = df_clean %>% group_by(df_clean$citizenship,
                               df_clean$event_location_district) %>%
  filter(citizenship =="Israeli") %>%  summarize(count = n())
#View(pa.icp)

#colnames(pa.deaths)[1] <- "NAME_0"
colnames(pa.icp)[2] <- "NAME_2"
#View(pa.icp)

map.icp = merge(pa.sh1, pa.icp, by= "NAME_2", )

#                 Palestinian Citizens Dead in Palestine
pa.pcp = df_clean %>% group_by(df_clean$citizenship,
                               df_clean$event_location_district) %>%
  filter(citizenship =="Palestina") %>%  summarize(count = n())
#View(pa.pcp)

#colnames(pa.deaths)[1] <- "NAME_0"
colnames(pa.pcp)[2] <- "NAME_2"
#View(pa.pcp)

map.pcp= merge(pa.sh1, pa.pcp, by= "NAME_2")


info= paste0(
  '\nName:', map.prw$NAME_2,
  '\nTotal Death Count: ', map.prw$count,
  '\nPalestinians dead in Palestine : ', map.pcp$count,
  '\nIsraelis dead in Palestine : ', map.icp$count
)
map.prw$info = info

pa.pd= ggplot(map.prw)+ geom_sf(aes(fill= map.prw$count, label= info),
                                color= 'white', size= 5)+
  ggtitle("Deaths in Palestine")+
  geom_text(data= points.prw, aes(x= points.prw$X, y= points.prw$Y, 
                                  label= paste(points.prw$NAME_2)),
            color= 'Red', vjust= -2)+
  scale_fill_viridis_c(option = 'A', trans= 'sqrt')+
  labs(x = "Longitude", y = "Latitude") 
    

fig= ggplotly(pa.pd) %>%
  layout(autosize = F, width = 500, height = 500)
fig

# ISRAEL DEATHS
#                  Total Israel Deaths
irw =  df_clean %>% group_by(event_location,
                             event_location_region) %>% 
  filter(event_location_region == "Israel") %>%
  summarize(count = n())
#View(irw)

colnames(irw)[1] <- "NAME_1"
#View(irw)

map.irw= merge(is.sh1, irw, by= "NAME_1")
#View(map.irw)

points.irw= cbind(map.irw, st_coordinates(st_centroid(map.irw$geometry)))
#View(points.irw)

#                         Israeli citizens dead in Israel
is.ici = df_clean %>% group_by(
  df_clean$event_location,
  df_clean$event_location_region,
  df_clean$citizenship) %>%
  filter( event_location_region =='Israel', citizenship== 'Israeli') %>%  
  summarize(count = n())
#View(is.ici)

colnames(is.ici)[1] <- "NAME_1"
#View(is.ici)

map.ici= merge(is.sh1, is.ici, by= "NAME_1")
#View(map.ici)

#                  Palestine Citizens Dead in Israel

is.pci = df_clean %>% group_by(
  df_clean$event_location,
  df_clean$event_location_region,
  df_clean$citizenship) %>%
  filter( event_location_region =='Israel', citizenship== 'Palestina') %>%  
  summarize(count = n())
#View(is.pci)

colnames(is.pci)[1] <- "NAME_1"
#View(is.pci)

map.pci= merge(is.sh1, is.pci, by= "NAME_1")
#View(map.pci)

info1= paste0(
  '\nName: ', map.irw$NAME_1,
  '\nTotal Death Count: ', map.irw$count,
  '\nIsraeli dead in Israel: ', map.ici$count,
  '\nPalestinians dead in Israel: ', map.pci$count
)
map.irw$info1 = info1


itd = ggplot(map.irw)+ geom_sf(aes(fill= map.irw$count, label= info1), 
                               color= 'white', size= 5)+
  scale_fill_viridis(option = 'B', trans= 'sqrt')+
  ggtitle("Israeli Deaths")+
  geom_text(data= points.irw, aes(x= points.irw$X, y= points.irw$Y, 
                                  label= paste(points.irw$NAME_1)),
            nudge_x = c(0, -0.4, 0.6, 0, 0.3, -0.4), nudge_y = c(0, 0, 0,0, 0,0),
            color= 'Red')+
  labs(x = "Longitude", y = "Latitude")+
  theme(
    panel.background = element_rect(fill = 'grey', size= 0.5,
                                    linetype = 0, color = 'black'),
  )



ggplotly(itd)

################################ time series ###################################
#install.packages('tseries')
#library(tseries)

yrwise= df_clean %>% group_by(DESIRED_FORMAT2) %>%
  mutate(DESIRED_FORMAT2 = as.Date(paste0(DESIRED_FORMAT2, " 01"), format = "%b %Y %d")) %>%
  arrange(DESIRED_FORMAT2)   %>%
  #arrange(month(DESIRED_FORMAT2)) %>% 
  summarize(count = n())
#View(yrwise)


# Find the minimum and maximum dates in yrwise$DESIRED_FORMAT2
min_date <- min(yrwise$DESIRED_FORMAT2)
max_date <- max(yrwise$DESIRED_FORMAT2)

# Create a sequence of complete monthly dates
complete_dates <- seq(from = min_date, to = max_date, by = "month")
complete_dates_df <- data.frame(DESIRED_FORMAT2 = complete_dates)

# Merge yrwise with the complete sequence of dates
yrwise <- merge(yrwise, complete_dates_df, by = "DESIRED_FORMAT2", all = TRUE)

#View(yrwise)

yrwise <- na_kalman(yrwise) 
#View(yrwise)

# plotting the graph
mts <- ts(yrwise$count, start = c(2000,10), end = c(2023,09), frequency = 12)
#mts

p1= ts.plot(mts, main = "Time Series Plot", xlab = "Time", ylab = "Count of Deaths")

abline(reg=lm(mts~time(mts)))

adf.test(mts)

arima_model=auto.arima(mts)

print(arima_model)


par(mfrow=c(2,1))
acf_result= acf(mts, lag.max = 12, main = "Autocorrelation Function", 
                xlab = "Lag", ylab = "ACF")
pacf_result= pacf(mts, lag.max = 12, main = "Partial Autocorrelation Function",
                  xlab = "Lag", ylab = "PACF")

par(mfrow=c(1,1))
forecast_values <- forecast(mts, h=12)
#par(mfrow=c(1,1))
plot(forecast_values, main="ARIMA Forecast",
     xlab="Time", ylab="Count of Deaths")



############### Rough work 

#map$count <- as.numeric(cut_number(map$count,5))

#Method 1

#View(is.sh1)
#p1= ggplot(is.sh1)
#p1= p1+ geom_sf(aes(fill= NAME_1))
#p1= p1+ ggthemes::theme_map()
#p1= p1+ labs(title= 'Israel')
#p1


#p2= ggplot(map.pa)
#p2= p2+ geom_sf(aes(fill= map.pa$count))
#p2= p2+ ggthemes::theme_map() +
# theme(legend.key.height= unit(1, 'cm'),
#      legend.key.width= unit(1.25, 'cm'), legend.position = "top")
#p2= p2+ labs(title= 'Palestine')

#p2

#Method 3

#library(cartography)
#plot(st_geometry(map.pa))
#choroLayer(x= map.pa, var = "count", method = "quantile",
#          nclass = 3)
#layoutLayer(title= 'Palestina deaths', tabtitle = T,frame = T, 
#           scale = 5)

 

#1

#aged = df_clean %>% group_by(df_clean$age_group,
  #                           df_clean$citizenship) %>%
#  summarize(count = n())
#
#ggplot(data=aged, aes(x=aged$`df_clean$age_group` , y= aged$count, fill= aged$`df_clean$citizenship`)) +
 # geom_bar(stat="identity", color="black",)+
  #geom_text(aes(label= aged$count), vjust=0, color="black", size=3.5)+theme_classic()


#2
  
#yrwise= df_clean %>% group_by(DESIRED_FORMAT) %>%
 # summarize(count = n())
#yrwise

#ggplot(data=yrwise, aes(x=yrwise$DESIRED_FORMAT, y=yrwise$count)) +
 # geom_bar(stat="identity", color="green", fill="red")+ 
  #geom_text(aes(label=yrwise$count), vjust=0, color="black", size=3.7)+theme_classic()




#View(df_clean)
#ag= df_clean$age_group
#gen= df_clean$gender
#cit= df_clean$citizenship

#inj= df_clean$type_of_injury

#mon= df_clean$DESIRED_FORMAT
#x = df_clean$killed_by


#PieChart(gen, values = "input", main = "Pie Chart of Gender")
#BarChart(ag, by= cit, beside = T,
        # values = "input", values_position = "out", legend_title = "Citizenship")
#BarChart(mon, values = "input",values_position = "out", fill = "red")

#BarChart(x, values = "input", values_position = "out", eval_df = F)

