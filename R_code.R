library(highcharter)
library(tidyverse)
library(downloader)
library(rgdal)
library(sf)
library(ggplot2)
library(reshape2)
library(plotly)
library(rgdal)
library(stringr)
library(tmap)
library(readxl)
library(tidyr)
library(devtools)
library(hrbrthemes)
library(RColorBrewer)
library(janitor)
library(car)
library(geojsonio)
library(mapview)
library(crosstalk)
library(gridExtra)
#library(gstat)#
library(here)
library(fs)
library(kernlab)
library(maptools)
library(lattice)
library(caret)
library(scales)
library(sp)
library(spdep)
library(fs)
library(nortest)
library(broom)
library(ggpubr)
library(tidypredict)
library(spatialreg)
#library(GWmodel)#
library(metaforest)
library(randomForest)
library(devtools)
library("Metrics")
library(nnet) #install if necessary
library(caret)

##read lsoa boundary data
Bristollsoas<-dir_info(here::here("code", "bristol_lsoa"))%>%
  filter(str_detect(path, "lsoa110.shp$"))%>%
  select(path)%>%
  pull()%>%
  st_read()
qtm(Bristollsoas)

Camdenlsoas<-dir_info(here::here("code", "camden_lsoa"))%>%
  filter(str_detect(path, "camden.shp$"))%>%
  select(path)%>%
  pull()%>%
  st_read()
qtm(Camdenlsoas)

Birminghamlsoas<-dir_info(here::here("code", "birmingham_lsoa"))%>%
  filter(str_detect(path, "Birmingham.shp$"))%>%
  select(path)%>%
  pull()%>%
  st_read()
qtm(Birminghamlsoas)


####read attribute data
bristol_attribute_data <- read_excel(here::here("code","bristol_merge.xlsx"),na = c("N/A"), col_names = TRUE)
camden_attribute_data <- read_excel(here::here("code","camden_merge.xlsx"),na = c("N/A"), col_names = TRUE)
birmingham_attribute_data <- read_excel(here::here("code","birmingham_merge.xlsx"),na = c("N/A"), col_names = TRUE)

##check type of attribute data
Typelist_bristol <- bristol_attribute_data %>% 
  summarise_all(class) %>%
  pivot_longer(everything(), 
               names_to="Features", 
               values_to="Feature_type")

Typelist_camden <- camden_attribute_data %>% 
  summarise_all(class) %>%
  pivot_longer(everything(), 
               names_to="Features", 
               values_to="Feature_type")

Typelist_birmingham <- birmingham_attribute_data %>% 
  summarise_all(class) %>%
  pivot_longer(everything(), 
               names_to="Features", 
               values_to="Feature_type")

##merge boundaries and geo data
joined_data_bristol <- Bristollsoas%>%
  right_join(.,
            bristol_attribute_data, 
            by = c("lsoa11cd" = "lsoa_code"))

joined_data_camden <- Camdenlsoas%>%
  right_join(.,
            camden_attribute_data, 
            by = c("LSOA11CD" = "lsoa_code"))

joined_data_birmingham <- Birminghamlsoas%>%
  right_join(.,
             birmingham_attribute_data, 
             by = c("lsoa_code" = "lsoa_code"))

#tmap_mode("view")
bristolbb <- st_bbox(joined_data_bristol,
                    crs = st_crs(joined_data_bristol)) %>% 
  st_as_sfc()

camdenbb <- st_bbox(joined_data_camden,
                     crs = st_crs(joined_data_camden)) %>% 
  st_as_sfc()

birminghambb <- st_bbox(joined_data_birmingham,
                    crs = st_crs(joined_data_birmingham)) %>% 
  st_as_sfc()

## mapping 
####read Bristol map data
bristol_map_total_data <- read_excel(here::here("code","bristol_group_total.xlsx"),na = c("N/A"), col_names = TRUE)
bristol_map_unit_data <- read_excel(here::here("code","bristol_group_unit.xlsx"),na = c("N/A"), col_names = TRUE)

bristol_map_total_data <- Bristollsoas%>%
  right_join(.,
             bristol_map_total_data, 
             by = c("lsoa11cd" = "lsoa_code"))

bristol_map_unit_data <- Bristollsoas%>%
  right_join(.,
             bristol_map_unit_data, 
             by = c("lsoa11cd" = "lsoa_code"))

tm_shape(bristol_map_total_data) + 
  tm_polygons(col='total_price') 

#bristol unit
bristol_map_unit_data$`unit price` = bristol_map_unit_data$unit_price
bristol_unit_price_map <- tm_shape(bristol_map_unit_data, bbbox = bristolbb) + 
  tm_polygons("unit price",
              breaks = c(1000,2000,3000,4000,5000,6000,7000),
              palette="-RdYlBu",title="unit price (£)")+
  tm_scale_bar(position = c("right", "bottom"), text.size = .8)+
  tm_layout(
    legend.position = c(0.8,0.3), 
    legend.text.size=.8, 
    legend.title.size = 1.1,
    main.title = "Average unit house price in Bristol(LSOA)",
    main.title.position = "center",
    main.title.color = "black",
    main.title.size = 1.2,
    frame=FALSE)+
  tm_compass(north=0, position = c(0.8, 0.15),size =3) +
  #bottom left top right
  tm_layout(inner.margin=c(0.02,0.02,0.02,0.2))

bristol_unit_price_map

## mapping 
#bristol total
bristol_map_total_data$`total price` = bristol_map_total_data$total_price
bristol_total_price_map <- tm_shape(bristol_map_total_data, bbbox = bristolbb) + 
  tm_polygons("total price",
              breaks = c(0,200000,400000,600000,800000,1000000),
              palette="-RdYlBu",title="total price (£)")+
  tm_scale_bar(position = c("right", "bottom"), text.size = .8)+
  tm_layout(
    legend.position = c(0.8,0.3), 
    legend.text.size=.8, 
    legend.title.size = 1.1,
    main.title = "Average total house price in Bristol(LSOA)",
    main.title.position = "center",
    main.title.color = "black",
    main.title.size = 1.2,
    frame=FALSE)+
  tm_compass(north=0, position = c(0.8, 0.15),size =3) +
  #bottom left top right
  tm_layout(inner.margin=c(0.02,0.02,0.02,0.2))

bristol_total_price_map
  
## mapping 
####read Bristol map data
camden_map_total_data <- read_excel(here::here("code","camden_group_total.xlsx"),na = c("N/A"), col_names = TRUE)
camden_map_unit_data <- read_excel(here::here("code","camden_group_unit.xlsx"),na = c("N/A"), col_names = TRUE)

camden_map_total_data <- Camdenlsoas%>%
  right_join(.,
             camden_map_total_data, 
             by = c("LSOA11CD" = "lsoa_code"))

camden_map_unit_data <- Camdenlsoas%>%
  right_join(.,
             camden_map_unit_data, 
             by = c("LSOA11CD" = "lsoa_code"))

tm_shape(camden_map_total_data) + 
  tm_polygons(col='total_price') +
  tm_layout(legend.format = list(big.num.abbr = NA))

#camden unit
camden_map_unit_data$`unit price` = camden_map_unit_data$unit_price
camden_unit_price_map <- tm_shape(camden_map_unit_data, bbbox = camdenbb) + 
  tm_polygons("unit price",
              breaks = c(0,5000,10000,15000,20000,25000),
              palette="-RdYlBu",title="unit price (£)")+
  tm_scale_bar(position = c(0.695,0), text.size = .8)+
  tm_layout(
    legend.position = c(0.75,0.3), 
    legend.text.size=.8, 
    legend.title.size = 1.1,
    main.title = "Average unit house price in Camden(LSOA)",
    main.title.position = "center",
    main.title.color = "black",
    main.title.size = 1.2,
    frame=FALSE)+
  #tm_text("LSOA11CD", size = 1/2)+
  tm_compass(north=0, position = c(0.8, 0.12),size =3) +
  #bottom left top right
  tm_layout(inner.margin=c(0.02,0.02,0.02,0.2))

camden_unit_price_map

#camden total
camden_map_total_data$`total price` = camden_map_total_data$total_price
camden_total_price_map <- tm_shape(camden_map_total_data, bbbox = camdenbb) + 
  tm_polygons("total price",
              breaks = c(0,1000000,2000000,3000000,4000000,5000000,6000000),
              palette="-RdYlBu",title="total price (£)")+
  tm_scale_bar(position = c(0.695,0), text.size = .8)+
  tm_layout(
    legend.position = c(0.75,0.3), 
    legend.text.size=.8, 
    legend.title.size = 1.1,
    main.title = "Average total house price in Camden(LSOA)",
    main.title.position = "center",
    main.title.color = "black",
    main.title.size = 1.2,
    legend.format = list(big.num.abbr = NA),
    frame=FALSE)+
  tm_compass(north=0, position = c(0.8, 0.12),size =3) +
  #bottom left top right
  tm_layout(inner.margin=c(0.02,0.02,0.02,0.2))

camden_total_price_map  


## mapping 
#birmingham unit
## mapping 
####read birmingham map data
birmingham_map_total_data <- read_excel(here::here("code","birmingham_group_total.xlsx"),na = c("N/A"), col_names = TRUE)
birmingham_map_unit_data <- read_excel(here::here("code","birmingham_group_unit.xlsx"),na = c("N/A"), col_names = TRUE)

birmingham_map_total_data <- Birminghamlsoas%>%
  right_join(.,
             birmingham_map_total_data, 
             by = c("lsoa_code" = "lsoa_code"))

birmingham_map_unit_data <- Birminghamlsoas%>%
  right_join(.,
             birmingham_map_unit_data, 
             by = c("lsoa_code" = "lsoa_code"))

tm_shape(birmingham_map_unit_data) + 
  tm_polygons(col='unit_price') 

#tm_shape(joined_data_birmingham) + 
#  tm_polygons(col='unit_price') 

birmingham_map_unit_data$`unit price` = birmingham_map_unit_data$unit_price
birmingham_unit_price_map <- tm_shape(birmingham_map_unit_data, bbbox = birminghambb) + 
  tm_polygons("unit price",
              breaks = c(0,1000,2000,3000,4000,5000),
              palette="-RdYlBu",title="unit price (£)")+
  tm_scale_bar(position = c("right", "bottom"), text.size = .8)+
  tm_layout(
    legend.position = c(0.75,0.3), 
    legend.text.size=.8, 
    legend.title.size = 1.1,
    main.title = "Average unit house price in Birmingham(LSOA)",
    main.title.position = "center",
    main.title.color = "black",
    main.title.size = 1.2,
    frame=FALSE)+
  #tm_text("LSOA11CD", size = 1/2)+
  tm_compass(north=0, position = c(0.8, 0.15),size =3) +
  #bottom left top right
  tm_layout(inner.margin=c(0.02,0.02,0.02,0.2))

birmingham_unit_price_map

birmingham_map_total_data$`total price` = birmingham_map_total_data$total_price
birmingham_total_price_map <- tm_shape(birmingham_map_total_data, bbbox = birminghambb) + 
  tm_polygons("total price",
              breaks = c(0,200000,400000,600000,800000,1000000),
              palette="-RdYlBu",title="total price (£)")+
  tm_scale_bar(position = c("right", "bottom"), text.size = .8)+
  tm_layout(
    legend.position = c(0.75,0.3), 
    legend.text.size=.8, 
    legend.title.size = 1.1,
    main.title = "Average total house price in Birmingham(LSOA)",
    main.title.position = "center",
    main.title.color = "black",
    main.title.size = 1.2,
    frame=FALSE)+
  #tm_text("LSOA11CD", size = 1/2)+
  tm_compass(north=0, position = c(0.8, 0.15),size =3) +
  #bottom left top right
  tm_layout(inner.margin=c(0.02,0.02,0.02,0.2))

birmingham_total_price_map

##multiple linear regression
data1<- read_excel(here::here("code","camden_unit_regression.xlsx"),na = c("N/A"), col_names = TRUE)
data2<- read_excel(here::here("code","bristol_unit_regression.xlsx"),na = c("N/A"), col_names = TRUE)
data3<- read_excel(here::here("code","camden_total_regression.xlsx"),na = c("N/A"), col_names = TRUE)
data4<- read_excel(here::here("code","bristol_total_regression.xlsx"),na = c("N/A"), col_names = TRUE)
data5<- read_excel(here::here("code","birmingham_unit_regression.xlsx"),na = c("N/A"), col_names = TRUE)
data6<- read_excel(here::here("code","birmingham_total_regression.xlsx"),na = c("N/A"), col_names = TRUE)

## camden unit train, test,validation split 
set.seed(123)
idx <- sample(seq(1, 3), size = nrow(data1), replace = TRUE, prob = c(.7, .15, .15))
camden_unit_train <- data1[idx == 1,]
camden_unit_test <- data1[idx == 2,]
camden_unit_val <- data1[idx == 3,]

## bristol unit train, test,validation split 
set.seed(123)
idx <- sample(seq(1, 3), size = nrow(data2), replace = TRUE, prob = c(.7, .15, .15))
bristol_unit_train <- data2[idx == 1,]
bristol_unit_test <- data2[idx == 2,]
bristol_unit_val <- data2[idx == 3,]

## camden total train, test,validation split 
set.seed(123)
idx <- sample(seq(1, 3), size = nrow(data3), replace = TRUE, prob = c(.7, .15, .15))
camden_total_train <- data3[idx == 1,]
camden_total_test <- data3[idx == 2,]
camden_total_val <- data3[idx == 3,]

## bristol total train, test,validation split 
set.seed(123)
idx <- sample(seq(1, 3), size = nrow(data4), replace = TRUE, prob = c(.7, .15, .15))
bristol_total_train <- data4[idx == 1,]
bristol_total_test <- data4[idx == 2,]
bristol_total_val <- data4[idx == 3,]

## birmingham unit train, test,validation split 
set.seed(123)
idx <- sample(seq(1, 3), size = nrow(data5), replace = TRUE, prob = c(.7, .15, .15))
birmingham_unit_train <- data5[idx == 1,]
birmingham_unit_test <- data5[idx == 2,]
birmingham_unit_val <- data5[idx == 3,]

## birmingham total train, test,validation split 
set.seed(123)
idx <- sample(seq(1, 3), size = nrow(data6), replace = TRUE, prob = c(.7, .15, .15))
birmingham_total_train <- data6[idx == 1,]
birmingham_total_test <- data6[idx == 2,]
birmingham_total_val <- data6[idx == 3,]


##camden unit price 
camden_unit_model1 <- lm(log(unit_price) ~ floor_area+ room_number+population_density+ num_of_household_with_children + employment_rate + num_of_no_qualification + car_per_household +property_type_S +property_type_T, data = camden_unit_train)

tidy(camden_unit_model1)
summary(camden_unit_model1)
vif(camden_unit_model1)#threshold = 5

camden_unit_train <- subset(camden_unit_train,select=-c(num_of_household_with_children))
camden_unit_test <- subset(camden_unit_test,select=-c(num_of_household_with_children))
camden_unit_val <- subset(camden_unit_val,select=-c(num_of_household_with_children))

camden_unit_model2 <- lm(log(unit_price) ~ floor_area+ room_number+population_density+ car_per_household + employment_rate + num_of_no_qualification +property_type_S +property_type_T, data = camden_unit_train)

tidy(camden_unit_model2)
summary(camden_unit_model2)
vif(camden_unit_model2)#threshold = 5

camden_unit_train <- subset(camden_unit_train,select=-c(room_number))
camden_unit_test <- subset(camden_unit_test,select=-c(room_number))
camden_unit_val <- subset(camden_unit_val,select=-c(room_number))

camden_unit_model3 <- lm(log(unit_price) ~ floor_area+population_density+ car_per_household + employment_rate + num_of_no_qualification +property_type_S +property_type_T, data = camden_unit_train)

tidy(camden_unit_model3)
summary(camden_unit_model3)
vif(camden_unit_model3)#threshold = 5

camden_unit_train <- subset(camden_unit_train,select=-c(car_per_household))
camden_unit_test <- subset(camden_unit_test,select=-c(car_per_household))
camden_unit_val <- subset(camden_unit_val,select=-c(car_per_household))

camden_unit_model4 <- lm(log(unit_price) ~ floor_area+population_density + employment_rate + num_of_no_qualification +property_type_S +property_type_T, data = camden_unit_train)

tidy(camden_unit_model4)
summary(camden_unit_model4)#r2=0.253
vif(camden_unit_model4)#threshold = 5

DW1 <- durbinWatsonTest(camden_unit_model4)
tidy(DW1)#1.99

model_data1 <- camden_unit_model4 %>%
  augment(., camden_unit_train)

joined_model_data1 <- Camdenlsoas%>%
  right_join(.,
             model_data1, 
             by = c("LSOA11CD" = "lsoa_code"))

joined_model_data1 <- joined_model_data1 %>%
  mutate(model1resids = residuals(camden_unit_model4))

joined_model_data1_map_res <- aggregate(joined_model_data1[, 'model1resids'], list(joined_model_data1$LSOA11CD), mean)

tm_shape(joined_model_data1_map_res) +
  tm_polygons("model1resids",
              palette = "RdYlBu")

##residual map for camden unit model 
joined_model_data1_map_res$`residual value` <- joined_model_data1_map_res$model1resids
OLS_residual_map <- tm_shape(joined_model_data1_map_res, bbbox = camdenbb) + 
  tm_polygons("residual value",
              breaks = c(-0.6, -0.4, -0.2, 0.0, 0.2,0.4,0.6,0.8),
              palette="-RdYlBu",title="Residuals")+
  tm_scale_bar(position = c(0.659,0), text.size = .8)+
  tm_layout(legend.position = c(0.75,0.3), 
            legend.text.size=.8, 
            legend.title.size = 1.1,
            main.title = "Residuals of unit price prediction model (Camden)",
            main.title.position = "center",
            main.title.size = 1.2,
            main.title.color = "black",
            frame=FALSE)+
  tm_compass(north=0, position = c(0.8, 0.12)) +
  tm_layout(inner.margin=c(0.02,0.02,0.02,0.2))
OLS_residual_map


camden_unit_model4.pred <-exp(predict(camden_unit_model4, camden_unit_test[,2:14]))

rss <- sum((camden_unit_model4.pred - as.numeric(unlist(camden_unit_test[,1]))) ^ 2)
tss <- sum((as.numeric(unlist(camden_unit_test[,1])) - mean(as.numeric(unlist(camden_unit_test[,1])))) ^ 2)
rsq <- 1 - rss/tss#r2=0.065
rsq


rmse(as.numeric(unlist(camden_unit_test[,1])),camden_unit_model4.pred)#4280.76
## camden total price 
camden_total_model1 <- lm(sqrt(total_price) ~ floor_area+ room_number+population_density+ num_of_household_with_children + employment_rate + num_of_no_qualification + car_per_household +property_type_S +property_type_T, data = camden_total_train)

tidy(camden_total_model1)
summary(camden_total_model1)
vif(camden_total_model1)#threshold = 5

camden_total_train <- subset(camden_total_train,select=-c(num_of_household_with_children))
camden_total_test <- subset(camden_total_test,select=-c(num_of_household_with_children))
camden_total_val <- subset(camden_total_val,select=-c(num_of_household_with_children))

camden_total_model2 <- lm(sqrt(total_price) ~ floor_area+ room_number+population_density + employment_rate + num_of_no_qualification + car_per_household + property_type_S +property_type_T, data = camden_total_train)

tidy(camden_total_model2)
summary(camden_total_model2)
vif(camden_total_model2)#threshold = 5

camden_total_train <- subset(camden_total_train,select=-c(employment_rate))
camden_total_test <- subset(camden_total_test,select=-c(employment_rate))
camden_total_val <- subset(camden_total_val,select=-c(employment_rate))

camden_total_model3 <- lm(sqrt(total_price) ~ floor_area+ room_number+population_density + num_of_no_qualification + car_per_household + property_type_S +property_type_T, data = camden_total_train)

tidy(camden_total_model3)
summary(camden_total_model3)
vif(camden_total_model3)#threshold = 5

camden_total_train <- subset(camden_total_train,select=-c(room_number))
camden_total_test <- subset(camden_total_test,select=-c(room_number))
camden_total_val <- subset(camden_total_val,select=-c(room_number))

camden_total_model4 <- lm(sqrt(total_price) ~ floor_area +population_density + num_of_no_qualification + car_per_household + property_type_S +property_type_T, data = camden_total_train)

tidy(camden_total_model4)
summary(camden_total_model4)
vif(camden_total_model4)#threshold = 5

camden_total_train <- subset(camden_total_train,select=-c(car_per_household))
camden_total_test <- subset(camden_total_test,select=-c(car_per_household))
camden_total_val <- subset(camden_total_val,select=-c(car_per_household))

camden_total_model5 <- lm(sqrt(total_price) ~ floor_area +population_density + num_of_no_qualification + property_type_S +property_type_T, data = camden_total_train)

tidy(camden_total_model5)
summary(camden_total_model5)#0.850
vif(camden_total_model5)#threshold = 5

DW2 <- durbinWatsonTest(camden_total_model5)
tidy(DW2)#1.99

model_data2 <- camden_total_model5 %>%
  augment(., camden_total_train)

joined_model_data2 <- Camdenlsoas%>%
  right_join(.,
             model_data2, 
             by = c("LSOA11CD" = "lsoa_code"))

joined_model_data2 <- joined_model_data2 %>%
  mutate(model2resids = residuals(camden_total_model5))



##residual map for camden unit model 
joined_model_data2_map_res <- aggregate(joined_model_data2[, 'model2resids'], list(joined_model_data2$LSOA11CD), mean)

tm_shape(joined_model_data2_map_res) +
  tm_polygons("model2resids",
              palette = "RdYlBu")

joined_model_data2_map_res$`residual value` <- joined_model_data2_map_res$model2resids
OLS_residual_map <- tm_shape(joined_model_data2_map_res, bbbox = camdenbb) + 
  tm_polygons("residual value",
              breaks = c(-600, -400, -200, 0, 200, 400,600,800),
              palette="-RdYlBu",title="Residuals")+
  tm_scale_bar(position = c("right", "bottom"), text.size = .8)+
  tm_layout(legend.position = c(0.73,0.3), 
            legend.text.size=.8, 
            legend.title.size = 1.1,
            main.title = "Residuals of total price prediction model (Camden)",
            main.title.position = "center",
            main.title.size = 1.2,
            main.title.color = "black",
            frame=FALSE)+
  tm_compass(north=0, position = c(0.8, 0.15)) +
  tm_layout(inner.margin=c(0.02,0.02,0.02,0.2))
OLS_residual_map


camden_total_model5.pred <-(predict(camden_total_model5, camden_total_test[,2:10])) ** 2
rss <- sum((camden_total_model5.pred - as.numeric(unlist(camden_total_test[,1]))) ^ 2)
tss <- sum((as.numeric(unlist(camden_total_test[,1])) - mean(as.numeric(unlist(camden_total_test[,1])))) ^ 2)
rsq <- 1 - rss/tss#r2=0.610
rsq

rmse(as.numeric(unlist(camden_total_test[,1])),camden_total_model5.pred)# 847199.9

##bristol unit price 
bristol_unit_model1 <- lm(sqrt(unit_price) ~ floor_area+ room_number+population_density + employment_num + num_of_no_qualification + no_car_avalibility +property_type_S + social_rented_tenure_per_household+property_type_T, data = bristol_unit_train)

tidy(bristol_unit_model1)
summary(bristol_unit_model1)
vif(bristol_unit_model1)#threshold = 5

bristol_unit_train <- subset(bristol_unit_train,select=-c(property_type_T))
bristol_unit_test <- subset(bristol_unit_test,select=-c(property_type_T))
bristol_unit_val <- subset(bristol_unit_val,select=-c(property_type_T))

bristol_unit_model2 <- lm(sqrt(unit_price) ~ floor_area+ room_number+population_density + employment_num + num_of_no_qualification + no_car_avalibility +property_type_S + social_rented_tenure_per_household, data = bristol_unit_train)

tidy(bristol_unit_model2)
summary(bristol_unit_model2)
vif(bristol_unit_model2)#threshold = 5

DW3 <- durbinWatsonTest(bristol_unit_model2)
tidy(DW3)#2.00

model_data3 <- bristol_unit_model2 %>%
  augment(., bristol_unit_train)

joined_model_data3 <- Bristollsoas%>%
  right_join(.,
             model_data3, 
             by = c("lsoa11cd" = "lsoa_code"))

joined_model_data3 <- joined_model_data3 %>%
  mutate(model3resids = residuals(bristol_unit_model2))


joined_model_data3_map_res <- aggregate(joined_model_data3[, 'model3resids'], list(joined_model_data3$lsoa11cd), mean)

tm_shape(joined_model_data3_map_res) +
  tm_polygons("model3resids",
              palette = "RdYlBu")

##residual map for bristol unit model 
joined_model_data3_map_res$`residual value` <- joined_model_data3_map_res$model3resids
OLS_residual_map <- tm_shape(joined_model_data3_map_res, bbbox = bristolbb) + 
  tm_polygons("residual value",
              breaks = c(-10, -5, 0, 5,10),
              palette="-RdYlBu",title="Residuals")+
  tm_scale_bar(position = c("right", "bottom"), text.size = .8)+
  tm_layout(legend.position = c(0.8,0.3), 
            legend.text.size=.8, 
            legend.title.size = 1.1,
            main.title = "Residuals of unit price prediction model (Bristol)",
            main.title.position = "center",
            main.title.size = 1.2,
            main.title.color = "black",
            frame=FALSE)+
  tm_compass(north=0, position = c(0.8, 0.15)) +
  tm_layout(inner.margin=c(0.02,0.02,0.02,0.2))
OLS_residual_map

bristol_unit_model2.pred <-predict(bristol_unit_model2, bristol_unit_test[2:13])** 2
rss <- sum((bristol_unit_model2.pred - as.numeric(unlist(bristol_unit_test[,1]))) ^ 2)
tss <- sum((as.numeric(unlist(bristol_unit_test[,1])) - mean(as.numeric(unlist(bristol_unit_test[,1])))) ^ 2)
rsq <- 1 - rss/tss#r2=0.363
rsq

rmse(as.numeric(unlist(bristol_unit_test[,1])),bristol_unit_model2.pred)#786.142

## bristol total price 
bristol_total_model1 <- lm(sqrt(total_price) ~ floor_area+ room_number+population_density + employment_num + num_of_no_qualification + no_car_avalibility +social_rented_tenure_per_household+property_type_S +property_type_T, data = bristol_total_train)

tidy(bristol_total_model1)
summary(bristol_total_model1)
vif(bristol_total_model1)#threshold = 5

DW4 <- durbinWatsonTest(bristol_total_model1)
tidy(DW4)#2.01

model_data4 <- bristol_total_model1 %>%
  augment(., bristol_total_train)

joined_model_data4 <- Bristollsoas%>%
  right_join(.,
             model_data4, 
             by = c("lsoa11cd" = "lsoa_code"))

joined_model_data4 <- joined_model_data4 %>%
  mutate(model4resids = residuals(bristol_total_model1))


joined_model_data4_map_res <- aggregate(joined_model_data4[, 'model4resids'], list(joined_model_data4$lsoa11cd), mean)

tm_shape(joined_model_data4_map_res) +
  tm_polygons("model4resids",
              palette = "RdYlBu")


##residual map for bristol total model 
joined_model_data4_map_res$`residual value` <- joined_model_data4_map_res$model4resids
OLS_residual_map <- tm_shape(joined_model_data4_map_res, bbbox = bristolbb) + 
  tm_polygons("residual value",
              breaks = c(-100, -50, 0, 50, 100),
              palette="-RdYlBu",title="Residuals")+
  tm_scale_bar(position = c("right", "bottom"), text.size = .8)+
  tm_layout(legend.position = c(0.8,0.3), 
            legend.text.size=.8, 
            legend.title.size = 1.1,
            main.title = "Residuals of total price prediction model (Bristol)",
            main.title.position = "center",
            main.title.size = 1.2,
            main.title.color = "black",
            frame=FALSE)+
  tm_compass(north=0, position = c(0.8, 0.15)) +
  tm_layout(inner.margin=c(0.02,0.02,0.02,0.2))
OLS_residual_map

bristol_total_model1.pred <-predict(bristol_total_model1, bristol_total_test[,2:14]) ** 2
rss <- sum((bristol_total_model1.pred - as.numeric(unlist(bristol_total_test[,1]))) ^ 2)
tss <- sum((as.numeric(unlist(bristol_total_test[,1])) - mean(as.numeric(unlist(bristol_total_test[,1])))) ^ 2)
rsq <- 1 - rss/tss#r2=0.727
rsq

rmse(as.numeric(unlist(bristol_total_test[,1])),bristol_total_model1.pred)#97706.82


##birmingham unit price 
birmingham_unit_model1 <- lm(sqrt(unit_price) ~ floor_area+ room_number+population_density + num_of_household_with_children +old_new_Y + employment_num + num_of_no_qualification + no_car_avalibility +property_type_S + social_rented_tenure_per_household+property_type_T+property_type_F, data = birmingham_unit_train)

tidy(birmingham_unit_model1)
summary(birmingham_unit_model1)
vif(birmingham_unit_model1)#threshold = 5

birmingham_unit_train <- subset(birmingham_unit_train,select=-c(population_density))
birmingham_unit_test <- subset(birmingham_unit_test,select=-c(population_density))
birmingham_unit_val <- subset(birmingham_unit_val,select=-c(population_density))

birmingham_unit_model2 <- lm(sqrt(unit_price) ~ floor_area+ room_number + num_of_household_with_children +old_new_Y + employment_num + num_of_no_qualification + no_car_avalibility +property_type_S + social_rented_tenure_per_household+property_type_T+property_type_F, data = birmingham_unit_train)

tidy(birmingham_unit_model2)
summary(birmingham_unit_model2)
vif(birmingham_unit_model2)#threshold = 5

birmingham_unit_train <- subset(birmingham_unit_train,select=-c(old_new_Y))
birmingham_unit_test <- subset(birmingham_unit_test,select=-c(old_new_Y))
birmingham_unit_val <- subset(birmingham_unit_val,select=-c(old_new_Y))

birmingham_unit_model3 <- lm(sqrt(unit_price) ~ floor_area+ room_number + num_of_household_with_children + employment_num + num_of_no_qualification + no_car_avalibility +property_type_S + social_rented_tenure_per_household+property_type_T+property_type_F, data = birmingham_unit_train)

tidy(birmingham_unit_model3)
summary(birmingham_unit_model3)
vif(birmingham_unit_model3)#threshold = 5

DW5 <- durbinWatsonTest(birmingham_unit_model3)
tidy(DW5)#1.96

model_data3 <- birmingham_unit_model3 %>%
  augment(., birmingham_unit_train)

joined_model_data5 <- Birminghamlsoas%>%
  right_join(.,
             model_data3, 
             by = c("lsoa_code" = "lsoa_code"))

joined_model_data5 <- joined_model_data5 %>%
  mutate(model5resids = residuals(birmingham_unit_model3))

joined_model_data5_map_res <- aggregate(joined_model_data5[, 'model5resids'], list(joined_model_data5$lsoa_code), mean)

tm_shape(joined_model_data5_map_res) +
  tm_polygons("model5resids",
              palette = "RdYlBu")

##residual map for bristol unit model 
joined_model_data5_map_res$`residual value` <- joined_model_data5_map_res$model5resids
OLS_residual_map <- tm_shape(joined_model_data5_map_res, bbbox = birminghambb) + 
  tm_polygons("residual value",
              breaks = c(-20, -10, 0, 10,20,30),
              palette="-RdYlBu",title="Residuals")+
  tm_scale_bar(position = c("right", "bottom"), text.size = .8)+
  tm_layout(legend.position = c(0.8,0.3), 
            legend.text.size=.8, 
            legend.title.size = 1.1,
            main.title = "Residuals of unit price prediction model (Birmingham)",
            main.title.position = "center",
            main.title.size = 1.2,
            main.title.color = "black",
            frame=FALSE)+
  tm_compass(north=0, position = c(0.8, 0.15)) +
  tm_layout(inner.margin=c(0.02,0.02,0.02,0.2))
OLS_residual_map

birmingham_unit_model3.pred <-predict(birmingham_unit_model3, birmingham_unit_test[2:14])** 2
rss <- sum((birmingham_unit_model3.pred - as.numeric(unlist(birmingham_unit_test[,1]))) ^ 2)
tss <- sum((as.numeric(unlist(birmingham_unit_test[,1])) - mean(as.numeric(unlist(birmingham_unit_test[,1])))) ^ 2)
rsq <- 1 - rss/tss#r2=0.545
rsq

rmse(as.numeric(unlist(birmingham_unit_test[,1])),birmingham_unit_model3.pred)#521.0325

## Birmingham total price 
birmingham_total_model1 <- lm(sqrt(total_price) ~ floor_area+ room_number + num_of_household_with_children + employment_num + num_of_no_qualification + no_car_avalibility +property_type_S + social_rented_tenure_per_household+property_type_T+property_type_F, data = birmingham_total_train)

tidy(birmingham_total_model1)
summary(birmingham_total_model1)
vif(birmingham_total_model1)#threshold = 5

DW6 <- durbinWatsonTest(birmingham_total_model1)
tidy(DW6)#1.95

model_data6 <- birmingham_total_model1 %>%
  augment(., birmingham_total_train)

joined_model_data6 <- Birminghamlsoas%>%
  right_join(.,
             model_data6, 
             by = c("lsoa_code" = "lsoa_code"))

joined_model_data6 <- joined_model_data6 %>%
  mutate(model6resids = residuals(birmingham_total_model1))

tm_shape(joined_model_data6) +
  tm_polygons("model6resids",
              palette = "RdYlBu")

##residual map for birmingham total model 

joined_model_data6_map_res <- aggregate(joined_model_data6[, 'model6resids'], list(joined_model_data6$lsoa_code), mean)

tm_shape(joined_model_data6_map_res) +
  tm_polygons("model6resids",
              palette = "RdYlBu")

joined_model_data6_map_res$`residual value` <- joined_model_data6_map_res$model6resids
OLS_residual_map <- tm_shape(joined_model_data6_map_res, bbbox = birminghambb) + 
  tm_polygons("residual value",
              breaks = c(-200,-100, 0, 100,200,300, 400),
              palette="-RdYlBu",title="Residuals")+
  tm_scale_bar(position = c("right", "bottom"), text.size = .8)+
  tm_layout(legend.position = c(0.8,0.3), 
            legend.text.size=.8, 
            legend.title.size = 1.1,
            main.title = "Residuals of total price prediction model (Birmingham)",
            main.title.position = "center",
            main.title.size = 1.2,
            main.title.color = "black",
            frame=FALSE)+
  tm_compass(north=0, position = c(0.8, 0.15)) +
  tm_layout(inner.margin=c(0.02,0.02,0.02,0.2))
OLS_residual_map

birmingham_total_model1.pred <-predict(birmingham_total_model1, birmingham_total_test[,2:14]) ** 2
rss <- sum((birmingham_total_model1.pred - as.numeric(unlist(birmingham_total_test[,1]))) ^ 2)
tss <- sum((as.numeric(unlist(birmingham_total_test[,1])) - mean(as.numeric(unlist(birmingham_total_test[,1])))) ^ 2)
rsq <- 1 - rss/tss#r2=0.794
rsq

rmse(as.numeric(unlist(birmingham_total_test[,1])),birmingham_total_model1.pred)#52514.17

###other models bristol 

#calculate the centroidsjoined_model_data1_map_res
coords_bristol <- joined_model_data3_map_res%>%
  st_centroid()%>%
  st_geometry()

plot(coords_bristol)

bristol_lsoa_nb <- joined_model_data3_map_res %>%
  poly2nb(., queen=T)

#or nearest neighbours
knn_bristol_lsoa <-coords_bristol %>%
  knearneigh(., k=4)

lsoa_knn_bristol <- knn_bristol_lsoa %>%
  knn2nb()

#plot them
#plot(bristol_lsoa_nb, st_geometry(coords_bristol), col="red")
#plot(knn_bristol_lsoa, st_geometry(coords_bristol), col="blue")

#create a spatial weights matrix object from these weights

lsoa_bristol.queens_weight <- bristol_lsoa_nb %>%
  nb2listw(., style="C")

lsoa_bristol.knn_4_weight <- lsoa_knn_bristol %>%
  nb2listw(., style="C")

Queen_bristol <- joined_model_data3_map_res %>%
  st_drop_geometry()%>%
  dplyr::select(model3resids)%>%
  pull()%>%
  moran.test(., lsoa_bristol.queens_weight)%>%
  tidy()

Nearest_neighbour_bristol <- joined_model_data3_map_res %>%
  st_drop_geometry()%>%
  dplyr::select(model3resids)%>%
  pull()%>%
  moran.test(., lsoa_bristol.knn_4_weight)%>%
  tidy()

Queen_bristol
Nearest_neighbour_bristol


##unit price

##random forest model 
rfgrid <- expand.grid(.mtry=c(1:3))
control <- trainControl(method="repeatedcv", number=10, repeats=3, search="grid")
set.seed(123)

bristol_unit_train_rf <- bristol_unit_train %>% select(unit_price,floor_area,room_number,population_density,employment_num,num_of_no_qualification,no_car_avalibility,property_type_S,social_rented_tenure_per_household) 
bristol_unit_test_rf <- bristol_unit_test %>% select(unit_price,floor_area,room_number,population_density,employment_num,num_of_no_qualification,no_car_avalibility,property_type_S,social_rented_tenure_per_household) 

rf_grid <- train(bristol_unit_train_rf[,-1],sqrt(as.numeric(unlist(bristol_unit_train_rf[,1]))), method="rf",ntrees = 100, trControl=control,tuneGrid=rfgrid)
print(rf_grid)#0.537
plot(rf_grid)

rf.pred<-predict(rf_grid$finalModel, bristol_unit_test_rf[,-1]) ** 2
rss <- sum((rf.pred - as.numeric(unlist(bristol_unit_test_rf[,1]))) ^ 2)
tss <- sum((as.numeric(unlist(bristol_unit_test_rf[,1])) - mean(as.numeric(unlist(bristol_unit_test_rf[,1])))) ^ 2)
rsq <- 1 - rss/tss
rsq#0.517

rf.unit.bristol <- randomForest(sqrt(unit_price) ~ floor_area+room_number+population_density+employment_num+num_of_no_qualification+no_car_avalibility+social_rented_tenure_per_household+property_type_S, data=bristol_unit_train_rf, ntree=100,mtry=3, keep.forest=FALSE,
                                 importance=TRUE)
varImpPlot(rf.unit.bristol,main=deparse(substitute("Variable importance of the unit price prediction model in Bristol")))

impotance = as.data.frame(importance(rf.unit.bristol))
impotance = cbind(vars=rownames(impotance), impotance)
impotance = impotance[order(impotance$`%IncMSE`),]
impotance$vars = factor(impotance$vars, levels=unique(impotance$vars))
par(mar=c(5,15,4,1)+.1)
barplot(impotance$`%IncMSE`, names.arg=impotance$vars,horiz=TRUE,las=1,col="#f06359",main="Variable importance of unit price prediction model (Bristol)")


rmse(as.numeric(unlist(bristol_unit_test_rf[,1])),rf.pred)#684.9579

## neural network
bristol_unit_train_ann <- bristol_unit_train %>% select(unit_price,floor_area,room_number,population_density,employment_num,num_of_no_qualification,no_car_avalibility,property_type_S,social_rented_tenure_per_household) 
bristol_unit_test_ann <- bristol_unit_test %>% select(unit_price,floor_area,room_number,population_density,employment_num,num_of_no_qualification,no_car_avalibility,property_type_S,social_rented_tenure_per_household) 
bristol_unit_train_ann.scale <- scale(bristol_unit_train_ann)
bristol_unit_test_ann.scale <- scale(bristol_unit_test_ann)
mygrid <- expand.grid(.decay=c(0.1,0.01,0.001,0.0001, 0.5,0.05,0.005,0.0005), .size=c(3, 4, 5, 6, 7, 8, 9, 10))
set.seed(123)
fitControl <- trainControl("repeatedcv", number = 10, repeats = 3, returnResamp = "all")
housing.nnet.cv <- train(bristol_unit_train_ann.scale[,-1],as.numeric(unlist(bristol_unit_train_ann.scale[,1])),"nnet", trControl = fitControl, tuneGrid=mygrid)
plot(housing.nnet.cv)
print(housing.nnet.cv)#0.447
housing.nnet.cv$finalModel
summary(housing.nnet.cv$finalModel)
source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')
plot.nnet(housing.nnet.cv$finalModel)#0.5417

ann.pred<-predict(housing.nnet.cv$finalModel, bristol_unit_test_ann.scale[,-1])
rss <- sum((ann.pred - as.numeric(unlist(bristol_unit_test_ann.scale[,1]))) ^ 2)
tss <- sum((as.numeric(unlist(bristol_unit_test_ann.scale[,1])) - mean(as.numeric(unlist(bristol_unit_test_ann.scale[,1])))) ^ 2)
rsq <- 1 - rss/tss
rsq#0.274

rmse(as.numeric(unlist(bristol_unit_test_ann.scale[,1])),ann.pred)#0.8510381

"
#unscale not work
NN.nnet <- nnet(bristol_unit_train_ann[,-1], sqrt(as.numeric(unlist(bristol_unit_train_ann[,1]))), decay=0.05, linout = TRUE, size=4)
NN.pred<-predict(NN.nnet, bristol_unit_test_ann) ** 2
NN.pred[,1]
rss <- sum((NN.pred - as.numeric(unlist(bristol_unit_test_ann[,1]))) ^ 2)
tss <- sum((as.numeric(unlist(bristol_unit_test_ann[,1])) - mean(as.numeric(unlist(bristol_unit_test_ann[,1])))) ^ 2)
rsq <- 1 - rss/tss
"

##SVR
bristol_unit_train_svr <- bristol_unit_train %>% select(unit_price,floor_area,room_number,population_density,employment_num,num_of_no_qualification,no_car_avalibility,social_rented_tenure_per_household) 
bristol_unit_test_svr <- bristol_unit_test %>% select(unit_price,floor_area,room_number,population_density,employment_num,num_of_no_qualification,no_car_avalibility,social_rented_tenure_per_household) 
bristol_unit_train_svr.scale <- scale(bristol_unit_train_svr)
bristol_unit_test_svr.scale <- scale(bristol_unit_test_svr)

SVRgrid <- expand.grid(.sigma=c(0.001, 0.01, 0.1), .C=c(10,100,1000))
ctrl <- trainControl(method = "cv", number=5)
SVRGridFit <- train(bristol_unit_train_svr.scale[,-1],as.numeric(unlist(bristol_unit_train_svr.scale[,1])), method="svmRadial", tuneGrid=SVRgrid,epsilon=0.01,
                    trControl=ctrl, type="eps-svr")
SVRGridFit#r2 = 0.497
plot(SVRGridFit)
names(SVRGridFit)
SVRGridFit$finalModels 
SVRGridFit$finalModel@nSV

stforecast <- predict(SVRGridFit$finalModel, bristol_unit_test_svr.scale[,-1],type = "response") 
stforecast <- stforecast 
rss <- sum((stforecast - as.numeric(unlist(bristol_unit_test_svr.scale[,1]))) ^ 2)
tss <- sum((as.numeric(unlist(bristol_unit_test_svr.scale[,1])) - mean(as.numeric(unlist(bristol_unit_test_svr.scale[,1])))) ^ 2)
rsq <- 1 - rss/tss
rsq#0.447

rmse(as.numeric(unlist(bristol_unit_test_svr.scale[,1])),stforecast)#0.7441234
#library(kenlab)
#stforecast <- predict(SVRGridFit$finalModel, bristol_unit_test_svr[,-1],type = "response") 
#stforecast <- stforecast ** 2
#rss <- sum((stforecast - as.numeric(unlist(bristol_unit_test_svr[,1]))) ^ 2)
#tss <- sum((as.numeric(unlist(bristol_unit_test_svr[,1])) - mean(as.numeric(unlist(bristol_unit_test_svr[,1])))) ^ 2)
#rsq <- 1 - rss/tss
#rsq#0.456
""


## bristol total
#calculate the centroids
coords_bristol <- joined_model_data4%>%
  st_centroid()%>%
  st_geometry()

plot(coords_bristol)

bristol_lsoa_nb <- joined_model_data4 %>%
  poly2nb(., queen=T)

#or nearest neighbours
knn_bristol_lsoa <-coords_bristol %>%
  knearneigh(., k=4)

lsoa_knn_bristol <- knn_bristol_lsoa %>%
  knn2nb()

#plot them
#plot(bristol_lsoa_nb, st_geometry(coords_bristol), col="red")
#plot(knn_bristol_lsoa, st_geometry(coords_bristol), col="blue")

#create a spatial weights matrix object from these weights

lsoa_bristol.queens_weight <- bristol_lsoa_nb %>%
  nb2listw(., style="C")

lsoa_bristol.knn_4_weight <- lsoa_knn_bristol %>%
  nb2listw(., style="C")

Queen_bristol <- joined_model_data4 %>%
  st_drop_geometry()%>%
  dplyr::select(model4resids)%>%
  pull()%>%
  moran.test(., lsoa_bristol.queens_weight)%>%
  tidy()

Nearest_neighbour_bristol <- joined_model_data4 %>%
  st_drop_geometry()%>%
  dplyr::select(model4resids)%>%
  pull()%>%
  moran.test(., lsoa_bristol.knn_4_weight)%>%
  tidy()

Queen_bristol
Nearest_neighbour_bristol


##random forest model 
rfgrid <- expand.grid(.mtry=c(1:3))
control <- trainControl(method="repeatedcv", number=10, repeats=3, search="grid")
set.seed(123)

bristol_total_train_rf <- bristol_total_train %>% select(total_price, floor_area, room_number,population_density , employment_num , num_of_no_qualification, no_car_avalibility ,social_rented_tenure_per_household,property_type_S,property_type_T) 
bristol_total_test_rf <- bristol_total_test %>% select(total_price, floor_area, room_number,population_density , employment_num , num_of_no_qualification, no_car_avalibility ,social_rented_tenure_per_household,property_type_S,property_type_T) 

rf_grid <- train(bristol_total_train_rf[,-1],sqrt(as.numeric(unlist(bristol_total_train_rf[,1]))), method="rf",ntrees = 100, trControl=control,tuneGrid=rfgrid)
print(rf_grid)#0.840
plot(rf_grid)

rf.pred<-predict(rf_grid$finalModel, bristol_total_test_rf[,-1]) ** 2
rss <- sum((rf.pred - as.numeric(unlist(bristol_total_test_rf[,1]))) ^ 2)
tss <- sum((as.numeric(unlist(bristol_total_test_rf[,1])) - mean(as.numeric(unlist(bristol_total_test_rf[,1])))) ^ 2)
rsq <- 1 - rss/tss
rsq#0.827

rf.total.bristol <- randomForest(sqrt(total_price) ~ floor_area+room_number+population_density+employment_num+num_of_no_qualification+no_car_avalibility+social_rented_tenure_per_household+property_type_S+property_type_T, data=bristol_total_train_rf, ntree=100,mtry=3, keep.forest=FALSE,
                               importance=TRUE)
varImpPlot(rf.total.bristol,main=deparse(substitute("Variable importance of the total price prediction model in Bristol")))

impotance = as.data.frame(importance(rf.total.bristol))
impotance = cbind(vars=rownames(impotance), impotance)
impotance = impotance[order(impotance$`%IncMSE`),]
impotance$vars = factor(impotance$vars, levels=unique(impotance$vars))
par(mar=c(5,15,4,1)+.1)
barplot(impotance$`%IncMSE`, names.arg=impotance$vars,horiz=TRUE,las=1,col="#f06359",main="Variable importance of total price prediction model (Bristol)")

rmse(as.numeric(unlist(bristol_total_test_rf[,1])),rf.pred)#77750.67

## neural network
bristol_total_train_ann <- bristol_total_train %>% select(total_price, floor_area, room_number,population_density , employment_num , num_of_no_qualification, no_car_avalibility ,social_rented_tenure_per_household,property_type_S,property_type_T) 
bristol_total_test_ann <- bristol_total_test %>% select(total_price, floor_area, room_number,population_density , employment_num , num_of_no_qualification, no_car_avalibility ,social_rented_tenure_per_household,property_type_S,property_type_T) 
bristol_total_train_ann.scale <- scale(bristol_total_train_ann)
bristol_total_test_ann.scale <- scale(bristol_total_test_ann)
mygrid <- expand.grid(.decay=c(0.1,0.01,0.001,0.0001, 0.5,0.05,0.005,0.0005), .size=c(3, 4, 5, 6, 7, 8, 9, 10))
set.seed(123)
fitControl <- trainControl("repeatedcv", number = 10, repeats = 3, returnResamp = "all")
housing.nnet.cv <- train(bristol_total_train_ann.scale[,-1],as.numeric(unlist(bristol_total_train_ann.scale[,1])),"nnet", trControl = fitControl, tuneGrid=mygrid)
plot(housing.nnet.cv)
print(housing.nnet.cv)#0.641
housing.nnet.cv$finalModel
summary(housing.nnet.cv$finalModel)
source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')
plot.nnet(housing.nnet.cv$finalModel)#0.5417

ann.pred<-predict(housing.nnet.cv$finalModel, bristol_total_test_ann.scale[,-1])
rss <- sum((ann.pred - as.numeric(unlist(bristol_total_test_ann.scale[,1]))) ^ 2)
tss <- sum((as.numeric(unlist(bristol_total_test_ann.scale[,1])) - mean(as.numeric(unlist(bristol_total_test_ann.scale[,1])))) ^ 2)
rsq <- 1 - rss/tss
rsq#0.378

rmse(as.numeric(unlist(bristol_total_test_ann.scale[,1])),ann.pred)# 0.7878058

"
#unscale not work
NN.nnet <- nnet(bristol_unit_train_ann[,-1], sqrt(as.numeric(unlist(bristol_unit_train_ann[,1]))), decay=0.05, linout = TRUE, size=4)
NN.pred<-predict(NN.nnet, bristol_unit_test_ann) ** 2
NN.pred[,1]
rss <- sum((NN.pred - as.numeric(unlist(bristol_unit_test_ann[,1]))) ^ 2)
tss <- sum((as.numeric(unlist(bristol_unit_test_ann[,1])) - mean(as.numeric(unlist(bristol_unit_test_ann[,1])))) ^ 2)
rsq <- 1 - rss/tss
"

##SVR
bristol_total_train_svr <- bristol_total_train %>% select(total_price, floor_area, room_number,population_density , employment_num , num_of_no_qualification, no_car_avalibility ,social_rented_tenure_per_household,property_type_S,property_type_T) 
bristol_total_test_svr <- bristol_total_test %>% select(total_price, floor_area, room_number,population_density , employment_num , num_of_no_qualification, no_car_avalibility ,social_rented_tenure_per_household,property_type_S,property_type_T) 
bristol_total_train_svr.scale <- scale(bristol_total_train_svr)
bristol_total_test_svr.scale <- scale(bristol_total_test_svr)

SVRgrid <- expand.grid(.sigma=c(0.001, 0.01, 0.1), .C=c(10,100,1000))
ctrl <- trainControl(method = "cv", number=5)
SVRGridFit <- train(bristol_total_train_svr.scale[,-1],as.numeric(unlist(bristol_total_train_svr.scale[,1])), method="svmRadial", tuneGrid=SVRgrid,epsilon=0.01,
                    trControl=ctrl, type="eps-svr")
SVRGridFit#r2 = 0.497
plot(SVRGridFit)
names(SVRGridFit)
SVRGridFit$finalModels 
SVRGridFit$finalModel@nSV

stforecast <- predict(SVRGridFit$finalModel, bristol_total_test_svr.scale[,-1],type = "response") 
stforecast <- stforecast 
rss <- sum((stforecast - as.numeric(unlist(bristol_total_test_svr.scale[,1]))) ^ 2)
tss <- sum((as.numeric(unlist(bristol_total_test_svr.scale[,1])) - mean(as.numeric(unlist(bristol_total_test_svr.scale[,1])))) ^ 2)
rsq <- 1 - rss/tss
rsq#0.447

rmse(as.numeric(unlist(bristol_total_test_svr.scale[,1])),stforecast)# 0.4343489

#library(kenlab)
#stforecast <- predict(SVRGridFit$finalModel, bristol_unit_test_svr[,-1],type = "response") 
#stforecast <- stforecast ** 2
#rss <- sum((stforecast - as.numeric(unlist(bristol_unit_test_svr[,1]))) ^ 2)
#tss <- sum((as.numeric(unlist(bristol_unit_test_svr[,1])) - mean(as.numeric(unlist(bristol_unit_test_svr[,1])))) ^ 2)
#rsq <- 1 - rss/tss
#rsq#0.456
""
#### spatial exploration camden unit

#calculate the centroids
coords_camden <- joined_model_data1%>%
  st_centroid()%>%
  st_geometry()

plot(coords_camden)

camden_lsoa_nb <- joined_model_data1 %>%
  poly2nb(., queen=T)

#or nearest neighbours
knn_camden_lsoa <-coords_camden %>%
  knearneigh(., k=4)

lsoa_knn_camden <- knn_camden_lsoa %>%
  knn2nb()

#plot them
#plot(bristol_lsoa_nb, st_geometry(coords_bristol), col="red")
#plot(knn_bristol_lsoa, st_geometry(coords_bristol), col="blue")

#create a spatial weights matrix object from these weights

lsoa_camden.queens_weight <- camden_lsoa_nb %>%
  nb2listw(., style="C",zero.policy=TRUE)

lsoa_camden.knn_4_weight <- lsoa_knn_camden %>%
  nb2listw(., style="C",zero.policy=TRUE)

Queen_camden <- joined_model_data1 %>%
  st_drop_geometry()%>%
  dplyr::select(model1resids)%>%
  pull()%>%
  moran.test(., lsoa_camden.queens_weight,zero.policy = TRUE)%>%
  tidy()

Nearest_neighbour_camden <- joined_model_data1 %>%
  st_drop_geometry()%>%
  dplyr::select(model1resids)%>%
  pull()%>%
  moran.test(., lsoa_camden.knn_4_weight)%>%
  tidy()

Queen_camden
Nearest_neighbour_camden

##random forest model 
rfgrid <- expand.grid(.mtry=c(1:3))
control <- trainControl(method="repeatedcv", number=10, repeats=3, search="grid")
set.seed(123)

camden_unit_train_rf <- camden_unit_train %>% select(unit_price,floor_area,population_density,employment_rate,num_of_no_qualification,property_type_S,property_type_T) 
camden_unit_test_rf <- camden_unit_test %>% select(unit_price,floor_area,population_density,employment_rate,num_of_no_qualification,property_type_S,property_type_T) 

rf_grid <- train(camden_unit_train_rf[,-1],log(as.numeric(unlist(camden_unit_train_rf[,1]))), method="rf",ntrees = 100, trControl=control,tuneGrid=rfgrid)
print(rf_grid)#0.359
plot(rf_grid)

rf.pred<-predict(rf_grid$finalModel, camden_unit_test_rf[,-1]) 
rf.pred <- exp(rf.pred)
rss <- sum((rf.pred - as.numeric(unlist(camden_unit_test_rf[,1]))) ^ 2)
tss <- sum((as.numeric(unlist(camden_unit_test_rf[,1])) - mean(as.numeric(unlist(camden_unit_test_rf[,1])))) ^ 2)
rsq <- 1 - rss/tss
rsq#0.208

rf.unit.camden <- randomForest(sqrt(unit_price) ~ floor_area+population_density+employment_rate+num_of_no_qualification+property_type_S+property_type_T, data=camden_unit_train_rf, ntree=100,mtry=2, keep.forest=FALSE,
                                importance=TRUE)
varImpPlot(rf.unit.camden,main=deparse(substitute("Variable importance of the unit price prediction model in Camden")))

impotance = as.data.frame(importance(rf.unit.camden))
impotance = cbind(vars=rownames(impotance), impotance)
impotance = impotance[order(impotance$`%IncMSE`),]
impotance$vars = factor(impotance$vars, levels=unique(impotance$vars))
par(mar=c(5,15,4,1)+.1)
barplot(impotance$`%IncMSE`, names.arg=impotance$vars,horiz=TRUE,las=1,col="#f06359",main="Variable importance of unit price prediction model (Camden)")

rmse(as.numeric(unlist(camden_unit_test_rf[,1])),rf.pred)#3939.659

## neural network
camden_unit_train_ann <- camden_unit_train %>% select(unit_price,floor_area,population_density,employment_rate,num_of_no_qualification,property_type_S,property_type_T) 
camden_unit_test_ann <- camden_unit_test %>% select(unit_price,floor_area,population_density,employment_rate,num_of_no_qualification,property_type_S,property_type_T) 
camden_unit_train_ann.scale <- scale(camden_unit_train_ann)
camden_unit_test_ann.scale <- scale(camden_unit_test_ann)
mygrid <- expand.grid(.decay=c(0.1,0.01,0.001,0.0001, 0.5,0.05,0.005,0.0005), .size=c(3, 4, 5, 6, 7, 8, 9, 10))
set.seed(123)
fitControl <- trainControl("repeatedcv", number = 10, repeats = 3, returnResamp = "all")
housing.nnet.cv <- train(camden_unit_train_ann.scale[,-1],as.numeric(unlist(camden_unit_train_ann.scale[,1])),"nnet", trControl = fitControl, tuneGrid=mygrid)
plot(housing.nnet.cv)
print(housing.nnet.cv)#0.231
housing.nnet.cv$finalModel
summary(housing.nnet.cv$finalModel)
source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')
plot.nnet(housing.nnet.cv$finalModel)

ann.pred<-predict(housing.nnet.cv$finalModel, camden_unit_test_ann.scale[,-1])
rss <- sum((ann.pred - as.numeric(unlist(camden_unit_test_ann.scale[,1]))) ^ 2)
tss <- sum((as.numeric(unlist(camden_unit_test_ann.scale[,1])) - mean(as.numeric(unlist(camden_unit_test_ann.scale[,1])))) ^ 2)
rsq <- 1 - rss/tss
rsq#0.150

rmse(as.numeric(unlist(camden_unit_test_ann.scale[,1])),ann.pred)#0.9429851

"
#unscale not work
NN.nnet <- nnet(bristol_unit_train_ann[,-1], sqrt(as.numeric(unlist(bristol_unit_train_ann[,1]))), decay=0.05, linout = TRUE, size=4)
NN.pred<-predict(NN.nnet, bristol_unit_test_ann) ** 2
NN.pred[,1]
rss <- sum((NN.pred - as.numeric(unlist(bristol_unit_test_ann[,1]))) ^ 2)
tss <- sum((as.numeric(unlist(bristol_unit_test_ann[,1])) - mean(as.numeric(unlist(bristol_unit_test_ann[,1])))) ^ 2)
rsq <- 1 - rss/tss
"

##SVR
camden_unit_train_svr <- camden_unit_train %>% select(unit_price,floor_area,population_density,employment_rate,num_of_no_qualification,property_type_S,property_type_T) 
camden_unit_test_svr <- camden_unit_test %>% select(unit_price,floor_area,population_density,employment_rate,num_of_no_qualification,property_type_S,property_type_T) 
camden_unit_train_svr.scale <- scale(camden_unit_train_svr)
camden_unit_test_svr.scale <- scale(camden_unit_test_svr)

SVRgrid <- expand.grid(.sigma=c(0.001, 0.01, 0.1), .C=c(10,100,1000))
ctrl <- trainControl(method = "cv", number=5)
SVRGridFit <- train(camden_unit_train_svr.scale[,-1],as.numeric(unlist(camden_unit_train_svr.scale[,1])), method="svmRadial", tuneGrid=SVRgrid,epsilon=0.01,
                    trControl=ctrl, type="eps-svr")
SVRGridFit#r2 = 0.286
plot(SVRGridFit)
names(SVRGridFit)
SVRGridFit$finalModel 
SVRGridFit$finalModel@nSV

stforecast <- predict(SVRGridFit$finalModel, camden_unit_test_svr.scale[,-1],type = "response") 
stforecast <- stforecast 
rss <- sum((stforecast - as.numeric(unlist(camden_unit_test_svr.scale[,1]))) ^ 2)
tss <- sum((as.numeric(unlist(camden_unit_test_svr.scale[,1])) - mean(as.numeric(unlist(camden_unit_test_svr.scale[,1])))) ^ 2)
rsq <- 1 - rss/tss
rsq#0.163

rmse(as.numeric(unlist(camden_unit_test_svr.scale[,1])),stforecast)#0.9125611

#### spatial exploration camden total

#calculate the centroids
coords_camden <- joined_model_data2%>%
  st_centroid()%>%
  st_geometry()

plot(coords_camden)

camden_lsoa_nb <- joined_model_data2 %>%
  poly2nb(., queen=T)

#or nearest neighbours
knn_camden_lsoa <-coords_camden %>%
  knearneigh(., k=4)

lsoa_knn_camden <- knn_camden_lsoa %>%
  knn2nb()

#plot them
#plot(bristol_lsoa_nb, st_geometry(coords_bristol), col="red")
#plot(knn_bristol_lsoa, st_geometry(coords_bristol), col="blue")

#create a spatial weights matrix object from these weights

lsoa_camden.queens_weight <- camden_lsoa_nb %>%
  nb2listw(., style="C",zero.policy=TRUE)

lsoa_camden.knn_4_weight <- lsoa_knn_camden %>%
  nb2listw(., style="C",zero.policy=TRUE)

Queen_camden <- joined_model_data2 %>%
  st_drop_geometry()%>%
  dplyr::select(model2resids)%>%
  pull()%>%
  moran.test(., lsoa_camden.queens_weight,zero.policy = TRUE)%>%
  tidy()

Nearest_neighbour_camden <- joined_model_data2 %>%
  st_drop_geometry()%>%
  dplyr::select(model2resids)%>%
  pull()%>%
  moran.test(., lsoa_camden.knn_4_weight)%>%
  tidy()

Queen_camden
Nearest_neighbour_camden

##random forest model 
rfgrid <- expand.grid(.mtry=c(1:6))
control <- trainControl(method="repeatedcv", number=10, repeats=3, search="grid")
set.seed(123)

camden_total_train_rf <- camden_total_train %>% select(total_price,floor_area,population_density, num_of_no_qualification, property_type_S,property_type_T) 
camden_total_test_rf <- camden_total_test %>% select(total_price,floor_area,population_density, num_of_no_qualification, property_type_S,property_type_T) 

rf_grid <- train(camden_total_train_rf[,-1],sqrt(as.numeric(unlist(camden_total_train_rf[,1]))), method="rf",ntrees = 100, trControl=control,tuneGrid=rfgrid)
print(rf_grid)#0.858
plot(rf_grid)

rf.pred<-predict(rf_grid$finalModel, camden_total_test_rf[,-1]) ** 2
rss <- sum((rf.pred - as.numeric(unlist(camden_total_test_rf[,1]))) ^ 2)
tss <- sum((as.numeric(unlist(camden_total_test_rf[,1])) - mean(as.numeric(unlist(camden_total_test_rf[,1])))) ^ 2)
rsq <- 1 - rss/tss
rsq#0.825

rf.total.camden <- randomForest(sqrt(total_price) ~ floor_area+population_density+num_of_no_qualification+property_type_S+property_type_T, data=camden_total_train_rf, ntree=100,mtry=3, keep.forest=FALSE,
                                   importance=TRUE)
varImpPlot(rf.total.camden,main=deparse(substitute("Variable importance of the total price prediction model in Camden")))

impotance = as.data.frame(importance(rf.total.camden))
impotance = cbind(vars=rownames(impotance), impotance)
impotance = impotance[order(impotance$`%IncMSE`),]
impotance$vars = factor(impotance$vars, levels=unique(impotance$vars))
par(mar=c(5,15,4,1)+.1)
barplot(impotance$`%IncMSE`, names.arg=impotance$vars,horiz=TRUE,las=1,col="#f06359",main="Variable importance of total price prediction model (Camden)")

rmse(as.numeric(unlist(camden_total_test_rf[,1])),rf.pred)#566479

#coors_camden_lsoa <- coords_camden %>%
  #as(., "Spatial")

#library(SpatialML)
#rf_spatial <- grf(sqrt(as.numeric(unlist(camden_total_train_rf[,1]))) ~ total_price + floor_area+ population_density+  num_of_no_qualification+  property_type_S+ property_type_T , dframe =camden_total_train_rf, bw=60,kernel="adaptive", coords=coors_camden_lsoa)

#rf.pred<-predict(rf_spatial, camden_total_test_rf[,-1]) ** 2
#rss <- sum((rf.pred - as.numeric(unlist(camden_total_test_rf[,1]))) ^ 2)
#tss <- sum((as.numeric(unlist(camden_total_test_rf[,1])) - mean(as.numeric(unlist(camden_total_test_rf[,1])))) ^ 2)
##rsq <- 1 - rss/tss
#rsq


## neural network
camden_total_train_ann <- camden_total_train %>% select(total_price,floor_area,population_density, num_of_no_qualification, property_type_S,property_type_T) 
camden_total_test_ann <- camden_total_test %>% select(total_price,floor_area,population_density, num_of_no_qualification, property_type_S,property_type_T) 
camden_total_train_ann.scale <- scale(camden_total_train_ann)
camden_total_test_ann.scale <- scale(camden_total_test_ann)
mygrid <- expand.grid(.decay=c(0.1,0.01,0.001,0.0001, 0.5,0.05,0.005,0.0005), .size=c(3, 4, 5, 6, 7, 8, 9, 10))
set.seed(123)
fitControl <- trainControl("repeatedcv", number = 10, repeats = 3, returnResamp = "all")
housing.nnet.cv <- train(camden_total_train_ann.scale[,-1],as.numeric(unlist(camden_total_train_ann.scale[,1])),"nnet", trControl = fitControl, tuneGrid=mygrid)
plot(housing.nnet.cv)
print(housing.nnet.cv)#0.661
housing.nnet.cv$finalModel
summary(housing.nnet.cv$finalModel)
source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')
plot.nnet(housing.nnet.cv$finalModel)

ann.pred<-predict(housing.nnet.cv$finalModel, camden_total_test_ann.scale[,-1])
rss <- sum((ann.pred - as.numeric(unlist(camden_total_test_ann.scale[,1]))) ^ 2)
tss <- sum((as.numeric(unlist(camden_total_test_ann.scale[,1])) - mean(as.numeric(unlist(camden_total_test_ann.scale[,1])))) ^ 2)
rsq <- 1 - rss/tss
rsq#0.419

rmse(as.numeric(unlist(camden_total_test_ann.scale[,1])),ann.pred)#0.7577496

"
#unscale not work
NN.nnet <- nnet(bristol_unit_train_ann[,-1], sqrt(as.numeric(unlist(bristol_unit_train_ann[,1]))), decay=0.05, linout = TRUE, size=4)
NN.pred<-predict(NN.nnet, bristol_unit_test_ann) ** 2
NN.pred[,1]
rss <- sum((NN.pred - as.numeric(unlist(bristol_unit_test_ann[,1]))) ^ 2)
tss <- sum((as.numeric(unlist(bristol_unit_test_ann[,1])) - mean(as.numeric(unlist(bristol_unit_test_ann[,1])))) ^ 2)
rsq <- 1 - rss/tss
"

##SVR
camden_total_train_svr <- camden_total_train %>% select(total_price,floor_area,population_density, num_of_no_qualification, property_type_S,property_type_T) 
camden_total_test_svr <- camden_total_test %>% select(total_price,floor_area,population_density, num_of_no_qualification, property_type_S,property_type_T) 
camden_total_train_svr.scale <- scale(camden_total_train_svr)
camden_total_test_svr.scale <- scale(camden_total_test_svr)

SVRgrid <- expand.grid(.sigma=c(0.001, 0.01, 0.1), .C=c(10,100,1000))
ctrl <- trainControl(method = "cv", number=5)
SVRGridFit <- train(camden_total_train_svr.scale[,-1],as.numeric(unlist(camden_total_train_svr.scale[,1])), method="svmRadial", tuneGrid=SVRgrid,epsilon=0.01,
                    trControl=ctrl, type="eps-svr")
SVRGridFit#r2 = 0.814
plot(SVRGridFit)
names(SVRGridFit)
SVRGridFit$finalModel 
SVRGridFit$finalModel@nSV

stforecast <- predict(SVRGridFit$finalModel, camden_total_test_svr.scale[,-1],type = "response") 
stforecast <- stforecast 
rss <- sum((stforecast - as.numeric(unlist(camden_total_test_svr.scale[,1]))) ^ 2)
tss <- sum((as.numeric(unlist(camden_total_test_svr.scale[,1])) - mean(as.numeric(unlist(camden_total_test_svr.scale[,1])))) ^ 2)
rsq <- 1 - rss/tss
rsq#0.768

rmse(as.numeric(unlist(camden_total_test_svr.scale[,1])),stforecast)#0.4729348


##other model birmingham
#### spatial exploration birmingham total

#calculate the centroids
coords_birmingham <- joined_model_data6%>%
  st_centroid()%>%
  st_geometry()

plot(coords_birmingham)

birmingham_lsoa_nb <- joined_model_data6 %>%
  poly2nb(., queen=T)

#or nearest neighbours
knn_birmingham_lsoa <-coords_birmingham %>%
  knearneigh(., k=4)

lsoa_knn_birmingham <- knn_birmingham_lsoa %>%
  knn2nb()

#plot them
#plot(bristol_lsoa_nb, st_geometry(coords_bristol), col="red")
#plot(knn_bristol_lsoa, st_geometry(coords_bristol), col="blue")

#create a spatial weights matrix object from these weights

lsoa_birmingham.queens_weight <- birmingham_lsoa_nb %>%
  nb2listw(., style="C",zero.policy=TRUE)

lsoa_birmingham.knn_4_weight <- lsoa_knn_birmingham %>%
  nb2listw(., style="C",zero.policy=TRUE)

Queen_birmingham <- joined_model_data6 %>%
  st_drop_geometry()%>%
  dplyr::select(model6resids)%>%
  pull()%>%
  moran.test(., lsoa_birmingham.queens_weight,zero.policy = TRUE)%>%
  tidy()

Nearest_neighbour_birmingham <- joined_model_data6 %>%
  st_drop_geometry()%>%
  dplyr::select(model6resids)%>%
  pull()%>%
  moran.test(., lsoa_birmingham.knn_4_weight)%>%
  tidy()

Queen_birmingham
Nearest_neighbour_birmingham

##random forest model 
rfgrid <- expand.grid(.mtry=c(1:6))
control <- trainControl(method="repeatedcv", number=10, repeats=3, search="grid")
set.seed(123)

birmingham_total_train_rf <- birmingham_total_train %>% select(total_price,floor_area,room_number,num_of_household_with_children,employment_num,num_of_no_qualification,no_car_avalibility,property_type_S,social_rented_tenure_per_household,property_type_T,property_type_F) 
birmingham_total_test_rf <- birmingham_total_test %>% select(total_price,floor_area,room_number,num_of_household_with_children,employment_num,num_of_no_qualification,no_car_avalibility,property_type_S,social_rented_tenure_per_household,property_type_T,property_type_F) 

rf_grid <- train(birmingham_total_train_rf[,-1],sqrt(as.numeric(unlist(birmingham_total_train_rf[,1]))), method="rf",ntrees = 100, trControl=control,tuneGrid=rfgrid)
print(rf_grid)#0.813
plot(rf_grid)

rf.pred<-predict(rf_grid$finalModel, birmingham_total_test_rf[,-1]) ** 2
rss <- sum((rf.pred - as.numeric(unlist(birmingham_total_test_rf[,1]))) ^ 2)
tss <- sum((as.numeric(unlist(birmingham_total_test_rf[,1])) - mean(as.numeric(unlist(birmingham_total_test_rf[,1])))) ^ 2)
rsq <- 1 - rss/tss
rsq#0.830

rmse(as.numeric(unlist(birmingham_total_test_rf[,1])),rf.pred)#47650.84

rf.total.birmingham <- randomForest(sqrt(total_price) ~ floor_area+room_number+num_of_household_with_children+employment_num+num_of_no_qualification+no_car_avalibility+property_type_S+social_rented_tenure_per_household+property_type_T+property_type_F, data=birmingham_total_train_rf, ntree=100,mtry=3, keep.forest=FALSE,
                                   importance=TRUE)
varImpPlot(rf.total.birmingham,main=deparse(substitute("Variable importance of the total price prediction model Birmingham")))


impotance = as.data.frame(importance(rf.total.birmingham))
impotance = cbind(vars=rownames(impotance), impotance)
impotance = impotance[order(impotance$`%IncMSE`),]
impotance$vars = factor(impotance$vars, levels=unique(impotance$vars))
par(mar=c(5,15,4,1)+.1)
barplot(impotance$`%IncMSE`, names.arg=impotance$vars,horiz=TRUE,las=1,col="#f06359",main="Variable importance of total price prediction model (Birmingham)")



#coors_camden_lsoa <- coords_camden %>%
#as(., "Spatial")

#library(SpatialML)
#rf_spatial <- grf(sqrt(as.numeric(unlist(camden_total_train_rf[,1]))) ~ total_price + floor_area+ population_density+  num_of_no_qualification+  property_type_S+ property_type_T , dframe =camden_total_train_rf, bw=60,kernel="adaptive", coords=coors_camden_lsoa)

#rf.pred<-predict(rf_spatial, camden_total_test_rf[,-1]) ** 2
#rss <- sum((rf.pred - as.numeric(unlist(camden_total_test_rf[,1]))) ^ 2)
#tss <- sum((as.numeric(unlist(camden_total_test_rf[,1])) - mean(as.numeric(unlist(camden_total_test_rf[,1])))) ^ 2)
##rsq <- 1 - rss/tss
#rsq


## neural network
birmingham_total_train_ann <- birmingham_total_train %>% select(total_price,floor_area,room_number,num_of_household_with_children,employment_num,num_of_no_qualification,no_car_avalibility,property_type_S,social_rented_tenure_per_household,property_type_T,property_type_F) 
birmingham_total_test_ann <- birmingham_total_test %>% select(total_price,floor_area,room_number,num_of_household_with_children,employment_num,num_of_no_qualification,no_car_avalibility,property_type_S,social_rented_tenure_per_household,property_type_T,property_type_F) 
birmingham_total_train_ann.scale <- scale(birmingham_total_train_ann)
birmingham_total_test_ann.scale <- scale(birmingham_total_test_ann)
mygrid <- expand.grid(.decay=c(0.1,0.01,0.001,0.0001, 0.5,0.05,0.005,0.0005), .size=c(3, 4, 5, 6, 7, 8, 9, 10))
set.seed(123)
fitControl <- trainControl("repeatedcv", number = 10, repeats = 3, returnResamp = "all")
housing.nnet.cv <- train(birmingham_total_train_ann.scale[,-1],as.numeric(unlist(birmingham_total_train_ann.scale[,1])),"nnet", trControl = fitControl, tuneGrid=mygrid)
plot(housing.nnet.cv)
print(housing.nnet.cv)#0.661
housing.nnet.cv$finalModel
summary(housing.nnet.cv$finalModel)
source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')
plot.nnet(housing.nnet.cv$finalModel)

ann.pred<-predict(housing.nnet.cv$finalModel, birmingham_total_test_ann.scale[,-1])
rss <- sum((ann.pred - as.numeric(unlist(birmingham_total_test_ann.scale[,1]))) ^ 2)
tss <- sum((as.numeric(unlist(birmingham_total_test_ann.scale[,1])) - mean(as.numeric(unlist(birmingham_total_test_ann.scale[,1])))) ^ 2)
rsq <- 1 - rss/tss
rsq#0.419

rmse(as.numeric(unlist(birmingham_total_test_ann.scale[,1])),ann.pred)#0.7902602

"
#unscale not work
NN.nnet <- nnet(bristol_unit_train_ann[,-1], sqrt(as.numeric(unlist(bristol_unit_train_ann[,1]))), decay=0.05, linout = TRUE, size=4)
NN.pred<-predict(NN.nnet, bristol_unit_test_ann) ** 2
NN.pred[,1]
rss <- sum((NN.pred - as.numeric(unlist(bristol_unit_test_ann[,1]))) ^ 2)
tss <- sum((as.numeric(unlist(bristol_unit_test_ann[,1])) - mean(as.numeric(unlist(bristol_unit_test_ann[,1])))) ^ 2)
rsq <- 1 - rss/tss
"

##SVR
birmingham_total_train_svr <- birmingham_total_train %>% select(total_price,floor_area,population_density, num_of_no_qualification, property_type_S,property_type_T) 
birmingham_total_test_svr <- birmingham_total_test %>% select(total_price,floor_area,population_density, num_of_no_qualification, property_type_S,property_type_T) 
birmingham_total_train_svr.scale <- scale(birmingham_total_train_svr)
birmingham_total_test_svr.scale <- scale(birmingham_total_test_svr)

SVRgrid <- expand.grid(.sigma=c(0.001, 0.01, 0.1), .C=c(10,100,1000))
ctrl <- trainControl(method = "cv", number=5)
SVRGridFit <- train(birmingham_total_train_svr.scale[,-1],as.numeric(unlist(birmingham_total_train_svr.scale[,1])), method="svmRadial", tuneGrid=SVRgrid,epsilon=0.01,
                    trControl=ctrl, type="eps-svr")
SVRGridFit#r2 = 0.783
plot(SVRGridFit)
names(SVRGridFit)
SVRGridFit$finalModel 
SVRGridFit$finalModel@nSV

stforecast <- predict(SVRGridFit$finalModel, birmingham_total_test_svr.scale[,-1],type = "response") 
stforecast <- stforecast 
rss <- sum((stforecast - as.numeric(unlist(birmingham_total_test_svr.scale[,1]))) ^ 2)
tss <- sum((as.numeric(unlist(birmingham_total_test_svr.scale[,1])) - mean(as.numeric(unlist(birmingham_total_test_svr.scale[,1])))) ^ 2)
rsq <- 1 - rss/tss
rsq#0.751

rmse(as.numeric(unlist(birmingham_total_test_svr.scale[,1])),stforecast)#0.499027

#### spatial exploration birmingham unit

#calculate the centroids
coords_birmingham <- joined_model_data5%>%
  st_centroid()%>%
  st_geometry()

plot(coords_birmingham)

birmingham_lsoa_nb <- joined_model_data5 %>%
  poly2nb(., queen=T)

#or nearest neighbours
knn_birmingham_lsoa <-coords_birmingham %>%
  knearneigh(., k=4)

lsoa_knn_birmingham <- knn_birmingham_lsoa %>%
  knn2nb()

#plot them
#plot(bristol_lsoa_nb, st_geometry(coords_bristol), col="red")
#plot(knn_bristol_lsoa, st_geometry(coords_bristol), col="blue")

#create a spatial weights matrix object from these weights

lsoa_birmingham.queens_weight <- birmingham_lsoa_nb %>%
  nb2listw(., style="C",zero.policy=TRUE)

lsoa_birmingham.knn_4_weight <- lsoa_knn_birmingham %>%
  nb2listw(., style="C",zero.policy=TRUE)

Queen_birmingham <- joined_model_data5 %>%
  st_drop_geometry()%>%
  dplyr::select(model5resids)%>%
  pull()%>%
  moran.test(., lsoa_birmingham.queens_weight,zero.policy = TRUE)%>%
  tidy()

Nearest_neighbour_birmingham <- joined_model_data5 %>%
  st_drop_geometry()%>%
  dplyr::select(model5resids)%>%
  pull()%>%
  moran.test(., lsoa_birmingham.knn_4_weight)%>%
  tidy()

Queen_birmingham
Nearest_neighbour_birmingham

##random forest model 
rfgrid <- expand.grid(.mtry=c(1:6))
control <- trainControl(method="repeatedcv", number=10, repeats=3, search="grid")
set.seed(123)

birmingham_unit_train_rf <- birmingham_unit_train %>% select(unit_price,floor_area,room_number,num_of_household_with_children,employment_num,num_of_no_qualification,no_car_avalibility,property_type_S,social_rented_tenure_per_household,property_type_T,property_type_F) 
birmingham_unit_test_rf <- birmingham_unit_test %>% select(unit_price,floor_area,room_number,num_of_household_with_children,employment_num,num_of_no_qualification,no_car_avalibility,property_type_S,social_rented_tenure_per_household,property_type_T,property_type_F) 

rf_grid <- train(birmingham_unit_train_rf[,-1],sqrt(as.numeric(unlist(birmingham_unit_train_rf[,1]))), method="rf",ntrees = 100, trControl=control,tuneGrid=rfgrid)
print(rf_grid)#0.613
plot(rf_grid)

rf.pred<-predict(rf_grid$finalModel, birmingham_unit_test_rf[,-1]) ** 2
rss <- sum((rf.pred - as.numeric(unlist(birmingham_unit_test_rf[,1]))) ^ 2)
tss <- sum((as.numeric(unlist(birmingham_unit_test_rf[,1])) - mean(as.numeric(unlist(birmingham_unit_test_rf[,1])))) ^ 2)
rsq <- 1 - rss/tss
rsq#0.637

rmse(as.numeric(unlist(birmingham_unit_test_rf[,1])),rf.pred)#465.2253

rf.unit.birmingham <- randomForest(sqrt(unit_price) ~ floor_area+room_number+num_of_household_with_children+employment_num+num_of_no_qualification+no_car_avalibility+property_type_S+social_rented_tenure_per_household+property_type_T+property_type_F, data=birmingham_unit_train_rf, ntree=100,mtry=6, keep.forest=FALSE,
                          importance=TRUE)
varImpPlot(rf.unit.birmingham,main=deparse(substitute("Variable importance of the unit price prediction model Birmingham")))

impotance = as.data.frame(importance(rf.unit.birmingham))
impotance = cbind(vars=rownames(impotance), impotance)
impotance = impotance[order(impotance$`%IncMSE`),]
impotance$vars = factor(impotance$vars, levels=unique(impotance$vars))
par(mar=c(5,15,4,1)+.1)
barplot(impotance$`%IncMSE`, names.arg=impotance$vars,horiz=TRUE,las=1,col="#f06359",main="Variable importance of unit price prediction model (Birmingham)")


#coors_camden_lsoa <- coords_camden %>%
#as(., "Spatial")

#library(SpatialML)
#rf_spatial <- grf(sqrt(as.numeric(unlist(camden_total_train_rf[,1]))) ~ total_price + floor_area+ population_density+  num_of_no_qualification+  property_type_S+ property_type_T , dframe =camden_total_train_rf, bw=60,kernel="adaptive", coords=coors_camden_lsoa)

#rf.pred<-predict(rf_spatial, camden_total_test_rf[,-1]) ** 2
#rss <- sum((rf.pred - as.numeric(unlist(camden_total_test_rf[,1]))) ^ 2)
#tss <- sum((as.numeric(unlist(camden_total_test_rf[,1])) - mean(as.numeric(unlist(camden_total_test_rf[,1])))) ^ 2)
##rsq <- 1 - rss/tss
#rsq


## neural network
birmingham_unit_train_ann <- birmingham_unit_train %>% select(unit_price,floor_area,room_number,num_of_household_with_children,employment_num,num_of_no_qualification,no_car_avalibility,property_type_S,social_rented_tenure_per_household,property_type_T,property_type_F) 
birmingham_unit_test_ann <- birmingham_unit_test %>% select(unit_price,floor_area,room_number,num_of_household_with_children,employment_num,num_of_no_qualification,no_car_avalibility,property_type_S,social_rented_tenure_per_household,property_type_T,property_type_F) 
birmingham_unit_train_ann.scale <- scale(birmingham_unit_train_ann)
birmingham_unit_test_ann.scale <- scale(birmingham_unit_test_ann)
mygrid <- expand.grid(.decay=c(0.1,0.01,0.001,0.0001, 0.5,0.05,0.005,0.0005), .size=c(3, 4, 5, 6, 7, 8, 9, 10))
set.seed(123)
fitControl <- trainControl("repeatedcv", number = 10, repeats = 3, returnResamp = "all")
housing.nnet.cv <- train(birmingham_unit_train_ann.scale[,-1],as.numeric(unlist(birmingham_unit_train_ann.scale[,1])),"nnet", trControl = fitControl, tuneGrid=mygrid)
plot(housing.nnet.cv)
print(housing.nnet.cv)#0.452
housing.nnet.cv$finalModel
summary(housing.nnet.cv$finalModel)
source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')
plot.nnet(housing.nnet.cv$finalModel)

ann.pred<-predict(housing.nnet.cv$finalModel, birmingham_unit_test_ann.scale[,-1])
rss <- sum((ann.pred - as.numeric(unlist(birmingham_unit_test_ann.scale[,1]))) ^ 2)
tss <- sum((as.numeric(unlist(birmingham_unit_test_ann.scale[,1])) - mean(as.numeric(unlist(birmingham_unit_test_ann.scale[,1])))) ^ 2)
rsq <- 1 - rss/tss
rsq#0.303

rmse(as.numeric(unlist(birmingham_unit_test_ann.scale[,1])),ann.pred)#0.8347152

"
#unscale not work
NN.nnet <- nnet(bristol_unit_train_ann[,-1], sqrt(as.numeric(unlist(bristol_unit_train_ann[,1]))), decay=0.05, linout = TRUE, size=4)
NN.pred<-predict(NN.nnet, bristol_unit_test_ann) ** 2
NN.pred[,1]
rss <- sum((NN.pred - as.numeric(unlist(bristol_unit_test_ann[,1]))) ^ 2)
tss <- sum((as.numeric(unlist(bristol_unit_test_ann[,1])) - mean(as.numeric(unlist(bristol_unit_test_ann[,1])))) ^ 2)
rsq <- 1 - rss/tss
"

##SVR
birmingham_unit_train_svr <- birmingham_unit_train %>% select(unit_price,floor_area,room_number,num_of_household_with_children,employment_num,num_of_no_qualification,no_car_avalibility,property_type_S,social_rented_tenure_per_household,property_type_T,property_type_F) 
birmingham_unit_test_svr <- birmingham_unit_test %>% select(unit_price,floor_area,room_number,num_of_household_with_children,employment_num,num_of_no_qualification,no_car_avalibility,property_type_S,social_rented_tenure_per_household,property_type_T,property_type_F) 
birmingham_unit_train_svr.scale <- scale(birmingham_unit_train_svr)
birmingham_unit_test_svr.scale <- scale(birmingham_unit_test_svr)

SVRgrid <- expand.grid(.sigma=c(0.001, 0.01, 0.1), .C=c(10,100,1000))
ctrl <- trainControl(method = "cv", number=5)
SVRGridFit <- train(birmingham_unit_train_svr.scale[,-1],as.numeric(unlist(birmingham_unit_train_svr.scale[,1])), method="svmRadial", tuneGrid=SVRgrid,epsilon=0.01,
                    trControl=ctrl, type="eps-svr")
SVRGridFit#r2 = 0.582
plot(SVRGridFit)
names(SVRGridFit)
SVRGridFit$finalModel 
SVRGridFit$finalModel@nSV

stforecast <- predict(SVRGridFit$finalModel, birmingham_unit_test_svr.scale[,-1],type = "response") 
stforecast <- stforecast 
rss <- sum((stforecast - as.numeric(unlist(birmingham_unit_test_svr.scale[,1]))) ^ 2)
tss <- sum((as.numeric(unlist(birmingham_unit_test_svr.scale[,1])) - mean(as.numeric(unlist(birmingham_unit_test_svr.scale[,1])))) ^ 2)
rsq <- 1 - rss/tss
rsq#0.601
rmse(as.numeric(unlist(birmingham_unit_test_svr.scale[,1])),stforecast)#0.6314978

par(mar = c(5, 4, 4, 6.5) + 0.1, xpd = TRUE)
unit_data <- data.frame(Camden = c(0.065,0.208,0.150,0.163), Bristol=c(0.363,0.517,0.274,0.447),Birmingham=c(0.545,0.637,0.303,0.601))
barplot(as.matrix(unit_data), main="Accuracy of unit price prediction models", ylab="R-squared",xlab = "Local authority", beside=TRUE, width = 0.5,
        col=terrain.colors(4),legend = TRUE)
legend("topright",c("MLR","RF","NN","SVR"), cex=0.8,fill=terrain.colors(4),inset=c(-0.4,0))

par(mar = c(5, 4, 4, 6.5) + 0.1, xpd = TRUE)
total_data <- data.frame(Camden = c(0.610,0.835,0.419,0.768), Bristol=c(0.727,0.827,0.378,0.811),Birmingham=c(0.794,0.830,0.375,0.751))
barplot(as.matrix(total_data), main="Accuracy of total price prediction models", ylab="R-squared",xlab = "Local authority", beside=TRUE, width = 0.5,
        col=terrain.colors(4),legend = TRUE)
legend("topright",c("MLR","RF","NN","SVR"), cex=0.8,fill=terrain.colors(4),inset=c(-0.4,0))

