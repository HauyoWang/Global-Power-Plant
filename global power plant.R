global_power_plant_data <- read.csv("global_power_plant_database.csv")

# Title: Global Power Plant Database
# Description: A comprehensive, global, open source database of power plants
# Version: 1.2.0
# Release Date: 2019-06-12
# URI: http://datasets.wri.org/dataset/globalpowerplantdatabase
# Copyright: Copyright 2018-2019 World Resources Institute and Data Contributors
# License: Creative Commons Attribution 4.0 International -- CC BY 4.0
# Contact: powerexplorer@wri.org
# Citation: Global Energy Observatory, Google, KTH Royal Institute of Technology in Stockholm, Enipedia, World Resources Institute. 2019. Global Power Plant Database. Published on Resource Watch and Google Earth Engine. http://resourcewatch.org/ https://earthengine.google.com/  

#
#
#
#
#  
plot(global_power_plant_data$capacity_mw~global_power_plant_data$latitude)
install.packages("hexbin")
library("hexbin")
with(global_power_plant_data, plot(hexbin(global_power_plant_data$capacity_mw,global_power_plant_data$latitude), style="centroids"))
with(global_power_plant_data, plot(hexbin(global_power_plant_data$capacity_mw,global_power_plant_data$longitude), style="centroids"))
barplot(global_power_plant_data$capacity_mw)
sunflowerplot(global_power_plant_data[,5:6],col = "gold",seg.col= "gold")

install.packages("RColorBrewer")
library(RColorBrewer)
max(global_power_plant_data$capacity_mw) # 22500
min(global_power_plant_data$capacity_mw) # 1
fuel_tpye_data <- global_power_plant_data[2:29910,c(5,6,7,8,24)]
fuel_tpye_data$primary_fuel 
# 15 Levels: Biomass Coal Cogeneration Gas Geothermal Hydro Nuclear Oil Other ... Wind
fuel_type <- fuel_tpye_data
fuel_type <- matrix(data = NA, nrow = 15, ncol = 5)
?as.matrix
fuel_type <- as.data.frame(fuel_type)
colnames(fuel_type) <- c("primary_fuel","capacity_mw","latitude","longitude","estimated_generation_gwh")

fuel_type_list <- global_power_plant_data[,8]
fuel_type_list[!duplicated(fuel_type_list)]
fuel_type[,1] <- c("Hydro","Gas","Other","Oil","Wind","Nuclear","Coal","Solar","Waste","Biomass ","Wave and Tidal","Petcoke","Geothermal","Cogeneration","Storage ")

Hydro_no <- grep(pattern="Hydro",global_power_plant_data[,8],ignore.case = T)
Hydro <- global_power_plant_data[Hydro_no,]
fuel_type[1,2] <- mean(Hydro$capacity_mw,na.rm = FALSE)
fuel_type[1,3] <- mean(abs(Hydro$latitude),na.rm = FALSE)
fuel_type[1,4] <- mean(abs(Hydro$longitude),na.rm =TRUE)
fuel_type[1,5] <- mean(Hydro$estimated_generation_gwh,na.rm = TRUE)

Gas_no <- grep(pattern="Gas",global_power_plant_data[,8],ignore.case = T)
Gas <- global_power_plant_data[Gas_no,]
fuel_type[2,2] <- mean(Gas$capacity_mw,na.rm = FALSE)
fuel_type[2,3] <- mean(abs(Gas$latitude),na.rm = FALSE)
fuel_type[2,4] <- mean(abs(Gas$longitude),na.rm =TRUE)
fuel_type[2,5] <- mean(Gas$estimated_generation_gwh,na.rm = TRUE)

Other_no <- grep(pattern="Other",global_power_plant_data[,8],ignore.case = T)
Other <- global_power_plant_data[Other_no,]
fuel_type[3,2] <- mean(Other$capacity_mw,na.rm = FALSE)
fuel_type[3,3] <- mean(abs(Other$latitude),na.rm = FALSE)
fuel_type[3,4] <- mean(abs(Other$longitude),na.rm =TRUE)
fuel_type[3,5] <- mean(Other$estimated_generation_gwh,na.rm = TRUE)

Oil_no <- grep(pattern="Oil",global_power_plant_data[,8],ignore.case = T)
Oil <- global_power_plant_data[Oil_no,]
fuel_type[4,2] <- mean(Oil$capacity_mw,na.rm = FALSE)
fuel_type[4,3] <- mean(abs(Oil$latitude),na.rm = FALSE)
fuel_type[4,4] <- mean(abs(Oil$longitude),na.rm =TRUE)
fuel_type[4,5] <- mean(Oil$estimated_generation_gwh,na.rm = TRUE)

Wind_no <- grep(pattern="Wind",global_power_plant_data[,8],ignore.case = T)
Wind <- global_power_plant_data[Wind_no,]
fuel_type[5,2] <- mean(Wind$capacity_mw,na.rm = FALSE)
fuel_type[5,3] <- mean(abs(Wind$latitude),na.rm = FALSE)
fuel_type[5,4] <- mean(abs(Wind$longitude),na.rm =TRUE)
fuel_type[5,5] <- mean(Wind$estimated_generation_gwh,na.rm = TRUE)

Nuclear_no <- grep(pattern="Nuclear",global_power_plant_data[,8],ignore.case = T)
Nuclear <- global_power_plant_data[Nuclear_no,]
fuel_type[6,2] <- mean(Nuclear$capacity_mw,na.rm = FALSE)
fuel_type[6,3] <- mean(abs(Nuclear$latitude),na.rm = FALSE)
fuel_type[6,4] <- mean(abs(Nuclear$longitude),na.rm =TRUE)
fuel_type[6,5] <- mean(Nuclear$estimated_generation_gwh,na.rm = TRUE)

Coal_no <- grep(pattern="Coal",global_power_plant_data[,8],ignore.case = T)
Coal <- global_power_plant_data[Coal_no,]
fuel_type[7,2] <- mean(Coal$capacity_mw,na.rm = FALSE)
fuel_type[7,3] <- mean(abs(Coal$latitude),na.rm = FALSE)
fuel_type[7,4] <- mean(abs(Coal$longitude),na.rm =TRUE)
fuel_type[7,5] <- mean(Coal$estimated_generation_gwh,na.rm = TRUE)

Solar_no <- grep(pattern="Solar",global_power_plant_data[,8],ignore.case = T)
Solar <- global_power_plant_data[Solar_no,]
fuel_type[8,2] <- mean(Solar$capacity_mw,na.rm = FALSE)
fuel_type[8,3] <- mean(abs(Solar$latitude),na.rm = FALSE)
fuel_type[8,4] <- mean(abs(Solar$longitude),na.rm =TRUE)
fuel_type[8,5] <- mean(Solar$estimated_generation_gwh,na.rm = TRUE)

Waste_no <- grep(pattern="Waste",global_power_plant_data[,8],ignore.case = T)
Waste <- global_power_plant_data[Waste_no,]
fuel_type[9,2] <- mean(Waste$capacity_mw,na.rm = FALSE)
fuel_type[9,3] <- mean(abs(Waste$latitude),na.rm = FALSE)
fuel_type[9,4] <- mean(abs(Waste$longitude),na.rm =TRUE)
fuel_type[9,5] <- mean(Waste$estimated_generation_gwh,na.rm = TRUE)

Biomass_no <- grep(pattern="Biomass",global_power_plant_data[,8],ignore.case = T)
Biomass <- global_power_plant_data[Biomass_no,]
fuel_type[10,2] <- mean(Biomass$capacity_mw,na.rm = FALSE)
fuel_type[10,3] <- mean(abs(Biomass$latitude),na.rm = FALSE)
fuel_type[10,4] <- mean(abs(Biomass$longitude),na.rm =TRUE)
fuel_type[10,5] <- mean(Biomass$estimated_generation_gwh,na.rm = TRUE)

Wave_no <- grep(pattern="Wave and Tidal",global_power_plant_data[,8],ignore.case = T)
Wave <- global_power_plant_data[Wave_no,]
fuel_type[11,2] <- mean(Wave$capacity_mw,na.rm = FALSE)
fuel_type[11,3] <- mean(abs(Wave$latitude),na.rm = FALSE)
fuel_type[11,4] <- mean(abs(Wave$longitude),na.rm =TRUE)
fuel_type[11,5] <- mean(Wave$estimated_generation_gwh,na.rm = TRUE)

Petcoke_no <- grep(pattern="Petcoke",global_power_plant_data[,8],ignore.case = T)
Petcoke <- global_power_plant_data[Petcoke_no,]
fuel_type[12,2] <- mean(Petcoke$capacity_mw,na.rm = FALSE)
fuel_type[12,3] <- mean(abs(Petcoke$latitude),na.rm = FALSE)
fuel_type[12,4] <- mean(abs(Petcoke$longitude),na.rm =TRUE)
fuel_type[12,5] <- mean(Petcoke$estimated_generation_gwh,na.rm = TRUE)

Geothermal_no <- grep(pattern="Geothermal",global_power_plant_data[,8],ignore.case = T)
Geothermal <- global_power_plant_data[Geothermal_no,]
fuel_type[13,2] <- mean(Geothermal$capacity_mw,na.rm = FALSE)
fuel_type[13,3] <- mean(abs(Geothermal$latitude),na.rm = FALSE)
fuel_type[13,4] <- mean(abs(Geothermal$longitude),na.rm =TRUE)
fuel_type[13,5] <- mean(Geothermal$estimated_generation_gwh,na.rm = TRUE)


Cogeneration_no <- grep(pattern="Cogeneration",global_power_plant_data[,8],ignore.case = T)
Cogeneration <- global_power_plant_data[Cogeneration_no,]
fuel_type[14,2] <- mean(Cogeneration$capacity_mw,na.rm = FALSE)
fuel_type[14,3] <- mean(abs(Cogeneration$latitude),na.rm = FALSE)
fuel_type[14,4] <- mean(abs(Cogeneration$longitude),na.rm =TRUE)
fuel_type[14,5] <- mean(Cogeneration$estimated_generation_gwh,na.rm = TRUE)

Storage_no <- grep(pattern="Storage",global_power_plant_data[,8],ignore.case = T)
Storage <- global_power_plant_data[Storage_no,]
fuel_type[15,2] <- mean(Storage$capacity_mw,na.rm = FALSE)
fuel_type[15,3] <- mean(abs(Storage$latitude),na.rm = FALSE)
fuel_type[15,4] <- mean(abs(Storage$longitude),na.rm =TRUE)
fuel_type[15,5] <- mean(Storage$estimated_generation_gwh,na.rm = TRUE)

rownames(fuel_type) <- c("Hydro","Gas","Other","Oil","Wind","Nuclear","Coal","Solar","Waste","Biomass ","Wave and Tidal","Petcoke","Geothermal","Cogeneration","Storage ")

barplot(as.matrix(fuel_type[,2]),
        legend.text=fuel_type[,1],
        args.legend=list(bty="n",horiz=TRUE),
        col=brewer.pal(15,"Set1"),border="white",
        ylim=c(0,22500),ylab="capacity_mw",
        main="Power Plant Capacity")
?barplot
barplot(fuel_type[,2],
        legend.text=fuel_type[,1],
        args.legend=list(bty="n")
        ,col=brewer.pal(15,"Set1"),border="white",
        ylim=c(0,5000),ylab="capacity_mw",
        main="Power Plant Capacity")

fuel_data <- as.data.frame(fuel_type)

fuel_data$capacity_mw <- abs(fuel_data$capacity_mw)
fuel_data=apply(fuel_data,2,as.numeric)
pie(fuel_data[,2],
    labels=fuel_data[,1],
    clockwise=TRUE,
    radius=1,
    col=brewer.pal(7,"Set1"),
    border="white",
    main="Power Plant Capacity")
?pie

hist(fuel_data$capacity_mw, freq=FALSE,
     xlab="Nitrogen Oxide Concentrations",
     main="Distribution of Nitrogen Oxide Concentrations")

pie(Nuclear[,5],
    labels=Nuclear[,1],
    clockwise=TRUE,
    radius=1,
    col=brewer.pal(7,"Set1"),
    border="white",
    main="Nuclear Power Plant Capacity")
?pie
par(Nuclear[,5], no.readonly = TRUE)
