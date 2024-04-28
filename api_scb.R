library("pxweb")
library("readxl")

d <- pxweb_interactive("api.scb.se")

# QUERY CODE GOTTEN FROM THE PXWEB INTERACTIVE FUNCTION 

# PXWEB query 
pxweb_query_list <- 
  list("Region"=c("00"),
       "Drivmedel"=c("100","110","120","130","140","150","160","190"),
       "ContentsCode"=c("TK1001AA"),
       "Tid"=c("2020M01","2020M02","2020M03","2020M04","2020M05","2020M06","2020M07","2020M08","2020M09","2020M10","2020M11","2020M12","2021M01","2021M02","2021M03","2021M04","2021M05","2021M06","2021M07","2021M08","2021M09","2021M10","2021M11","2021M12","2022M01","2022M02","2022M03","2022M04","2022M05","2022M06","2022M07","2022M08","2022M09","2022M10","2022M11","2022M12","2023M01","2023M02","2023M03","2023M04","2023M05","2023M06","2023M07","2023M08","2023M09","2023M10","2023M11","2023M12","2024M01","2024M02","2024M03"))

# Download data 
px_data <- 
  pxweb_get(url = "https://api.scb.se/OV0104/v1/doris/en/ssd/TK/TK1001/TK1001A/PersBilarDrivMedel",
            query = pxweb_query_list)

# Convert to data.frame 
px_data_frame <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")

# save to csv file
write.csv(px_data_frame, "C:/Users/Bruger/OneDrive/Desktop/Data Science/R programmering/kunskapskontroll/scb_data.csv", row.names = FALSE)


