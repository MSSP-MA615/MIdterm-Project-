library(tidyverse)
library(magrittr)
library(readxl)
strawb <- read_xlsx("strawberries-2022oct30-a.xlsx",col_names = TRUE)

strawb_CA <- strawb[strawb$Year == 2016&strawb$State =="CALIFORNIA",]
strawb2 <- strawb %>% separate(col=`Data Item`,
                               into = c("Strawberries", "items"),
                               sep = "-",
                               fill = "right")

strawb2 %<>% separate(col = `Strawberries`, into = c("Strawberries", "type", "type2"), 
                      sep = ",", fill = "right")



strawb3 <- strawb2 %>% separate(col = `items`, into = c("sale type", "units"), 
                                sep = ",", fill = "right")
strawb4 <- strawb3 %>% select(-c(4, 8:13, 15, 17))
Domain_organic1 <- grep("organic", 
                       strawb4$Domain, 
                       ignore.case = T)

org_rows1 <- Domain_organic1
strawb_organic <- strawb4 %>% slice(org_rows, preserve = FALSE)

strawb_non_organic <- strawb4 %>% filter(!row_number() %in% org_rows)
strawb_non_organic %<>% pivot_wider(names_from = `units`, values_from = `Value`)
a <- strawb_non_organic[strawb_non_organic$Year == 2016,]
b <- a[a$State == "CALIFORNIA",]
strawb_organic %<>% pivot_wider(names_from = `units`, values_from = `Value`)

CA_2016 <- strawb_organic[strawb_organic$Year == 2016 & strawb_organic$State == "CALIFORNIA",]

CA_chem <- strawb_non_organic[strawb_non_organic$State == "CALIFORNIA",]
unique(CA_chem$`Domain Category`)
FL_chem <- strawb_non_organic[strawb_non_organic$State == "FLORIDA",]
unique(FL_chem$`Domain Category`)
length(unique(CA_chem$`Domain Category`)) -  length(unique(FL_chem$`Domain Category`)) 
Domain_not_sp <- grep("NOT SPECIFIED", 
                        FL_chem$`Domain Category`, 
                        ignore.case = T)


