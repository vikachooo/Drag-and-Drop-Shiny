library(readxl)
library(tidyverse)

HASfull <- read.csv("cleaning_again_HAS_import.csv")
HASimported <- read_excel("cleaned_finalHAS_import2019_translated.xlsx")

ema_medicines_list <- read.csv("C:/Users/VictoriaZaitceva/Desktop/DataGrip/ema_medicines_list.csv")
ema_indication_extension <- read.csv("C:/Users/VictoriaZaitceva/Desktop/DataGrip/ema_indicaton_extensions.csv")



##### BIG DATASET FOR EMA #####

ema_medicines_list_joining <- ema_medicines_list %>% select(2,3,5,9,11:17,25,19)

colnames(ema_medicines_list_joining)

ema_indication_extension_joining <- ema_indication_extension %>% select(7,8,10,12,14,15,16,17,18,19,3,4,22)
colnames(ema_indication_extension_joining)

colnames(ema_medicines_list_joining) <- colnames(ema_indication_extension_joining) #assigning the same column names to both datasets
#short indication for extensions, but for medicines_list it is just indication

# identify if it is extension or not

ema_medicines_list_joining <- ema_medicines_list_joining %>% mutate(extension = FALSE)

ema_medicines_list_joining <- ema_medicines_list_joining %>% mutate(
  generic = ifelse(generic == "no", FALSE, TRUE),
  biosimilar = ifelse(biosimilar == "no", FALSE, TRUE),
  conditional_approval = ifelse(conditional_approval == "no", FALSE, TRUE),
  exceptional_circumstances = ifelse(exceptional_circumstances == "no", FALSE, TRUE),
  accelerated_assessment = ifelse(accelerated_assessment == "no", FALSE, TRUE),
  orphan_status = ifelse(orphan_status == "no", FALSE, TRUE)
  )


ema_indication_extension_joining <- ema_indication_extension_joining %>% mutate(extension = TRUE)

ema_indication_extension_joining <- ema_indication_extension_joining %>% mutate(
  generic = ifelse(generic == "false", FALSE, TRUE),
  biosimilar = ifelse(biosimilar == "false", FALSE, TRUE),
  conditional_approval = ifelse(conditional_approval == "false", FALSE, TRUE),
  exceptional_circumstances = ifelse(exceptional_circumstances == "false", FALSE, TRUE),
  accelerated_assessment = ifelse(accelerated_assessment == "false", FALSE, TRUE),
  orphan_status = ifelse(orphan_status == "false", FALSE, TRUE)
)


full_ema <- rbind(ema_medicines_list_joining, ema_indication_extension_joining)

# remove time  00:00:00 from parsed_date and convert to date format

full_ema$parsed_date <- as.Date(full_ema$parsed_date, format = "%Y-%m-%d")

colnames(full_ema) 
full_ema <- full_ema %>% select(1,3,11,12,14,13,2,4,5,6,7,8,9,10)

# str to low for medicine_name

full_ema$medicine_name <- tolower(full_ema$medicine_name)
full_ema$active_ingredient <- tolower(full_ema$active_ingredient)

# remove text in parentheses in medicine_name

full_ema$medicine_name <- gsub("\\(.*?\\)", "", full_ema$medicine_name)


# sorting by medicine_name and date
full_ema<- full_ema %>% group_by(medicine_name) %>% arrange(medicine_name, parsed_date) %>% ungroup()

# generating id
full_ema <- full_ema %>% mutate(
  id = row_number()
)

colnames(full_ema)[1] <- "brand_name"
colnames(full_ema)[2] <- "molecule_name"
colnames(full_ema)[3] <- "ma_date"
colnames(full_ema)[4] <- "indication_text"

# save

write.csv(full_ema, "full_ema.csv", row.names = FALSE)





# shorter version for working with HAS 


ema2018 <- full_ema %>% filter(ma_date >= "2018-01-01") %>% select(1:6,15)
ema2018 <- ema2018 %>% select(7,1:6)
write.csv(ema2018, "ema2018.csv", row.names = FALSE)


# shorter version for working with HAS

HASimported$brand_name <- tolower(HASimported$brand_name)
HASimported <- HASimported %>% mutate(
  id = row_number()
)

HASshort <- HASimported %>% select(1:5,17)

#convert hta_outcome_date to date format 31/01/2024

HASshort$hta_outcome_date <- as.Date(HASshort$hta_outcome_date, format = "%d/%m/%Y")

HASshort <- HASshort %>% group_by(brand_name) %>% arrange(brand_name, hta_outcome_date) %>% ungroup()

# add submission type

# MA: id, brand, molecule, ma_date, indication_text, extension, manufacturer
# HTA: brand, molecule, hta_outcome_date, indciation_text, submission_type, manufacturer, id, caller_idalso, on top

write.csv(HASshort, "HASshort.csv", row.names = FALSE)
