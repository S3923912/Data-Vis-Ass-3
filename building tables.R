# data to build the tables
library(shiny)
library(tidyverse)
#NPAC Table 1 - attendance total ybfs (PI 1)
attend_total_raw <- readxl::read_xlsx("data/Preschool Attendance, 2024 Tables 1-3.xlsx", sheet = 2,  range = "A5:H50")
attend_total_formatted <- attend_total_raw%>%
  rename(service_type = `Service type`)%>%
  mutate(
    state = ifelse(grepl("[a-zA-Z]", `Attending 1-359 hours`), `Attending 1-359 hours`, NA),
    state = recode(state,"New South Wales(f)" = "NSW",
                   "Victoria(h)" = "VIC",
                   "Queensland"="QLD",
                   "South Australia" = "SA",
                   "Western Australia" = "WA", 
                   "Tasmania(i)" = "TAS",
                   "Northern Territory" = "NT",
                   "Australian Capital Territory" = "ACT"))%>%
  fill(state, .direction = "down")%>%
  filter(!is.na(service_type))%>%
  mutate(across(contains("Attending"), as.numeric))%>%
  pivot_longer(cols = contains("Attending"), names_to = "attending_hours", values_to = "attend_n")
#cummulative hours
attend_total_cumulative <- attend_total_formatted%>%
  mutate(attending_hours = trimws(gsub("(Attending|hours)", "", attending_hours)),
         y_label = recode(attending_hours, 
                          "600+" = "100%",
                          "540-599" = "90%",
                          "480-539" = "80%",
                          "420-479" = "70%", 
                          "360-419" = "60%",
                          "1-359" = "1%"), 
         at_least_hours = str_extract(attending_hours, "^[0-9]+"), 
         at_least_hours = ifelse(is.na(at_least_hours), "600", at_least_hours),
         at_least_hours = as.numeric(at_least_hours))%>%
  filter(!str_detect(attending_hours, "total"),
         service_type == "Total children in a preschool program", 
         state != "Australia")%>%
  arrange(desc(at_least_hours))%>%
  group_by(service_type, state)%>%
  mutate(cummulative_n = cumsum(attend_n),
         population = "Enrolled")%>%
  ungroup()


#NPAC Table 2 - attendance inidg ybfs (PI 2.2)
attend_indig_raw <- readxl::read_xlsx("data/Preschool Attendance, 2024 Tables 1-3.xlsx", sheet = 3, range = "A5:F50")
attend_indig_formatted <- attend_indig_raw%>%
  rename(service_type = `Service type`)%>%
  mutate(
    state = ifelse(grepl("[a-zA-Z]", `Attending 1-359 hours`), `Attending 1-359 hours`, NA),
    state = recode(state,"New South Wales(f)" = "NSW",
                   "Victoria(h)" = "VIC",
                   "Queensland"="QLD",
                   "South Australia" = "SA",
                   "Western Australia" = "WA", 
                   "Tasmania(i)" = "TAS",
                   "Northern Territory" = "NT",
                   "Australian Capital Territory" = "ACT"),
    `Attending 1-359 hours` = ifelse(grepl("[0-9]+$", `Attending 1-359 hours`), `Attending 1-359 hours`, NA))%>%
  fill(state, .direction = "down")%>%
  filter(!is.na(service_type))%>%
  mutate(across(contains("Attending"), as.numeric))%>%
  pivot_longer(cols = contains("Attending"), names_to = "attending_hours", values_to = "attend_n")
atten_indig_cumulative <- attend_indig_formatted %>%
  mutate(attending_hours = trimws(gsub("(Attending|hours)", "", attending_hours)),
         y_label = recode(attending_hours, 
                          "600+" = "100%",
                          "480-599" = "80%",
                          "360-479" = "60%", 
                          "1-359" = "1%"),
         at_least_hours = str_extract(attending_hours, "^[0-9]+"), 
         at_least_hours = ifelse(is.na(at_least_hours), "600", at_least_hours),
         at_least_hours = as.numeric(at_least_hours))%>%
  filter(!str_detect(attending_hours, "total")&
           service_type == "Total children in a preschool program" & 
           state != "Australia")%>%
  group_by(service_type, state)%>%
  arrange(desc(at_least_hours))%>%
  mutate(cummulative_n = cumsum(attend_n),
         population = "Aboriginal and Torres Strait Islanders")%>%
  ungroup()

#NECCEC Table 17 - attendance disad 4-5 (PI 3.2)
attend_disad_raw <- readxl::read_xlsx("data/Preschool Education, 2024 (Tables 1 to 32).xlsx", sheet = "Table_17",  range = "A5:I32")
attend_disad_formatted <- attend_disad_raw%>%
  rename(col_1 = `Index of Relative Socio-economic Disadvantage(d)(e)`)%>%
  mutate(attending_hours  = ifelse(grepl("Attending", col_1), col_1, NA))%>%
  fill(attending_hours, .direction = "down")%>%
  filter(col_1 == "Quintile 1")%>%
  pivot_longer(cols = -c(col_1, attending_hours), names_to = "state", values_to = "attend_n")%>%
  mutate(state = str_to_upper(gsub("\\([^\\)]*\\)", "", state)))
attend_disad_cummulative <- attend_disad_formatted%>%
  #the neccec tables have hours per week this converts to per 600 hours to match npac attending
  mutate(attending_hours = recode(attending_hours, 
                                  "Attending less than 10 hours" = "<400", 
                                  "Attending 10-14 hours" = "400-560", 
                                  "Attending 15 hours or more" = "600+"),
         y_label = recode(attending_hours, 
                          "600+" = "100%",
                          "400-560" = "66%",
                          "<400" = "1%"),
         at_least_hours = str_extract(attending_hours, "^[0-9]+"), 
         at_least_hours = ifelse(is.na(at_least_hours), "1", at_least_hours),
         at_least_hours = as.numeric(at_least_hours))%>%
  group_by(state)%>%
  arrange(desc(at_least_hours))%>%
  mutate(cummulative_n = cumsum(attend_n),
         population = "Disadvantaged")

#NECCEC Table 30 - enrollment total & indig (PI 1 & PI 2.1)
enrol_indig_total_raw <- readxl::read_xlsx("data/Preschool Education, 2024 (Tables 1 to 32).xlsx", sheet = "Table_30",  range = "A5:Q78")
enrol_indig_total <- enrol_indig_total_raw%>%
  rename(col_1 = names(enrol_indig_total_raw[1]), indig_enrolled = ...3, indig_attend = ...5, total_enrolled = ...16, total_attend = ...17)%>%
  select(col_1, indig_enrolled, indig_attend, total_attend, total_enrolled)%>%
  filter(col_1 %in% c("Total",
                      "New South Wales(i)",
                      "Victoria(j)", 
                      "Queensland(k)", 
                      "South Australia(l)", 
                      "Western Australia(m)", 
                      "Tasmania(n)", 
                      "Australian Capital Territory(h)(p)", 
                      "Northern Territory(o)"))%>%
  mutate(state = ifelse(col_1 != "Total", col_1, NA),
         state = gsub("\\([^\\)]*\\)", "", state),
         state = recode(state, "New South Wales" = "NSW",
                        "Victoria" = "VIC",
                        "Queensland"="QLD",
                        "South Australia" = "SA",
                        "Western Australia" = "WA", 
                        "Tasmania" = "TAS",
                        "Northern Territory" = "NT",
                        "Australian Capital Territory" = "ACT"))%>%
  fill(state, .direction = "down")%>%
  filter(!is.na(indig_enrolled))

enrol_total <- enrol_indig_total%>%
  select(state, total_enrolled)

enrol_indig <- enrol_indig_total%>%
  select(state, indig_enrolled)

#NECCEC Table 16 - enrollment disadvantage (PI 3.1)
enrol_disad_raw <- readxl::read_xlsx("data/Preschool Education, 2024 (Tables 1 to 32).xlsx", sheet = "Table_16",  range = "A5:I41")
enrol_disad <- enrol_disad_raw %>%
  rename(col_1 = `Index of Relative Socio-economic Disadvantage(c)(d)`)%>%
  mutate(enrolled_hours  = ifelse(is.na(NSW), col_1, NA))%>%
  fill(enrolled_hours, .direction = "down")%>%
  filter(enrolled_hours == "Total"& col_1 == "Quintile 1")%>%
  pivot_longer(cols = -c(col_1, enrolled_hours), names_to = "state", values_to = "enrolled_n")%>%
  mutate(state = str_to_upper(gsub("\\([^\\)]*\\)", "", state)))

#SSYBFS (PI 2.1 & PI 2.2)
ssybfs_raw<- read_csv("data/A5 Population estimates in state-specific YBFS cohorts.csv")
colnames(ssybfs_raw) <- ssybfs_raw[1,]
colnames(ssybfs_raw)[1] <- "population"
ssybfs_pop <- ssybfs_raw[2:4,]%>%
  filter(population %in% c("Aboriginal and Torres Strait Islanders","Total(b)"))%>%
  pivot_longer(cols = -population, names_to = "state", values_to = "ssybfs_erp")%>%
  mutate(state = str_to_upper(state), 
         state = gsub("\\.", "", state),
         ssybfs_erp = gsub(",", "", ssybfs_erp),
         ssybfs_erp = as.numeric(ssybfs_erp), 
         population = gsub("\\([^\\)]*\\)", "", population))

ssybfs_total <- ssybfs_pop%>%
  filter(population == "Total")

ssybfs_indig <- ssybfs_pop%>%
  filter(population == "Aboriginal and Torres Strait Islanders")

#SEIFA (PI 3.1 & 3.2)
seifa_raw<- read_csv("data/state_seifa_ybfs.csv")
colnames(seifa_raw) <- c("state", "seifa_decile", "age", "seifa_erp")
seifa_pop <- seifa_raw%>%
  slice(11:90)%>%
  fill(state, seifa_decile, .direction = "down")%>%
  pivot_wider(names_from = seifa_decile, values_from = seifa_erp)%>%
  mutate(Quintile_1 = `Decile 1` + `Decile 2`)%>%
  filter (age %in% c("4", "5") & !(state %in% c("Total", "Other Territories")))%>%
  group_by(state)%>%
  summarise(disad_erp = sum(Quintile_1))%>%
  mutate(state = recode(state,"New South Wales" = "NSW",
                        "Victoria" = "VIC",
                        "Queensland"="QLD",
                        "South Australia" = "SA",
                        "Western Australia" = "WA", 
                        "Tasmania" = "TAS",
                        "Northern Territory" = "NT",
                        "Australian Capital Territory" = "ACT"))

#PI 1 - attend_total_cumulative/enrol_total

PI1_table <- attend_total_cumulative%>%
  left_join(enrol_total, by = "state")%>%
  mutate(erp = as.numeric(total_enrolled))%>%
  select(state, population,  cummulative_n, erp, at_least_hours, y_label)

enrol_total_pop <- enrol_total%>%
  left_join(ssybfs_total, by = "state")%>%
  rename(enrolled_n = total_enrolled, 
         erp = ssybfs_erp)%>%
  select(state, population, enrolled_n, erp)

attend_total_pop <- attend_total_cumulative%>%
  left_join(ssybfs_total, by = c("state", "population"))%>%
  rename(erp = ssybfs_erp)%>%
  select(state, population, cummulative_n, erp, at_least_hours, y_label)

#PI 2.1 - enrol_indig/ssybfs_pop
PI2.1_table <- enrol_indig%>%
  left_join(ssybfs_indig, by = "state")%>%
  mutate(enrolled_n = as.numeric(indig_enrolled), 
         erp = ssybfs_erp)%>%
  select(state, population, enrolled_n, erp)

#PI 2.2 - atten_indig_cumulative/ssybfs_pop
PI2.2_table <- atten_indig_cumulative%>%
  left_join(ssybfs_indig, by = c("state", "population")) %>%
  rename(erp = ssybfs_erp)%>%
  select(state, population, cummulative_n, erp, at_least_hours, y_label)

#P1 3.1 - enrol_disad/seifa_pop
PI3.1_table <- enrol_disad%>%
  left_join(seifa_pop, by = "state")%>%
  mutate(population = "Disadvantaged")%>%
  rename(erp = disad_erp)%>%
  select(state, population, enrolled_n, erp)

#P1 3.2 - attend_disad_cummulative/seifa_pop
PI3.2_table <- attend_disad_cummulative%>%
  left_join(seifa_pop, by = "state")%>%
  rename(erp = disad_erp)%>%
  select(state, population, cummulative_n, erp, at_least_hours, y_label)

#enrollment_table 
enrolment_table <- rbind(enrol_total_pop, PI2.1_table,PI3.1_table)

#attendance_table 
attendance_table <- rbind(attend_total_pop, PI2.2_table, PI3.2_table)

