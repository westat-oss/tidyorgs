# Use this code to install packages - remove any packages that are already installed
install.packages("openxlsx")
install.packages('countrycode')
install.packages("dyplr")
install.packages("stringr")
install.packages("readxl")


library(dplyr)
library(stringr)
library(readxl)

library(openxlsx)
library(countrycode)

detach("package:tidyorgs", unload = TRUE)

# intall local package
# change the path here
devtools::install_local("C:/Users/Saluja_R/OneDrive - Westat/Desktop/Westat/OSS/Sectoring GH/sectoring/tidyorgs-main/tidyorgs-main", force = TRUE)
devtools::install_local("C:/Users/Saluja_R/OneDrive - Westat/Desktop/Westat/OSS/Sectoring GH/sectoring/diverstidy-main/diverstidy-main", force = TRUE)

# ACADEMIC

academic_tidyorgs_path <- "sectoring/tidyorgs-main/tidyorgs-main/data/academic_institutions.rda"
load(academic_tidyorgs_path)
data_by_carol <- read.csv("C:\\Users\\Saluja_R\\OneDrive - Westat\\Desktop\\Westat\\OSS\\Sectoring_Carol\\Sector_Mapping_Institutions_Patent_Assignees.csv")

nrow(academic_institutions)

data_carol_academic <- data_by_carol %>%
  filter(sector == "Academic") 

check_match <- function(org, catch_terms) {
  terms <- unlist(str_split(catch_terms, "\\|"))
  return(any(str_to_lower(org) %in% str_to_lower(terms)))
}

catch_terms_pattern <- paste(academic_institutions$catch_terms, collapse = "|")
result_carol_academic <- data_carol_academic %>%
  mutate(match_found = str_detect(disambig_assignee_organization, regex(catch_terms_pattern, ignore_case = TRUE)))

# result_carol_academic <- data_carol_academic %>%
#   rowwise() %>%
#   mutate(match_found = any(sapply(academic_institutions$catch_terms, function(ct) check_match(disambig_assignee_organization, ct)))) %>%
#   ungroup()

result_carol_academic <- result_carol_academic %>%
  filter(match_found == FALSE) %>%
  select(disambig_assignee_organization, country) %>%
  mutate(country_full = countrycode(country, "iso2c", "country.name"))


result_carol_academic <- result_carol_academic %>%
  select(disambig_assignee_organization, country_full) %>%
  rename(country = country_full)


existing_cols <- colnames(academic_institutions)


new_rows <- result_carol_academic %>%
  transmute(
    catch_terms = disambig_assignee_organization,
    organization_name = disambig_assignee_organization,
    country = country  # This already has country_full mapped via countrycode
  )


new_rows[setdiff(existing_cols, colnames(new_rows))] <- NA

# Bind new rows to academic_institutions
academic_institutions <- bind_rows(academic_institutions, new_rows)

nrow(academic_institutions)
colnames(academic_institutions)

save(academic_institutions, file = "sectoring/tidyorgs-main/tidyorgs-main/data/academic_institutions.rda")
write.csv(academic_institutions, "sectoring/academic_institutions1.csv", row.names = FALSE)

# BUSINESS

business_tidyorgs_path <- "sectoring/tidyorgs-main/tidyorgs-main/data/business_data.rda"
load(business_tidyorgs_path)

data_carol_business <- data_by_carol %>%
  filter(sector == "Private") 

nrow(business_data)
nrow(data_carol_business)

catch_terms_pattern <- paste(business_data$catch_terms, collapse = "|")
result_carol_business <- data_carol_business %>%
  mutate(match_found = str_detect(disambig_assignee_organization, regex(catch_terms_pattern, ignore_case = TRUE)))


# result_carol_business <- data_carol_business %>%
#   rowwise() %>%
#   mutate(match_found = any(sapply(business_data$catch_terms, function(ct) check_match(disambig_assignee_organization, ct)))) %>%
#   ungroup()

result_carol_business <- result_carol_business %>%
  filter(match_found == FALSE) %>%
  select(disambig_assignee_organization, country) %>%
  mutate(country_full = countrycode(country, "iso2c", "country.name"))


result_carol_business <- result_carol_business %>%
  select(disambig_assignee_organization, country_full) %>%
  rename(country = country_full)


# Get column names from business_data
existing_cols <- colnames(business_data)

# Create new rows with required columns
new_rows <- result_carol_business %>%
  transmute(
    catch_terms = disambig_assignee_organization,
    organization_name = disambig_assignee_organization,
    country = country  # Already mapped
  )

# Fill missing columns with NA
new_rows[setdiff(existing_cols, colnames(new_rows))] <- NA

# Bind new rows to business_data
business_data <- bind_rows(business_data, new_rows)

nrow(business_data)

save(business_data, file = "sectoring/tidyorgs-main/tidyorgs-main/data/business_data.rda")

# GOVERNMENT

government_tidyorgs_path <- "sectoring/tidyorgs-main/tidyorgs-main/data/government_data.rda"
load(government_tidyorgs_path)

data_carol_government <- data_by_carol %>%
  filter(sector == "Government")

nrow(government_data)

catch_terms_pattern <- paste(government_data$catch_terms, collapse = "|")
result_carol_government <- data_carol_government %>%
  mutate(match_found = str_detect(disambig_assignee_organization, regex(catch_terms_pattern, ignore_case = TRUE)))


# result_carol_government <- data_carol_government %>%
#   rowwise() %>%
#   mutate(match_found = any(sapply(government_data$catch_terms, function(ct) check_match(disambig_assignee_organization, ct)))) %>%
#   ungroup()

result_carol_government <- result_carol_government %>%
    filter(match_found == FALSE) %>%
    select(disambig_assignee_organization, country) %>%
    mutate(country_full = countrycode(country, "iso2c", "country.name"))

result_carol_government <- result_carol_government %>%
    select(disambig_assignee_organization, country_full) %>%
    rename(country = country_full)

# Get column names from government_data
existing_cols <- colnames(government_data)

# Create new rows with required columns
new_rows <- result_carol_government %>%
  transmute(
    catch_terms = disambig_assignee_organization,
    organization_name = disambig_assignee_organization,
    country = country  # Already mapped
  )

# Fill missing columns with NA
new_rows[setdiff(existing_cols, colnames(new_rows))] <- NA

# Bind new rows to government_data
government_data <- bind_rows(government_data, new_rows)

nrow(government_data)

save(government_data, file = "sectoring/tidyorgs-main/tidyorgs-main/data/government_data.rda")
