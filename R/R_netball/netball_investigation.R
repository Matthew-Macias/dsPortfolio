library(tidyverse)
library(readr)

ant_inj <- c("Neck,", "Hip", "Knee,")
post_inj <- c("Buttock", "", "Lumbar,", "Spine,")

pull_smartabase <- function(url, form, start_date, end_date, username = NULL, password = NULL) {
  
  form_no_space <- gsub(" ", "+", utils::URLencode(form, reserved = TRUE), fixed = TRUE)
  
  password <- utils::URLencode(password, reserved = TRUE)
  
  domain <- paste0(
    "", # Hiding main domain for security,
    "/externalcsvreports?app=asc",
    "&formName=", form_no_space,
    "&startTime=", date_string(start_date),
    "&finishTime=", date_string(end_date)
  )
  
  password <- utils::URLencode(password, reserved = TRUE)
  
  url_name <- paste0("https://", username, ":", password, "@", domain)
  
  data_get <- httr::GET(url_name)
  on.exit(url_name)
  
  if (httr::http_error(data_get)) {
    data <- NULL
  } else {
    data <- httr::content(data_get, as = "parsed", encoding = "UTF-8", col_types = cols(
      `Netball Team Journey - Aus Teams` = col_character(),
      `Netball - National Team Event` = col_character(),
      `Netball - National Team Camp` = col_character(),
      `Netball - National Team Event Camp` = col_character(),
      `Netball - COE Event` = col_character(),
      `Netball - COE Camp` = col_character(),
      `Netball - COE Event Camp` = col_character()
    ))
  }
  
  # Use API messages to try and catch causes of error by inspecting RConnect log file
  auth_status <- httr::http_status(data_get)[["category"]]
  auth_message <- paste("Authentication", tolower(httr::http_status(data_get)[["message"]]))
  
  if (auth_status == "Success") {
    if (length(data) == 0) {
      data_message <- paste("Error: could not download '", form, "' form. ",
                            "Please check that 'url', 'form',",
                            "'start_date' and 'end_date' are valid.")
    } else {
      data_message <- paste0(form, " download successful")
    }
    auth_message <- paste0(auth_message, "\n", data_message)
  }
  
  rm(password)
  
  cat(auth_message)
  
  return(data)
}

date_string <- function(date) {
  
  day <- stringr::str_sub(date, -2)
  
  month <- stringr::str_sub(date, -5, -4)
  
  year <- stringr::str_sub(date, 1, 4)
  
  date_output <- paste0(day, month, year)
  
  return(date_output)
}

raw_data <- pull_smartabase(form = "Information Transition", # <--- Insert form name
                username = "", # <--- Insert user name
                password = "", # <--- Insert password (Try not to hardcode, using something like getpass)
                start_date = Sys.Date() - 10*365,
                end_date = Sys.Date() + 10*365 )

injury_data <- raw_data %>%
  filter(Form == 'Injury Record',
         `Row #` == 1,
         !is.na(`Netball - National Team Event`)) %>%
  select(Date, 
         About,
         `Original Injury`,
         `Injury Classification`, 
         `Total Days Injured`,
         `Days No Training`,
         `Days Modified Training`, 
         `Days Full Training`,
         `Netball Time Loss`,
         `Injury Activity`,
         `General Location`,
         `Body Area`,
         `Anatomical Structure`,
         `Dominant Side Injury`,
         `Injury Onset`,
         OSICS,
         `Netball - National Team Event`,
         `Netball - National Team Camp`,
         `Netball - National Team Event Camp`,
         `Date Month`,
         `Date Year`,
         `Current Status`,
         `Netball Period`,
         `Netball Position`,
         `Netball Injury Mechanism`
         ) %>%
  mutate(Group = ifelse(`Netball - National Team Event` == 'Diamonds Team', 'Pre-Tournament Camp', 
                        ifelse(`Netball - National Team Event` == 'Diamonds Squad', 'Camp', 'Tournament')),
         `Netball - National Team Event Camp` = str_trim(gsub('Diamonds Team', "", `Netball - National Team Event Camp`)),
         `Netball - National Team Event Camp` = gsub('Diamonds Squad', 'Camp', `Netball - National Team Event Camp`),
         Identifier = paste(`Netball - National Team Event Camp`, ' - ', `Date Year`), 
         Side = word(`Original Injury`),
         AntPost = ifelse(word(`Original Injury`, start = 2) %in% ant_inj, "Anterior", 
                          ifelse(word(`Original Injury`, start = 2) %in% post_inj, "Posterior",
                                 word(`Original Injury`, start = 2)))) %>%
  filter(AntPost != "Illness,") %>%
  select(-`Netball - National Team Camp`, -`Netball - National Team Event`) %>%
  rename(Event = `Netball - National Team Event Camp`) 
  

maint_data <- raw_data %>%
  filter(Form == 'Maintenance Rx',
         `Row #` == 1,
         !is.na(`Netball - National Team Event`)) %>%
  select(Date,
         About,
         `Maintenance Area`,
         `Netball Team Journey - Aus Teams`,
         `Netball - National Team Event`,
         `Netball - National Team Camp`,
         `Netball - National Team Event Camp`,
         `Date Month`,
         `Date Year`
         ) %>%
  mutate(Group = ifelse(`Netball - National Team Event` == 'Diamonds Team', 'Pre-Tournament Camp', 
                        ifelse(`Netball - National Team Event` == 'Diamonds Squad', 'Camp', 'Tournament')),
         `Netball - National Team Event Camp` = str_trim(gsub('Diamonds Team', "", `Netball - National Team Event Camp`)),
         `Netball - National Team Event Camp` = gsub('Diamonds Squad', 'Camp', `Netball - National Team Event Camp`),
         Identifier = paste(`Netball - National Team Event Camp`, ' - ', `Date Year`)) %>%
  select(-`Netball Team Journey - Aus Teams`, -`Netball - National Team Camp`, -`Netball - National Team Event`) %>%
  rename(Event = `Netball - National Team Event Camp`)


ill_data <- raw_data %>%
  filter(Form == 'Illness Record',
         `Row #` == 1,
         !is.na(`Netball - National Team Event`)) %>%
  select(Date,
         About,
         `Netball Team Journey - Aus Teams`,
         `Netball - National Team Event`,
         `Netball - National Team Camp`,
         `Netball - National Team Event Camp`,
         `Date Month`,
         `Date Year`
         ) %>%
  mutate(Group = ifelse(`Netball - National Team Event` == 'Diamonds Team', 'Pre-Tournament Camp', 
                        ifelse(`Netball - National Team Event` == 'Diamonds Squad', 'Camp', 'Tournament')),
         `Netball - National Team Event Camp` = str_trim(gsub('Diamonds Team', "", `Netball - National Team Event Camp`)),
         `Netball - National Team Event Camp` = gsub('Diamonds Squad', 'Camp', `Netball - National Team Event Camp`),
         Identifier = paste(`Netball - National Team Event Camp`, ' - ', `Date Year`)) %>%
  select(-`Netball Team Journey - Aus Teams`, -`Netball - National Team Camp`, -`Netball - National Team Event`) %>%
  rename(Event = `Netball - National Team Event Camp`)

qtr <- injury_data %>%
  filter(!is.na(`Netball Period`),
         `Netball Period` != 'Not Applicable') %>%
  group_by(`Netball Period`, Event, `Date Year`) %>%
  summarise(n = n())

pos <- injury_data %>%
  filter(!is.na(`Netball Position`),
         `Netball Position` != 'Not Applicable') %>%
  group_by(`Netball Position`) %>%
  summarise(n = n())

X <- c((courtLength/2), (courtLength - 8.5), 8.5, 3, (courtLength - 3), (courtLength - 8.5), 8.5)
Y <- c((courtWidth/2 + 1.5), (courtWidth - 2.5), 2.5, (courtWidth/2), (courtWidth/2), 2.5, (courtWidth - 2.5))

pos_full <- cbind(pos, X, Y)
