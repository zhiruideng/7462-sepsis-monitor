getPatient <- function(idnum) {
  if(is.integer(idnum)) {
    ptid_string <- sprintf("%06d", idnum)
  } else {
    ptid_string <- idnum
  }
  file_name <- sprintf("p%s.psv", ptid_string)
  url <- paste0("https://physionet.org/files/challenge-2019/1.0.0/training/training_setA/", file_name)
  data <- fread(url,
                sep = "|",
                na.strings = c("", "NA", "NaN"),
                showProgress = FALSE) %>%
    tibble() %>%
    select(ICULOS, HR, Temp, Resp, SepsisLabel) %>%
    mutate(across(-Temp, as.integer),
           Temp = as.numeric(Temp),
           PatientID = ptid_string) %>%
    select(PatientID, everything())

  return(data)
}

## Grab the second row of data for n randomly chosen patients 
## (first row is mostly NA's)
initializePatients <- function(n = 50) {
  ptids <- sample(1:10000, n*2, replace = FALSE)
  initial_data <- map_dfr(ptids, getPatient) %>%
    filter(ICULOS == 2) %>%
    mutate(lastUpdate = Sys.time()) %>%
    slice(1:n) %>%
    arrange(PatientID)
  
  return(initial_data)
}

updatePatients <- function(patient_data, update_increment = 60) {
  ptids <- patient_data %>% pull(PatientID) %>% unique()
  lastUpdate <- patient_data %>% pull(lastUpdate) %>% max()
  maxLOS <- patient_data %>% pull(ICULOS) %>% max()
  timediff <- as.numeric(Sys.time() - lastUpdate) # This value will be in minutes
  
  if(timediff < update_increment) {
    message("No patient status updates are available right now. Check back later.")
    return(patient_data)
  }
  
  maxLOS_to_fetch <- maxLOS + floor(timediff/update_increment)
  
  new_data <- map_dfr(ptids, getPatient) %>% filter(ICULOS > maxLOS & ICULOS <= maxLOS_to_fetch) %>%
    mutate(lastUpdate = Sys.time())
  
  updated_data <- bind_rows(patient_data, new_data) %>%
    arrange(PatientID, ICULOS)
  
  return(updated_data)
}

