require(magrittr)
require(tibble)
require(tidyr)
require(readr)
require(stringr)

wd <- choose.dir(default = "C:/", caption = "Select folder containing data")

a <- readr::read_csv(
  paste0(wd, '\\', "2022-03-16-205842-ELEMNT BOLT 409A-104-0.csv"),
  col_names = TRUE,
  col_types = NULL,
  locale = default_locale(),
  na = c("", "NA"),
  quote = "\"",
  comment = "",
  trim_ws = TRUE,
  skip = 0,
  n_max = Inf,
  progress = show_progress(),
)

not_all_na <- function(x) any(!is.na(x))
field_value_units <- function(x) str_detect(base::colnames(x), pattern = c("Field|Value|Units"))
semicircle_to_deg <- function(x) x * (180 / 2^31) # https://docs.microsoft.com/en-us/previous-versions/windows/embedded/cc510650(v=msdn.10)?redirectedfrom=MSDN
field_is_time_created <- function(x) str_detect(x, "time_created") & !is.na(x)

# # Extract time created
time_created_field_number <- a %>% 
  dplyr::filter(Type == "Data" & Message == "file_id") %>% 
  {. ->> time_created_row} %>% 
  dplyr::select(where(field_is_time_created)) %>% 
  base::colnames() %>% 
  stringr::str_split(., " ") %>% 
  `[[`(1) %>% 
  `[[`(2)

time_created <- time_created_row %>% 
  dplyr::select(paste0("Value ", time_created_field_number)) %>% 
  as.numeric()

b <- a %>% 
  dplyr::filter(Type == "Data" & Message == "record") %>%
  dplyr::select(where(not_all_na))
  # dplyr::select(contains(c("Field", "Values", "Units")))
  

b <- b[, field_value_units(b)]

# # Determine all of the unique Field names, f, available in the dataset
f <- b %>% 
  # # Select columns containing "Field" 
  dplyr::select(contains("Field")) %>% 
  # # Convert to matrix so we can then convert to vector
  as.matrix() %>% 
  # # Convert matrix to single-row vector
  as.vector() %>% 
  # # Grab unique values only (the field names available throughout the original tibble)
  unique() %>% 
  # # Omit NA values (null)
  na.omit()

# # Instantiate empty tibbles
v <- as_tibble(matrix(nrow = base::nrow(b), ncol = length(f), dimnames = list(NULL, f)))
u <- as_tibble(matrix(nrow = 1, ncol = length(f), dimnames = list(NULL, f)))
v[,] <- 123456789
u[,] <- 'UNITLESS'

field_available_matrix <- apply(b, 2, function(x) x %in% f)

for(i in 1:nrow(v)){
  field_array <- as.character(b[i, field_available_matrix[i, ]])
  value_array <- as.numeric(b[i, which(field_available_matrix[i, ]) + 1])
  # value_array <- b[i, which(field_available_matrix[i, ]) + 1]
  if (is.element('UNITLESS', u[,])){
    units_array <- b[i, which(field_available_matrix[i, ]) + 2]
    u[1, field_array] <- as.list(units_array) 
  }
  
  v[i, field_array] <- as.list(value_array)
  # u[i, units_array] <- as.list(units_array)
}

v[v == 123456789] <- NA

v$position_lat_deg <- semicircle_to_deg(v$position_lat)
v$position_long_deg <- semicircle_to_deg(v$position_long) 
v$time_since_start_minutes <- (v$timestamp - v$timestamp[1])/60
v$time_created <- time_created
