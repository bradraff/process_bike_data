require(magrittr)
require(tibble)
require(tidyr)
require(readr)
require(stringr)
require(ggplot2)

setwd("C:/tool development/bike data/")

a <- readr::read_csv(
  "2022-03-16-205842-ELEMNT BOLT 409A-104-0.csv",
  # "2022-03-16-003243-ELEMNT BOLT 409A-103-0.csv",
  col_names = TRUE,
  col_types = NULL,
  col_select = NULL,
  id = NULL,
  locale = default_locale(),
  na = c("", "NA"),
  # quoted_na = TRUE,
  quote = "\"",
  comment = "",
  trim_ws = TRUE,
  skip = 0,
  n_max = Inf,
  # guess_max = min(1000, n_max),
  name_repair = "unique",
  num_threads = readr_threads(),
  progress = show_progress(),
  show_col_types = should_show_types(),
  skip_empty_rows = TRUE,
  lazy = should_read_lazy()
)

not_all_na <- function(x) any(!is.na(x))
field_value_units <- function(x) str_detect(base::colnames(x), pattern = c("Field|Value|Units"))

b <- a %>% 
  dplyr::filter(Type == "Data" & Message == "record") %>%
  dplyr::select(where(not_all_na))

b <- b[, field_value_units(b)]

# # Run through each row and assign each cell to the appropriate column in a new table
f <- b[,str_detect(base::colnames(b), pattern = c("Field"))]
f <- unique(as.vector(as.matrix(f)))
f <- f[!is.na(f)]

# # Instantiate an empty tibble of NA values
v <- as_tibble(matrix(nrow = base::nrow(b), ncol = length(f), dimnames = list(NULL, f)))
field_available_matrix <- apply(b, 2, function(x) x %in% f)

for(i in 1:nrow(v)){
  field_array <- as.character(b[i, field_available_matrix[i, ]])
  value_array <- type.convert(b[i, which(field_available_matrix[i, ]) + 1], as.is = TRUE)
  units_array <- b[i, which(field_available_matrix[i, ]) + 2]
  v[i, field_array] <- value_array
}

# https://docs.microsoft.com/en-us/previous-versions/windows/embedded/cc510650(v=msdn.10)?redirectedfrom=MSDN
#TODO: 
v$position_lat_deg <- v$position_lat * (180 / 2^31)
v$position_long_deg <- v$position_long * (180 / 2^31) 

ggplot2::ggplot(data = v, aes(timestamp, cadence)) +
  geom_line() + geom_point()

ggplot2::ggplot(data = v, aes(timestamp, heart_rate)) +
  geom_line() + geom_point()

ggplot2::ggplot(data = v, aes(timestamp, altitude)) +
  geom_line() + geom_point()

ggplot2::ggplot(data = v, aes(position_long_deg, position_lat_deg)) +
  geom_point()

## Extra
# b <- Filter(function(x)!all(is.na(x)), a)
# str_detect(base::colnames(b), pattern = paste(c("Field", "Units", "Value"), sep="", collapse = "|"))