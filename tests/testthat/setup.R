
data_test <- data.frame(
  number = 1:5,
  integer = 6L:10L,
  logical = c(F, F, T, F, T),
  character = letters[1:5],
  factor = factor(letters[6:10]),
  date = as.POSIXct(Sys.time()+1:5)
)

dd <- as_data_dictionary(data_test)



