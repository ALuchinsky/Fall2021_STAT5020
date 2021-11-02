library(dplyr)
library(zoo)
library(tidyverse)
library(purrr)


print_missing <- function(data_frame, only_missing = T) {
  miss_list <- map(data_frame, ~sum( !is.na(.)))
  miss_df = data.frame(name = names(miss_list))
  miss_df$Present = unlist(miss_list, use.names = F)
  miss_df $ Missing = nrow(data_frame) - miss_df$Present
  miss_df $ MissingPCT = as.integer( 100.*miss_df$Missing/nrow(data_frame))
  miss_df $ Type = unlist( map(data_frame, class), use.names = F)
  miss_df$nUnique = apply(data_frame, 2, function(x) length(unique(x)))
  if(only_missing)
    miss_df <- filter( miss_df, Missing > 0)
  return(miss_df[ order(miss_df$Present, decreasing = F), ])
}
