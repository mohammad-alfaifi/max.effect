
main <- function(){
  raw_stocks_info_file <- '/home/moh/Documents/MAX Effect/Data/Raw/tasi-2017.csv'
  dt <- group_vars(raw_stocks_info_file)

  alphas <- calculate_double_sorted_alpha(dt)

}

