
rm(list = ls())

source("config.R")

subject <- gen_site_subj(seed)

gen_core_ic <- function() {

  # icdat <- data.frame(subject, folder = "SCN") |>
  #   merge(vis, by = c("subject", "folder")) |>
  #   pull(visdt)
  
  icdat <- vis %>% filter(folder == "SCN") %>% pull(visdt) %>% as.Date() - 10
  
  df <- env_to_df(mget(ls()))
  
  return(df)
}

process_ie <- function() {
  df1 <- gen_folder(crfpagename = "Inclusion/Exclusion Form", foldername = "SCREENING", folder = "SCN", size = get_size(data))
  df2 <- gen_core_ic()
  
  df3 <- cbind(subject, df1, df2)
  date_to_char(df3)
}

raw_ie <- process_ie()

names(raw_ie) <- toupper(names(raw_ie))
save(raw_ie, file = "data/raw_ie.rda")

