library(tidyverse)
library(r2rtf)
library(janitor)
library(Tplyr)

raw_dm <- syngenR::raw_dm
raw_ex <- syngenR::raw_ex
raw_rand <- syngenR::raw_rand


adsl <- raw_dm %>%
  left_join(
    select(raw_rand, subject, rndose), by = "subject"
  ) %>%
  mutate(
    age = age,
    sex = case_when(sex == "male" ~ "Male", sex == "female" ~ "Female" ),
    usubjid = paste0(project, "-", subject),
    ethnic = ethnic,
    race = case_when(
      Race1 == 1 ~ "American Indian or Alaska Native",
      Race2 == 1 ~ "Asian",
      Race3 == 1 ~ "Black or African American",
      Race4 == 1 ~ "Native Hawaiian or Other Pacific Islander",
      Race5 == 1 ~ "White"
    ),
    trta = rndose,
    SAFFL = "Y"
  ) %>%
  select(usubjid, subject, age, sex, race, ethnic, trta, SAFFL)

desc_fmt <- c("n", "Mean (SD)", "Median", "Min - Max")


tbl_adsl <- adsl %>%
  tplyr_table(trta, where = SAFFL == "Y" & !is.na(trta)) %>%
  set_desc_layer_formats(
    "n" = f_str("xx", n),
    "Mean (SD)" = f_str("xx.x (xx.xx)", mean, sd),
    "Median" = f_str("xx.x", median),
    "Min - Max" = f_str("xx.x - xx.x", min, max)
  ) %>%
  add_total_group() %>%
  add_layer(
    group_desc(age, by = "Age (y)")
  ) %>%
  add_layer(
    group_count(sex, by = "Sex")
  ) %>%
  add_layer(
    group_count(race, by = "Race")
  ) %>%
  add_layer(
    group_count(ethnic, by = "Ethnicity")
  )

# create and replace row_label
dbase_tbl2 <- tbl_adsl %>%
  build() %>%
  apply_row_masks(row_breaks = T) %>%
  janitor::clean_names() %>%
  mutate(
    row_label3 = case_when(
      row_label2 == desc_fmt[1] ~ desc_fmt[1],
      row_label2 == desc_fmt[2] ~ desc_fmt[2],
      row_label2 == desc_fmt[3] ~ desc_fmt[3],
      row_label2 == desc_fmt[4] ~ desc_fmt[4],
      row_label2 != "" ~ "n (%)",
      TRUE ~ ""
    ),
    row_label2 = if_else(row_label2 %in% desc_fmt, "", row_label2)
  ) %>%
  select(row_label1, row_label2, row_label3, var1_treatment_a, var1_placebo, var1_total)

# statistics=if_else(row_label2 %in% c("n","Mean (SD)","Median","Min - Max"),row_label2,"N (%)"))%>%

# replace zero percent
dbase_tbl2[dbase_tbl2 == " 0 (  0.0%)"] <- " 0"

# row_label2 = str_replace_all(row_label2, desc_fmt[1] = "")

bigN <- dbase_tbl2 %>% filter(row_label3 == "n") %>%
  select(starts_with("var")) %>%
  distinct(.keep_all = T) %>%
  mutate(across(everything(), ~paste0("(N=", .x, ")"))) %>%
  as.character()


rel_width <- c(1, 1.3, rep(0.6, 4))

colheader <- paste0(
  " Variables | Category | Statistics | Treatment A | Placebo | Overall"
)

colheader2 <- paste(
  "", "", "", bigN[1], bigN[2], bigN[3], sep = " | "
)

rtime <- format(Sys.time(), "%d%b%Y %H:%M")

rtab <- \(x) paste0(rep("\t", x), collapse = "")

rpageheader <- paste0("SYNGENR TEMPLATE FOR DM", rtab(12), "Page \\pagenumber of \\pagefield \nProtocol SYNGEN-01", rtab(12), "\ \ \ \\", rtime)

rtitle <- "Table 14.1.2 \
Summary of Demographics
Safety Population"

# rfootnote <-  ""

rfootnote <- paste0("\n DATASETS: ADSL", rtab(8), "PROGRAM NAME: t-dm.R")
rfooter <- paste0("Page \\b \\pagenumber", " of ", "\\b \\pagefield")

dbase_tbl2 %>%
  rtf_page(
    orientation = "landscape",
    margin = c(0.3, 0.3, 0.5, 0.3, 0.5, 0.5),
    nrow = 29,
    border_first = "single",
    border_last = "single"
  ) %>%
  rtf_page_header(
    text = rpageheader,
    text_justification = "c",
    text_font = 9,
    text_font_size = 10
  ) %>%
  rtf_title(rtitle,
            text_font_size = 12
  ) %>%
  rtf_colheader(
    colheader,
    border_top = "single",
    col_rel_width = rel_width,
    border_left = NULL,
    border_right = NULL,
    text_font = 4,
    text_font_size = 9,
    text_justification = c(rep("l", 3), rep("c", 3))
  ) %>%
  rtf_colheader(
    colheader2,
    border_top = NULL,
    col_rel_width = rel_width,
    border_left = NULL,
    border_right = NULL,
    text_font = 4,
    text_font_size = 9
  ) %>%
  rtf_body(
    border_top = NULL,
    border_left = NULL,
    border_right = NULL,
    text_font = 4,
    text_font_size = 9,
    col_rel_width = rel_width,
    text_justification = c(rep("l", 3), rep("c", 3))
  ) %>%
  rtf_footnote(footnote = rfootnote,
               border_left = NULL,
               border_right = NULL,
               border_bottom = NULL,
               text_justification = "c",
               text_font_size = 12,
               as_table = F) %>%
  rtf_page_footer(text = rfooter, text_font_size = 12) %>%
  rtf_encode(page_footnote = "all") %>%
  write_rtf(paste0("inst/output/Table 14.1.2.rtf"))

# saveRDS(dbase_tbl2, paste0(spath, "/Table 14.1.2_b.rds"))

