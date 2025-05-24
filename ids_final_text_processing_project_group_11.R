install.packages(c("rvest", "httr", "stringr", "dplyr", "lubridate", "future", "future.apply"))

library(rvest)
library(httr)
library(stringr)
library(dplyr)
library(lubridate)
library(future)
library(future.apply)

ua <- httr::user_agent("Chrome/134.0.0.0")

plan(multisession)

clean_published_date <- function(raw_text) {
  if (is.na(raw_text) || raw_text == "") return(NA_character_)
  
  match <- str_extract(raw_text, "^[^U]+")
  if (is.na(match)) return(NA_character_)
  
  match <- str_remove(match, "^Published:\\s*")
  match <- str_trim(match)
  
  date_str <- str_extract(match, "\\d{1,2}\\s+[A-Za-z]+\\s+\\d{4}")
  if (is.na(date_str)) return(NA_character_)
  
  parsed_date <- tryCatch(lubridate::dmy(date_str), error = function(e) NA)
  if (is.na(parsed_date)) return(NA_character_)
  
  return(format(parsed_date, "%d/%m/%Y"))
}

get_data <- function(section_url, max_pages, count) {
  all_articles <- list()
  section_name <- str_extract(section_url, "(?<=english/)[a-z\\-]+")
  
  for (page_num in (max_pages - count):(max_pages - 1)) {
    url <- paste0(section_url, page_num)
    # cat("🔄 Reading:", url, "\n")
    
    page <- tryCatch(read_html(GET(url, ua)), error = function(e) {
      message("❌ Failed to load:", url)
      return(NULL)
    })
    
    if (is.null(page)) next
    
    # extract elements
    title <- tryCatch(page %>% html_element("h1") %>% html_text2(), error = function(e) NA_character_)
    ptime_raw <- tryCatch(page %>% html_element(".Ptime") %>% html_text2(), error = function(e) NA_character_)
    ptime <- clean_published_date(ptime_raw)
    
    content_node <- page %>% html_elements("div#content-details") %>% .[1]
    content_text <- tryCatch(html_text2(content_node), error = function(e) NA_character_)
    
    # Check for valid data
    if (!is.null(title) && !is.null(content_text) &&
        is.character(title) && is.character(content_text) &&
        length(title) > 0 && length(content_text) > 0 &&
        !is.na(title) && !is.na(content_text) &&
        nzchar(title) && nzchar(content_text)) {
      
      cleaned_text <- content_text %>%
        str_remove_all("googletag\\.cmd\\.push\\(.*?\\);") %>%
        str_remove_all("\\(adsbygoogle = window\\.adsbygoogle \\|\\| \\[\\]\\)\\.push\\(\\{\\}\\);") %>%
        str_remove_all("\\}\\);") %>%
        str_remove_all(";") %>%
        str_squish()
      
      all_articles[[length(all_articles) + 1]] <- data.frame(
        section = section_name,
        url = url,
        title = title,
        published_date = ptime,
        content = cleaned_text,
        stringsAsFactors = FALSE
      )
    } else {
      cat("⚠️ Skipped page due to missing content or title\n")
    }
    
    # Random delay
    delay <- runif(1, min = 0, max = 0)
    # cat("⏳ Sleeping for", round(delay, 1), "seconds...\n")
    Sys.sleep(delay)
  }
  
  if (length(all_articles) == 0) return(data.frame())
  do.call(rbind, all_articles)
}

section_url_science_technology <- c("https://www.risingbd.com/english/science-technology/news/")
section_url_politics <- c("https://www.risingbd.com/english/politics/news/")
section_url_sports <- c("https://www.risingbd.com/english/sports/news/")
section_url_entertainment <- c("https://www.risingbd.com/english/entertainment/news/")
section_url_business <- c("https://www.risingbd.com/english/business/news/")
section_url_education <- c("https://www.risingbd.com/english/education/news/")
section_url_international <- c("https://www.risingbd.com/english/international/news/")
section_url_interview <- c("https://www.risingbd.com/english/interview/news/")
section_url_country <- c("https://www.risingbd.com/english/country/news/")
section_url_national <- c("https://www.risingbd.com/english/national/news/")

max_pages_science_technology <- 112396
max_pages_politics <- 112225
max_pages_sports <- 112423
max_pages_entertainment <- 112246
max_pages_business <- 112389
max_pages_education <- 112354
max_pages_international <- 112431
max_pages_interview <- 112389
max_pages_country <- 112430
max_pages_national <- 112436

count_1 <- sample(900:1000, 1)
count_2 <- sample(900:1000, 1)
count_3 <- sample(900:1000, 1)
count_4 <- sample(900:1000, 1)
count_5 <- sample(900:1000, 1)
count_6 <- sample(900:1000, 1)
count_7 <- sample(900:1000, 1)
count_8 <- sample(900:1000, 1)
count_9 <- sample(900:1000, 1)
count_10 <- sample(900:1000, 1)

section_data_science_technology <- lapply(section_url_science_technology, function(url) {
  cat("\n📂 Scraping section:", url, "| 🔢 Count:", count_1, "\n")
  get_data(section_url = url, max_pages = max_pages_science_technology, count = count_1)
})

section_data_politics <- lapply(section_url_politics, function(url) {
  cat("\n📂 Scraping section:", url, "| 🔢 Count:", count_2, "\n")
  get_data(section_url = url, max_pages = max_pages_politics, count = count_2)
})

section_data_sports <- lapply(section_url_sports, function(url) {
  cat("\n📂 Scraping section:", url, "| 🔢 Count:", count_3, "\n")
  get_data(section_url = url, max_pages = max_pages_sports, count = count_3)
})

section_data_entertainment <- lapply(section_url_entertainment, function(url) {
  cat("\n📂 Scraping section:", url, "| 🔢 Count:", count_4, "\n")
  get_data(section_url = url, max_pages = max_pages_entertainment, count = count_4)
})

section_data_business <- lapply(section_url_business, function(url) {
  cat("\n📂 Scraping section:", url, "| 🔢 Count:", count_5, "\n")
  get_data(section_url = url, max_pages = max_pages_business, count = count_5)
})

section_data_education <- lapply(section_url_education, function(url) {
  cat("\n📂 Scraping section:", url, "| 🔢 Count:", count_5, "\n")
  get_data(section_url = url, max_pages = max_pages_education, count = count_6)
})

section_data_international <- lapply(section_url_international, function(url) {
  cat("\n📂 Scraping section:", url, "| 🔢 Count:", count_5, "\n")
  get_data(section_url = url, max_pages = max_pages_international, count = count_7)
})

section_data_interview <- lapply(section_url_interview, function(url) {
  cat("\n📂 Scraping section:", url, "| 🔢 Count:", count_5, "\n")
  get_data(section_url = url, max_pages = max_pages_interview, count = count_8)
})

section_data_country <- lapply(section_url_country, function(url) {
  cat("\n📂 Scraping section:", url, "| 🔢 Count:", count_5, "\n")
  get_data(section_url = url, max_pages = max_pages_country, count = count_9)
})

section_data_national <- lapply(section_url_national, function(url) {
  cat("\n📂 Scraping section:", url, "| 🔢 Count:", count_5, "\n")
  get_data(section_url = url, max_pages = max_pages_national, count = count_10)
})

df <- bind_rows(
  section_data_science_technology,
  section_data_politics,
  section_data_sports,
  section_data_entertainment,
  section_data_business,
  section_data_education,
  section_data_international,
  section_data_interview,
  section_data_country,
  section_data_national)

df_clean <- df[!duplicated(df[, c("title", "content")]), ]

cat("Total Data:", nrow(df), "\n")
cat("Duplicates :", nrow(df) - nrow(df_clean), "\n")
cat("Available Data:", nrow(df_clean), "\n")

if (nrow(df_clean) > 0) {
  write.csv(df_clean, "ids_final_project_group_11_news_raw.csv", row.names = FALSE)
  cat("✅ Scraping complete. Data saved to \n")
} else {
  cat("⚠️ No articles were scraped.\n")
}
