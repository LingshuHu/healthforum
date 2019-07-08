
## scrape data from the first page of one post
get_one_page <- function(url) {
  page <- xml2::read_html(url)
  ## get all the posts
  posts <- page %>%
    rvest::html_nodes(".post")
  posts_id <- posts %>%
    rvest::html_attrs() %>%
    purrr::map(2) %>% unlist()
  ## main reply count
  main_reply <- page %>%
    rvest::html_nodes(".reply__title") %>%
    rvest::html_text()
  main_reply <- sub("(\\d+)\\s[A-z]+", "\\1", main_reply) %>% as.numeric()
  ## likes
  likes <- vector("list", length(posts))
  for (i in seq_along(posts)) {
    if (grepl('post__actions', posts[i]) & grepl('post__count', posts[i])) {
      likes[i] <- posts[i] %>% rvest::html_node(".post__actions") %>%
        rvest::html_node(".post__count") %>% rvest::html_text()
    } else if (grepl('post__actions', posts[i]) & !grepl('post__count', posts[i])) {
      likes[i] <- 0L
    } else {
      likes[i] <- NA
    }
  }
  ## get user names
  names <- page %>% rvest::html_nodes(".author__name") %>% rvest::html_text()
  ## reply to names
  reply_names <- vector("list", length(posts))
  for (i in seq_along(posts)) {
    if (grepl('author__recipient', posts[i])) {
      reply_names[i] <- rvest::html_node(posts[i], ".author__recipient") %>% rvest::html_text()
    } else {
      reply_names[i] <- NA
    }
  }
  ## date and time
  post_time <- page %>%
    rvest::html_nodes("time") %>%
    rvest::html_attr("datetime") %>%
    as.POSIXct(format = "%Y-%m-%dT%H:%M+%S:00")
  ## reply count and type
  replies <- vector("list", length(posts))
  types <- vector("list", length(posts))
  for (i in seq_along(posts)) {
    if (grepl('<article class=\"post post__root\"', posts[i])) {
      replies[i] <- length(regmatches(posts[i], gregexpr('<article class="post"', posts[i]))[[1]])
      types[i] <- "reply"
    } else if (grepl('<article class=\"post mb-0"', posts[i])) {
      replies[i] <- main_reply
      types[i] <- "main_post"
    } else {
      replies[i] <- 0L
      types[i] <- "nested_reply"
    }
  }
  post_title <- posts %>% rvest::html_node(".post__title") %>% rvest::html_text(trim = TRUE)
  text <- posts %>% rvest::html_nodes(".post__content") %>% rvest::html_text(trim = TRUE)
  ## combine to a dataframe
  df <- data.frame(posts_id = posts_id,
                   post_time = as.POSIXct(post_time, origin = "1970-01-01"),
                   types = unlist(types),
                   user_names = names,
                   reply_names = unlist(reply_names),
                   likes = as.numeric(unlist(likes)),
                   replies = as.numeric(unlist(replies)),
                   text = text,
                   stringsAsFactors = FALSE)
  df$post_title <- post_title[1]
  return(df)
}

## scrape the total page numbers
get_page_numbers <- function(x) {
  p <- x %>%
    rvest::html_node(".reply__control.reply-pagination") %>%
    rvest::html_text()
  m <- gregexpr("\\d+(?=/)", p, perl = TRUE)
  as.numeric(regmatches(p, m)[[1]])
}

## scrape the post urls from the first page of one topic group
get_posts_urls_in_one_topic_page <- function(topic_url) {
  post_urls <- xml2::read_html(topic_url) %>% rvest::html_nodes(".post__title") %>%
    rvest::html_nodes("a") %>% rvest::html_attr("href")
  return(post_urls)
}

## scrape all the post urls from a topic group
get_posts_urls <- function(topic_url, n1=1, n2=length(topic_urls)) {
  topic_page <- xml2::read_html(topic_url)
  page_numbers<- get_page_numbers(topic_page)
  if (length(page_numbers) == 0L) {
    post_urls <- topic_page %>% rvest::html_nodes(".post__title") %>% rvest::html_nodes("a") %>%
      rvest::html_attr("href")
    post_urls <- lapply(post_urls, function (x) paste0("https://patient.info", x))
  } else {
    topic_urls <- sprintf("%s?page=%s", topic_url, page_numbers-1)
    post_urls <- lapply(topic_urls[n1:n2], get_posts_urls_in_one_topic_page)
    post_urls <- unlist(post_urls)
    post_urls <- lapply(post_urls, function (x) paste0("https://patient.info", x))
  }
  return(post_urls)
}


## get all groups urls in one index page
get_group_urls_in_one_index_page <- function(index_url) {
  group_urls <- xml2::read_html(index_url) %>% rvest::html_nodes(".row-0") %>%
    rvest::html_nodes("a") %>% rvest::html_attr("href")
  group_urls <- paste0("https://patient.info", group_urls)
  return(group_urls)
}

## get all groups urls of one or more innitial letter
get_group_urls_by_initial_letter <- function(index = letters) {
  index_list <- paste0("https://patient.info/forums/index-", index)
  group_urls <- lapply(index_list, get_group_urls_in_one_index_page)
  group_urls <- unlist(group_urls)
  group_names <- sub(".*browse/(.+)-\\d+", "\\1", group_urls)
  groups <- data.frame(group_names = group_names,
                       group_urls = group_urls,
                       stringsAsFactors = FALSE)
  return(groups)
}

## get all groups urls in one category
get_group_urls_in_one_category <- function(cat_url) {
  group_urls <- xml2::read_html(cat_url) %>% rvest::html_nodes(".title") %>%
    rvest::html_nodes("a") %>% rvest::html_attr("href")
  group_urls <- paste0("https://patient.info", group_urls)
  cat_name <- sub(".*categories/(.+)-\\d+", "\\1", cat_url)
  group_names <- sub(".*categories/(.+)-\\d+", "\\1", cat_url)
  return(group_urls)
}

## get category urls
get_category_urls <- function() {
  cat_urls <- xml2::read_html("https://patient.info/forums") %>%
    rvest::html_nodes(".con-meds-lnk") %>%
    rvest::html_attr("href")
  cat_urls <- paste0("https://patient.info", cat_urls)
  cat_names <- sub(".*categories/(.+)-\\d+", "\\1", cat_urls)
  categories <- data.frame(cat_names = cat_names,
                       cat_urls = cat_urls,
                       stringsAsFactors = FALSE)
  return(categories)
}

## function to count words matches in a dictionary
word_match <- function(x, dict) {
  if (is.character(x)) {
    ## this removes URLs
    x <- gsub("https?://\\S+|@\\S+", "", x)
    x <- tokenizers::tokenize_words(
      x, lowercase = TRUE, strip_punct = TRUE, strip_numeric = FALSE
    )
  }
  word_count <- function(token) {
    total_words_count <- length(token)
    med_words_count <- sum(dict$value[match(token, dict$word)], na.rm = TRUE)
    med_words_ratio <- med_words_count/total_words_count
    data.frame(total_words_count = total_words_count,
               med_words_count = med_words_count,
               med_words_ratio = med_words_ratio,
               stringsAsFactors = FALSE)
  }
  count <- lapply(x, word_count)
  count <- do.call("rbind", count)
}
