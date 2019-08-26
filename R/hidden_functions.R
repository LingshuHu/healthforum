
### likes of post_root get all the likes of nested replies.
## get_likes, get_reply_names, get_replies_and_type are the functions for get_one_page
get_likes <- function(posts, post_action, post_count) {
  if (post_action & post_count) {
    likes <- rvest::html_node(posts, ".post__actions") %>%
      rvest::html_node(".post__count") %>% rvest::html_text()
  } else if (post_action & !post_count) {
    likes <- 0L
  } else {
    likes <- NA
  }
}

get_reply_names <- function(posts, author_recipient) {
  if (author_recipient) {
    reply_names <- rvest::html_node(posts, ".author__recipient") %>% rvest::html_text()
  } else {
    reply_names <- NA
  }
}

get_replies_and_type <- function(posts, class_post_root, class_post) {
  if (class_post_root) {
    replies <- length(
      gregexpr('<article class="post"', posts, fixed = TRUE)[[1]]
      ) # count how many "<article class="post" are in a post
    types <- "reply"
  } else if (class_post) {
    replies <- as.numeric(sub(".*\\s(\\d+)\\sreplies", "\\1",
                                 rvest::html_text(
                                   rvest::html_nodes(posts, ".post__stats")
                                   )[2]
                              )
                          )
    types <- "main_post"
  } else {
    replies <- 0L
    types <- "nested_reply"
  }
  return(c(replies, types))
}

## scrape data from the first page of one post
get_one_page <- function(url) {
  page <- xml2::read_html(url)
  ## get all the posts
  posts <- rvest::html_nodes(page, ".post")
  posts_id <- unlist(purrr::map(rvest::html_attrs(posts), 2))
  ## likes
  post_action = grepl('post__actions', posts, fixed = TRUE)
  post_count = grepl('post__count', posts, fixed = TRUE)
  likes <- base::mapply(get_likes, posts, post_action, post_count) # get_likes is a function
  ## get user names
  names <- rvest::html_text(rvest::html_nodes(page, ".author__name"))
  ## reply to names
  author_recipient <- grepl('author__recipient', posts, fixed = TRUE)
  reply_names <- base::mapply(get_reply_names, posts, author_recipient) # get_reply_names is a function
  ## date and time
  post_time <- rvest::html_attr(rvest::html_nodes(page, "time"), "datetime")
  ## reply count and type
  class_post_root <- grepl('<article class=\"post post__root\"', posts, fixed = TRUE)
  class_post <- grepl('<article class=\"post mb-0"', posts, fixed = TRUE)
  replies_and_type <- base::mapply(get_replies_and_type, posts, class_post_root, class_post)
  ## post title and text
  post_title <- rvest::html_text(rvest::html_node(posts, ".post__title"), trim = TRUE)
  text <- rvest::html_text(rvest::html_nodes(posts, ".post__content"), trim = TRUE)
  ## combine to a dataframe
  df <- data.frame(posts_id = posts_id,
                   post_time = post_time,
                   types = as.character(replies_and_type[2, ]),
                   user_names = names,
                   reply_names = reply_names,
                   likes = as.numeric(likes),
                   replies = as.numeric(replies_and_type[1, ]),
                   text = text,
                   stringsAsFactors = FALSE,
                   check.names = F, fix.empty.names = F)
  df$post_title <- post_title[1]
  return(df)
}


## scrape the total page numbers
get_page_numbers <- function(x) {
  p <- rvest::html_node(x, ".reply__control.reply-pagination") %>%
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
    post_urls <- rvest::html_nodes(topic_page, ".post__title") %>% rvest::html_nodes("a") %>%
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


