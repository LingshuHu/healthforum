

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
  df <- as.data.frame(cbind(posts_id, post_time, types = unlist(types), names,
                            reply_names = unlist(reply_names),
                            likes = unlist(likes), replies = unlist(replies),
                            text))
  df$post_title <- post_title[1]
  return(df)
}


get_page_numbers <- function(x) {
  p <- x %>%
    rvest::html_node(".reply__control.reply-pagination") %>%
    rvest::html_text()
  m <- gregexpr("\\d+(?=/)", p, perl = TRUE)
  as.numeric(regmatches(p, m)[[1]])
}


#' Scrape one initial post
#'
#' Get the data from one initial post by entering its url
#'
#' @param url URL to the page to scrape
#' @param From The starting page number
#' @param To The ending page number
#'
#' @return A data frame
#'
#' @export
scrape_one_post <- function(url, From = 1L, To = length(urls)) {
  page <- xml2::read_html(url)
  page_numbers <- get_page_numbers(page)
  if (length(page_numbers) == 0L) {
    df <- tryCatch(
      get_one_page(url),
      error = function(e) NULL
      )
    ## if null, wait 3 min
    if (is.null(df)) {
      cat("first atempt failed. Trying again in 3 seconds...\n")
      Sys.sleep(3)
      df <- tryCatch(
        get_one_page(url),
        error = function(e) NULL
      )
      return(df)
    }
  } else {
    urls <- sprintf("%s?page=%s", url, page_numbers-1)
    page_list <- tryCatch(
      lapply(urls[From:To], get_one_page),
      error = function(e) NULL
    )
    if (is.null(page_list)) {
      cat("first atempt failed. Trying again in 3 seconds...\n")
      Sys.sleep(3)
      page_list <- tryCatch(
        lapply(urls[From:To], get_one_page),
        error = function(e) NULL
      )
      return(page_list)
    }
    df <- do.call("rbind", page_list)
    df <- df[!duplicated(df$posts_id), ]
  }
  return(df)
}

get_posts_urls_in_one_topic_page <- function(topic_url) {
  post_urls <- xml2::read_html(topic_url) %>% rvest::html_nodes(".post__title") %>%
    rvest::html_nodes("a") %>% rvest::html_attr("href")
  return(post_urls)
}

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

#' Scrape one group
#'
#' Get all the posts and all the replies to the posts from one group by entering its url
#'
#' @param group_url URL to the page to scrape
#' @param random_post_number Random number of posts to scrape
#'
#' @return A data frame
#'
#' @export
scrape_one_group <- function(group_url, random_post_number = NULL) {
  post_urls <- unlist(get_posts_urls(group_url))
  if (is.null(random_post_number)) {
    group_data <- lapply(post_urls, scrape_one_post)
    df <- do.call("rbind", group_data)
    df$group <- sub(".*/(.+)$", "\\1", group_url)
    return(df)
  } else if (random_post_number >= length(post_urls)) {
    group_data <- lapply(post_urls, scrape_one_post)
    df <- do.call("rbind", group_data)
    df$group <- sub(".*/(.+)$", "\\1", group_url)
    return(df)
  } else {
    post_urls_random <- base::sample(post_urls, random_post_number)
    group_data <- lapply(post_urls_random, scrape_one_post)
    df <- do.call("rbind", group_data)
    df$group <- sub(".*/(.+)$", "\\1", group_url)
    return(df)
  }
}

############# test ##############
group_url <- "https://patient.info/forums/discuss/browse/alzheimer-s-disease-67"
url2 <- "https://patient.info/forums/discuss/terbinafine-loss-of-taste-246577"
sub(".*/(.+)$", "\\1", url)
posts_urls <- get_posts_urls(url)
df <- scrape_one_group(group_url, 10)
df <- scrape_one_post("https://patient.info/forums/discuss/terbinafine-loss-of-taste-246577")

dff <- do.call("rbind", df)
df <- get_one_page("https://patient.info/forums/discuss/terbinafine-loss-of-taste-246577")
title <- xml2::read_html(url2) %>%
  rvest::html_node(".post__title") %>% rvest::html_text(trim = TRUE)

page <- xml2::read_html("https://patient.info/forums/discuss/stomach-pain-and-weightloss-699734")
main_reply <- page %>%
  rvest::html_nodes(".reply__title") %>%
  rvest::html_text()

post_urls <- unlist(get_posts_urls(group_url))
length(post_urls)
post_urls_random <- base::sample(post_urls, 2)
