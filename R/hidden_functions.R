
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
    # count how many "<article class="post" are in a post
    matches <- gregexpr('<article class="post"', posts, fixed = TRUE)[[1]]
    if (matches[1] > 0L) {
      replies <- length(matches)
    } else {
      replies <- 0L
    }
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

## get users' information
get_users_information <- function(user_profile_url) {
  profile_page <- xml2::read_html(user_profile_url)
  date_posts <- rvest::html_nodes(profile_page, ".masthead__actions__link") %>%
    rvest::html_text(trim = TRUE)
  join_date <- sub("Joined ", "", date_posts[1], fixed = TRUE) %>%
    as.POSIXct(tryFormats = "%d-%m-%Y")
  posts_num <- as.numeric(sub(" posts", "", date_posts[2], fixed = TRUE))
  user_profile <- data.frame(join_date = join_date, posts_num = posts_num,
                             stringsAsFactors = FALSE)
  return(user_profile)
}

## scrape data from the first page of one post
get_one_page <- function(url, get_user_info = TRUE) {
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
  post_time <- gsub("T|\\+00", " ", post_time) %>%
    as.POSIXct(tryFormats = "%Y-%m-%d %H:%M")

  ## reply count and type
  class_post_root <- grepl('<article class=\"post post__root\"', posts, fixed = TRUE)
  class_post <- grepl('<article class=\"post mb-0"', posts, fixed = TRUE)
  replies_and_type <- base::mapply(get_replies_and_type, posts, class_post_root, class_post)

  ## post title and text
  post_title <- rvest::html_text(rvest::html_node(posts, ".post__title"), trim = TRUE)
  text <- rvest::html_text(rvest::html_nodes(posts, ".post__content"), trim = TRUE)

  ### clean the text
  text <- text %>%
    gsub(pattern = "\n|\r|[\\^]|\\s+", replacement = " ") %>%
    gsub(pattern = "(\\d+ likes)|(\\d+ replies)|Report|Reply", replacement = "") %>%
    stringr::str_trim(side = "both")

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

  ## get users' information
  if (get_user_info) {
    user_profile_urls <- rvest::html_nodes(page, ".author__name") %>% rvest::html_attr("href")
    user_profile_urls <- paste0("https://patient.info", user_profile_urls)
    users_profile <- lapply(user_profile_urls, get_users_information)
    users_profile <- do.call("rbind", users_profile)
    df <- cbind(df, users_profile)
    return(df)
  } else {
    return(df)
    }
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


## get a user's reply information from one topic post
get_user_reply <- function(re_url) {
  page <- xml2::read_html(re_url)
  content_id <- sub(".*commentid=(\\d+)", "\\1", re_url)
  content_id <- sprintf('[id="%s"]', content_id)

  ## get this user's content
  this_user <- rvest::html_node(page, content_id)

  ## get topic post content
  topic_post <- rvest::html_node(page, ".post__main")

  ## get user names
  name <- rvest::html_text(rvest::html_nodes(this_user, ".author__name"))

  ## reply to name
  reply_name <- rvest::html_text(rvest::html_nodes(this_user, ".author__recipient"))

  ## time
  time <- rvest::html_attr(rvest::html_node(this_user, "time"), "datetime")
  time <- gsub("T|\\+00", " ", time) %>%
    as.POSIXct(tryFormats = "%Y-%m-%d %H:%M")

  ## topic post title
  topic_title <- rvest::html_text(rvest::html_node(page, ".post__title"), trim = TRUE)

  ## topic post author
  topic_author <- rvest::html_node(topic_post, ".author__name") %>% rvest::html_text()

  ## topic post time
  topic_post_time <- rvest::html_attr(rvest::html_node(topic_post, "time"), "datetime")
  topic_post_time <- gsub("T|\\+00", " ", topic_post_time) %>%
    as.POSIXct(tryFormats = "%Y-%m-%d %H:%M")

  ## number of topic post likes and replies
  topic_post_content <- rvest::html_node(topic_post, ".post__content") %>%
    rvest::html_nodes("p") %>% rvest::html_text(trim = TRUE)
  topic_post_likes <- sub("(\\d+)\\slikes.*", "\\1", tail(topic_post_content, n = 1))
  topic_post_replies <- sub(".*(\\d+)\\sreplies", "\\1", tail(topic_post_content, n = 1))

  ## topic post text
  topic_post_text <- paste(head(topic_post_content, -1), sep = ' ', collapse = ' ')

  ## number of likes of the reply post
  post_action = grepl('post__actions', this_user, fixed = TRUE)
  post_count = grepl('post__count', this_user, fixed = TRUE)
  likes <- get_likes(posts = this_user, post_action, post_count)

  ## number of replies of the reply post and the type of reply post
  class_post_root <- grepl('<article class=\"post post__root\"', this_user, fixed = TRUE)
  replies_and_type <- get_replies_and_type(posts = this_user,
                                           class_post_root, class_post = FALSE)

  ## text
  text <- this_user %>%
    rvest::html_node(".post__content") %>%
    rvest::html_nodes("p") %>% rvest::html_text(trim = TRUE)
  text <- paste(text, sep = ' ', collapse = ' ')

  df_user_reply <- data.frame(user = name,
                           reply_name = reply_name,
                           time = time,
                           likes = likes,
                           replies = replies_and_type[1],
                           text = text,
                           type = replies_and_type[2],
                           topic_title = topic_title,
                           topic_author = topic_author,
                           topic_post_time = topic_post_time,
                           topic_post_likes = topic_post_likes,
                           topic_post_replies = topic_post_replies,
                           topic_post_text = topic_post_text,
                           stringsAsFactors = FALSE)
  return(df_user_reply)
}

## get a user's all reply urls (re_urls)
get_re_urls <- function(user_profile_url) {
  replies_list_url <- paste0(user_profile_url, "/replies")
  following_list_url <- paste0(user_profile_url, "/discussions/following")

  page1 <- xml2::read_html(replies_list_url)
  page2 <- xml2::read_html(following_list_url)

  re_urls <- rvest::html_nodes(page1, ".recent-list") %>%
    rvest::html_nodes("a") %>% rvest::html_attr("href")
  re_urls <- re_urls[grepl(".*discuss.*", re_urls)]

  re_urls2 <- rvest::html_nodes(page2, "h3") %>%
    rvest::html_nodes("a") %>% rvest::html_attr("href")

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


