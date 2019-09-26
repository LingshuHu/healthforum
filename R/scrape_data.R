
#' Scrape one initial post
#'
#' Get the data from one initial post by entering its url
#'
#' @param url URL to the post to scrape
#' @param From The starting page number. Default is the first page
#' @param To The ending page number. Default (Inf) is the last page (so all pages)
#' @param get_user_info Get users' profile information. It includes the date of joining the forum and the total posts they have sent. The default is TRUE.
#'
#' @return A data frame
#'
#' @examples
#' \donttest{
#' ## get two pages of data from the post titled "Can Gastritis be cured?"
#' post_url = "https://patient.info/forums/discuss/can-gastritis-be-cured--613999"
#' scrape_one_post(url = post_url, From = 1, To = 2)
#' }
#'
#' @export
scrape_one_post <- function(url, From = 1L, To = Inf, get_user_info = TRUE) {
  ## examine the validity of the input url
  patient.info_url_post(url)
  page <- xml2::read_html(url)
  page_numbers <- get_page_numbers(page)
  if (length(page_numbers) == 0L) {
    df <- tryCatch(
      get_one_page(url, get_user_info),
      error = function(e) NULL
      )
    ## if null, wait 3 seconds
    if (is.null(df)) {
      message("first atempt failed. Trying again in 3 seconds...\n")
      Sys.sleep(3)
      df <- tryCatch(
        get_one_page(url),
        error = function(e) NULL
      )
      return(df)
    }
  } else {
    if (To < max(page_numbers)) {
      page_numbers <- page_numbers[page_numbers <= To]
    }
    if (From > min(page_numbers)) {
      page_numbers <- page_numbers[page_numbers >= From]
    }
    urls <- sprintf("%s?page=%s", url, page_numbers-1)
    page_list <- tryCatch(
      lapply(urls, function(x) get_one_page(x, get_user_info)),
      error = function(e) NULL
    )
    ## if null, wait 3 seconds
    if (is.null(page_list)) {
      message("first atempt failed. Trying again in 3 seconds...\n")
      Sys.sleep(3)
      page_list <- tryCatch(
        lapply(urls, function(x) get_one_page(x, get_user_info)),
        error = function(e) NULL
      )
      return(page_list)
    }
    df <- do.call("rbind", page_list)
    df <- df[!duplicated(df$posts_id), ]
  }
  return(df)
}


#' Scrape one group
#'
#' Get all the posts containing all the replies to those posts from one group by entering its url.
#'
#' @param group_url URL to the page to scrape.
#' @param random_post_number The number of random posts to scrape. Default is NULL, which means scrape the total number of posts.
#' @param random_seed A random number used to set the random seed to reproduce the work.
#' @param ... optional arguments to FUN.
#'
#' @return A data frame
#'
#' @examples
#' \donttest{
#' ## get the data of 5 random posts from the group "Angiotensin II Receptor Blockers"
#' group_url = "https://patient.info/forums/discuss/browse/angiotensin-ii-receptor-blockers-3037"
#' scrape_one_group(group_url = group_url, random_post_number = 5)
#' }
#'
#' @export
scrape_one_group <- function(group_url, random_post_number = NULL, random_seed = NULL, ...) {
  ## examine the validity of the input url
  patient.info_url_group(group_url)
  ## get all post urls in one topic group
  post_urls <- unlist(get_posts_urls(group_url))
  ## without random_post_number command
  if (is.null(random_post_number)) {
    group_data <- lapply(post_urls, scrape_one_post)
    df <- do.call("rbind", group_data)
    df$group <- sub(".*browse/(.+)-\\d+", "\\1", group_url)
    return(df)
  ## if random_post_number is larger than the total number of posts, just scrape all the posts
  } else if (random_post_number >= length(post_urls)) {
    group_data <- lapply(post_urls, scrape_one_post)
    df <- do.call("rbind", group_data)
    df$group <- sub(".*browse/(.+)-\\d+", "\\1", group_url)
    return(df)
  } else {
    if (!is.null(random_seed)) {
      set.seed(random_seed)
      post_urls_random <- base::sample(post_urls, random_post_number, replace = FALSE)
      group_data <- lapply(post_urls_random, scrape_one_post)
      df <- do.call("rbind", group_data)
      df$group <- sub(".*browse/(.+)-\\d+", "\\1", group_url)
      return(df)
    } else {
      post_urls_random <- base::sample(post_urls, random_post_number, replace = FALSE)
      group_data <- lapply(post_urls_random, scrape_one_post)
      df <- do.call("rbind", group_data)
      df$group <- sub(".*browse/(.+)-\\d+", "\\1", group_url)
      return(df)
    }
  }
}

#' Scrape groups by initial letter
#'
#' Get posts and all the replies to the posts from groups by entering the initial letter of group names
#'
#' @param index The initial letter of groups. Can be one letter or a vector of letters.
#' @param post_number_per_group The number of random posts to scrape per group. Default is NULL, which means scrape the total number of posts in each group
#' @param ... optional arguments to FUN.
#'
#' @return A data frame
#'
#' @examples
#'
#' \donttest{
#' ## Get the posts data of groups whose names starting with the letter "a" and "z"
#' scrape_groups_by_initial_letter(index = "x", post_number_per_group = 1)
#' }
#'
#' @export
scrape_groups_by_initial_letter <- function(index, post_number_per_group = NULL, ...) {
  group_urls <- get_group_urls_by_initial_letter(index)[, 2]
  df <- lapply(group_urls, scrape_one_group, random_post_number = post_number_per_group)
  df <- do.call("rbind", df)
  return(df)
}


#' Scrape groups by category
#'
#' Get posts and all the replies to the posts from groups of a category by entering category name of URL
#'
#' @param cat The category name (lower case, replace space with -) or category URL
#' @param post_number_per_group The number of random posts to scrape per group. Default is NULL, which means scrape the total number of posts in each group
#' @param ... optional arguments to FUN.
#'
#' @return A data frame
#'
#' @examples
#'
#' \donttest{
#' ## Get the posts data of groups whose names starting with the letter "a" and "z"
#' scrape_groups_by_category(cat = "health-promotion", post_number_per_group = 1)
#' cat_url = "https://patient.info/forums/categories/health-promotion-17"
#' scrape_groups_by_category(cat = cat_url, post_number_per_group = 1)
#' }
#'
#' @export
scrape_groups_by_category <- function(cat, post_number_per_group = NULL, ...) {
  stopifnot(is.character(cat))
  cat_names <- get_category_urls()
  if (grepl("^http", cat) && grepl("patient\\.info/forums/categories", cat)) {
    group_urls <- get_group_urls_in_one_category(cat)
    df <- lapply(group_urls, scrape_one_group, random_post_number = post_number_per_group)
    df <- do.call("rbind", df)
    return(df)
  } else if (cat %in% cat_names$cat_names) {
    cat_url <- cat_names[cat_names$cat_names == cat, 2]
    group_urls <- get_group_urls_in_one_category(cat_url)
    df <- lapply(group_urls, scrape_one_group, random_post_number = post_number_per_group)
    df <- do.call("rbind", df)
    return(df)
  } else {
    stop("Input error: input should be a cateogry name or a category URL")
  }
}


#' Scrape a user's posts
#'
#' Get all posts/replies one user has sent by his/her user name
#'
#' @param user_profile_url The URL of a user's profile page.
#' @param type Choose a type of posts to be scraped. It includes "replies", i.e., a user's replies to others' posts, or "topic_post", i.e., a user's initial posts. The default is to get "both"
#'
#' @return A data frame
#'
#' @examples
#' user_profile_url <- "https://patient.info/forums/profiles/utgh4k33-1264038"
#' scrape_user_posts(user_profile_url = user_profile_url, type = "both")
#'
#'
#' @export
scrape_user_posts <- function(user_profile_url, type = c("both", "replies", "topic_post")) {
  ## use trycatch function
  try_get_user_reply <- function(url) {
    return(tryCatch(get_user_reply(url),
    error = function(e) NULL))
  }

  try_get_user_topic_post <- function(url) {
    return(tryCatch(get_user_topic_post(url),
                    error = function(e) NULL))
  }

  if (type == "replies") {
    re_urls <- get_re_urls(user_profile_url)
    df_user_reply <- lapply(re_urls, try_get_user_reply)
    df_user_reply <- do.call("rbind", df_user_reply)
    return(df_user_reply)
  }

  if (type == "topic_post") {
    tp_urls <- get_tp_urls(user_profile_url)
    df_user_tpost <- lapply(tp_urls, try_get_user_topic_post)
    df_user_tpost <- do.call("rbind", df_user_tpost)
    return(df_user_tpost[, 7:13])
  }

  if (type == "both") {
    re_urls <- get_re_urls(user_profile_url)
    df_user_reply <- lapply(re_urls, try_get_user_reply)
    df_user_reply <- do.call("rbind", df_user_reply)

    tp_urls <- get_tp_urls(user_profile_url)
    df_user_tpost <- lapply(tp_urls, try_get_user_topic_post)
    df_user_tpost <- do.call("rbind", df_user_tpost)

    df_user_posts <- rbind(df_user_reply, df_user_tpost)
    return(df_user_posts)
  }
}


#' Count medical glossaries
#'
#' Count the number and ratio of medical glossaries used in the text.
#'
#' @param text Input data. Should be character vector or data frame with character
#'   variable of interest named "text".
#'
#' @return A data frame containing the number of medical words, the number of total words,
#'   and the ratio of medical words to total words.
#'
#' @examples
#' ## create a character vector
#' medical_text <- c(
#' "No, it isn't possible to predict anything before the result of your biopsy is received.",
#' "Thank you for the nice reply! Very thoughtful answer that did ease my fears!",
#' "Can't help regards the meds.  Just want to give support.")
#'
#' ## get the medical glossaries counts from a character vector
#' count_medical_terms(text = medical_text)
#'
#' ## creat a data frame with a character vector named "text"
#' df <- data.frame(
#'   id = c(1, 2, 3),
#'   text = c("No, it isn't possible to predict anything before the result
#'             of your biopsy is received.",
#'            "Thank you for the nice reply! Very thoughtful answer that
#'             did ease my fears!",
#'            "Can't help regards the meds.  Just want to give support."),
#'   stringsAsFactors = FALSE
#' )
#'
#' ## get the medical glossaries counts from a data frame with "text" variable
#' count_medical_terms(df)
#'
#' @details
#' The medical glossary dictionary was obtained from Aristotelis P.
#'   <https://github.com/glutanimate/wordlist-medicalterms-en>.
#'   It is based on two prominent medical dictionary projects:
#'   OpenMedSpel by R. Robinson of e-MedTools (Version 2.0.0, released 2014-01-21)
#'   <http://www.e-medtools.com/openmedspel.html> and
#'   MTH-Med-Spel-Chek by Rajasekharan N. of MT-Herald (released 2014-04-02)
#'   <http://mtherald.com/free-medical-spell-checker-for-microsoft-word-custom-dictionary/>.
#'
#'
#' @export
count_medical_terms <- function(text) {
  UseMethod("count_medical_terms")
}

#' @export
count_medical_terms.character <- function(text) {
  ## validate inputs
  stopifnot(is.character(text))
  med_words_count <- word_match(text, medical_words)
  text <- data.frame(text = text, stringsAsFactors = FALSE)
  cbind(text, med_words_count)
}

#' @export
count_medical_terms.factor <- function(text) {
  count_medical_terms(as.character(text))
}

#' @export
count_medical_terms.data.frame <- function(text) {
  ## validate input
  stopifnot("text" %in% names(text))
  count <- count_medical_terms(text$text)[, 2:4]
  cbind(text, count)
}

#' @export
count_medical_terms.list <- function(text) {
  ## validate input
  stopifnot(is.list(text))
  text <- data.frame(text = unlist(text), stringsAsFactors = FALSE)
  count_medical_terms(text)
}




