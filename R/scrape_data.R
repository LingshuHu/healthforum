
#' Scrape one initial post
#'
#' Get the data from one initial post by entering its url
#'
#' @param url URL to the post to scrape
#' @param From The starting page number. Default is the first page
#' @param To The ending page number. Default is the last page
#'
#' @return A data frame
#'
#' @example
#' ## get two pages of data from the post titled "Can Gastritis be cured?"
#' scrape_one_post(url = "https://patient.info/forums/discuss/can-gastritis-be-cured--613999",
#'                 From = 1, To = 2)
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
    ## if null, wait 3 seconds
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
    ## if null, wait 3 seconds
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


#' Scrape one group
#'
#' Get all the posts and all the replies to the posts from one group by entering its url
#'
#' @param group_url URL to the page to scrape
#' @param random_post_number The number of random posts to scrape. Default is NULL, which means scrape the total number of posts
#'
#' @return A data frame
#'
#' @example
#' ## get the data of 100 random posts from the group "Abdominal Disorders"
#' scrape_one_group(group_url = "https://patient.info/forums/discuss/browse/abdominal-disorders-3321",
#'                  random_post_number = 100)
#'
#' @export
scrape_one_group <- function(group_url, random_post_number = NULL) {
  ## get all post urls in one topic group
  post_urls <- unlist(get_posts_urls(group_url))
  ## without random_post_number command
  if (is.null(random_post_number)) {
    group_data <- lapply(post_urls, scrape_one_post)
    df <- do.call("rbind", group_data)
    df$group <- sub(".*/(.+)$", "\\1", group_url)
    return(df)
  ## if random_post_number is larger than the total number of posts, just scrape all the posts
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

