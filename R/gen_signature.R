#' Generate signature from x3p file
#'
#' @param path path to x3p file
#' @return signature
#' @import assertthat
#' @importFrom x3ptools read_x3p
#' @importFrom dplyr as_tibble
#' @import grooveFinder
#' @export


gen_signature_from_x3p <- function(folder = NULL, ext = ".x3p$", urllist = NULL, size = NA){

  # Define variables to make CRAN happy
  bullet <- x3p <- cclist <- crosscut <- x <- value <- NULL
  ccdata <- glist <- grooves <- slist <- sigs <- NULL

  b1 <- read_bullet(folder = folder, ext = ".x3p$", urllist = urllist, size = size)

  b1$bullet <- 1
  b1$land <- 1:6

  b1 <- b1 %>% mutate(
    x3p = x3p %>% purrr::map(.f = x3p_m_to_mum)
  )

  b1 <- b1 %>% mutate(
    x3p = x3p %>% purrr::map(.f = function(x) x %>%
                               rotate_x3p(angle = -90) %>%
                               y_flip_x3p())
  )

  b1 <- b1 %>% mutate(
    crosscut = x3p %>% purrr::map_dbl(.f = x3p_crosscut_optimize)
  )
  # now extract the crosscuts
  b1 <- b1 %>% mutate(
    ccdata = purrr::map2(.x = x3p, .y = crosscut,
                         .f = x3p_crosscut)
  )
  crosscuts <- b1 %>% tidyr::unnest(ccdata)
  crosscuts %>%
    ggplot(aes(x = x, y = value)) +
    geom_line() +
    facet_grid(bullet~land, labeller="label_both") +
    theme_bw()

  crosscuts


  b1 <- b1 %>% mutate(
    grooves = ccdata %>%
      purrr::map(.f = cc_locate_grooves, method = "middle",
                 adjust = 30, return_plot = TRUE)
  )

  b1 <- b1 %>% mutate(
    sigs = purrr::map2(
      .x = ccdata, .y = grooves,
      .f = function(x, y) {
        cc_get_signature(
          ccdata = x, grooves = y, span1 = 0.75, span2 = 0.03)
      })
  )

  signatures <- b1 %>%
    select(source, sigs) %>% tidyr::unnest(sigs)
  signatures %>%
    filter(!is.na(sig),!is.na(raw_sig)) %>%
    ggplot(aes(x = x)) +
    geom_line(aes(y = raw_sig), colour = "grey70") +
    geom_line(aes(y = sig), colour = "grey30") +
    facet_wrap(~source, ncol = 6) +
    ylim(c(-5,5)) +
    theme_bw()

  #signatures <- b1 %>%
  #  select(sigs) %>% tidyr::unnest(sigs)

  #return(signatures %>% select(sig))
}

