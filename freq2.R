library(tidyverse)
library(magrittr)
library(patchwork)

## `get_data` function ------------------------------------------------------

get_freq <- function(.nomes) {
  
  api <- 'https://servicodados.ibge.gov.br/api/v2/censos/nomes/' 
  
  if (length(.nomes) == 1){
    
    consulta <- glue::glue(api, .nomes) %>% 
      jsonlite::fromJSON(.) 
    
  } else {
    
    inputs <- .nomes %>% 
      paste0(collapse = '|')
    
    consulta <- glue::glue(api, inputs) %>% 
      jsonlite::fromJSON(.) 
    
  }
  
  if(!is.data.frame(consulta)) {
    
    rlang::abort('There is an invalid name!')
    
  }
  
  
  if (length(.nomes) != consulta %$% res %>% length()){
    
    rlang::abort('There is at least one invalid name!')
    
  }
  
  
  consulta
  
}

## `get_freq_UF` function ------------------------------------------------------

get_freqby_UF <- function(.nomes){
  
  
  if(length(.nomes) > 1){
    
    rlang::abort('There is an invalid name!')
    
  }
  
  api <- glue::glue('https://servicodados.ibge.gov.br/api/v2/censos/nomes/{.nomes}?groupBy=UF')
  
  consulta <- api %>% 
    jsonlite::fromJSON()
  
  consulta
}

## `clean_data` function -------------------------------------------------------

clean_data <- function(consulta, .nomes) {
  
  consulta %$% 
    res %>% 
    purrr::set_names(.nomes) %>% 
    tibble::enframe() %>% 
    mutate(
      limpo = map(
        .x = value,
        .f = function(df){
          
          df %>% 
            mutate(
              periodo = readr::parse_number(periodo) %>% 
                map_chr(~ ifelse(
                  test = nchar(.x) <= 5,
                  yes = stringr::str_sub(.x, 1, 4),
                  no = stringr::str_sub(.x, 5, 8))))}
      ))
  
}

## `viewer_plot` ggplot function ---------------------------------------------

sep_plot <- function(df){
  
  df %>% 
    mutate(
      graficos = map2(
        .x = limpo,
        .y = name,
        .f = 
          ~ ggplot(.x, aes(x = periodo, y = frequencia, group = 1)) +
          geom_line(size = 1.2) +
          ylim(0, 200000) +
          theme_minimal() +
          labs(
            x = 'Década',
            y = 'Frequência',
            title = glue::glue('Frequência de nascimentos com o nome {stringr::str_to_title(.y)} por década'),
            subtitle = 'Consulta via `API Nomes - IBGE`',
            caption = 'Fonte: IBGE'))) %$%
    
    reduce(graficos, `+`)
}


## `agg_plot` function -------------------------------------------------------

agg_plot <- function(df, .nomes, .transition = TRUE) {
  
  df %>% 
    purrr::pluck('limpo') %>% 
    purrr::reduce(~ left_join(..., by = 'periodo')) %>%  
    purrr::set_names(c('periodo', .nomes)) %>%
    pivot_longer(
      cols = all_of(.nomes),
      values_to = 'frequencia',
      names_to = 'nome') %>%
    mutate(periodo = lubridate::date(glue::glue('{periodo}-01-01'))) %>% 
    ggplot(aes(periodo, frequencia, color = nome)) +
    geom_line(size = 1.2) +
    theme_minimal() +
    scale_y_log10() +
    {if(.transition) gganimate::transition_reveal(periodo)} +
    labs(
      x = 'Década',
      y = 'Frequência',
      title = glue::glue('Frequência de nascimentos por década'),
      subtitle = 'Consulta via `API Nomes - IBGE`',
      caption = 'Fonte: IBGE')
}
## `to_csv` function ---------------------------------------------------------

to_csv <- function(df, .nomes){
  
  df %>% 
    purrr::pluck('limpo') %>% 
    purrr::reduce(~ left_join(..., by = 'periodo')) %>%  
    purrr::set_names(c('periodo', .nomes)) %>%
    pivot_longer(
      cols = all_of(.nomes),
      values_to = 'frequencia',
      names_to = 'nome') %>% 
    readr::write_csv(glue::glue('consulta_{lubridate::today()}.csv'))
} 


## `to_tibble` function ------------------------------------------------------
to_tibble <- function(df, .nomes){
  
  df %>% 
    purrr::pluck('limpo') %>% 
    purrr::reduce(~ left_join(..., by = 'periodo')) %>%  
    purrr::set_names(c('periodo', .nomes)) %>%
    pivot_longer(
      cols = all_of(.nomes),
      values_to = 'frequencia',
      names_to = 'nome')
}


## api function --------------------------------------------------------------
api_nomes <- function(.nomes, .output = 'tibble', .transition = TRUE){
  
  ## modo de aplicação ---------------------
  
  acceptable_outputs <- c('list', 'tibble', 'csv', 'sep_plot', 'agg_plot')
  
  if (! .output %in% acceptable_outputs){
    
    msg <- glue::glue("
      Supplied `.output` argument: {.output}
      Acceptable `.output` arguments {purrr::reduce(acceptable_outputs, paste, sep = ',' )}")
    
    rlang::abort(msg)
  }
  
  list(
    'list' = function(x) { x %$% limpo %>% purrr::set_names(.nomes)},
    'tibble' = function(x) {to_tibble(x, .nomes)},
    'csv' = function(x) {to_csv(x, .nomes)},
    'sep_plot' = function(x) { sep_plot(x) },
    'agg_plot' = function(x) { agg_plot(x, .nomes, .transition = .transition) }) %>% 
    purrr::pluck(.output) ->
    output_formatter
  
  ## lógica da função --------------------
  
  get_freq(.nomes) %>% 
    clean_data(.nomes) %>% 
    output_formatter()
  
}