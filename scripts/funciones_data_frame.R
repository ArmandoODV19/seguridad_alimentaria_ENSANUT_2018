### funciones para obtener data frames

security_df <- function(x = seguridad_alimentaria){
  x %>%
    select(alimentaria) %>%
    group_by(alimentaria) %>%
    count() %>%
    mutate(porcentaje = freq/44151)
}

security_df()




security_state_df <- function(x = seguridad_alimentaria, state){
  sta <- x %>%
    select(alimentaria, entidad) %>%
    filter(entidad == state) %>%
    group_by(alimentaria, entidad) %>%
    count() %>%
    summarise(total = sum(freq))

  sta_total <- sta$total

  x %>%
    filter(entidad == state) %>%
    select(alimentaria) %>%
    group_by(alimentaria) %>%
    count() %>%
    mutate(porcentaje = freq/sta_total)
}

security_state_df(state = "Morelos")
