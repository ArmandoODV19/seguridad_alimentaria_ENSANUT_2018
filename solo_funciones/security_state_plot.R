security_state_plot <- function(x = seguridad_alimentaria, state){
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
    mutate(porcentaje = freq/sta_total) %>%
    ggplot(aes(x = alimentaria, y = porcentaje, fill = alimentaria))+
    geom_col()+
    scale_x_discrete(limits=c("seguridad_alimentaria","inseguridad_leve",
                              "inseguridad_moderada", "inseguridad_severa"))+
    ylab("")+
    xlab("")+
    ylim(0,1)+
    theme_grey()+
    theme(legend.position="none")
}
