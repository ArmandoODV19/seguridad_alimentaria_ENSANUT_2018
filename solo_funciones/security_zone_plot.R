security_zone_plot <- function(x = seguridad_alimentaria, zone){
  reg <- x %>%
    select(alimentaria, region) %>%
    filter(region == zone) %>%
    group_by(alimentaria, region) %>%
    count() %>%
    summarise(total = sum(freq))

  reg_total <- reg$total

  x %>%
    filter(region == zone) %>%
    select(alimentaria) %>%
    group_by(alimentaria) %>%
    count() %>%
    mutate(porcentaje = freq/reg_total) %>%
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
