pacman::p_load("tidyverse","lubridate")

partidas=read_csv("https://raw.githubusercontent.com/henriquepgomide/caRtola/master/data/2018/2018_partidas.csv")

partidas = partidas %>% mutate(date=dmy_hm(date))

partidas = partidas %>% mutate(copa=if_else(date<dmy("14/06/2018"),"antes","depois"))

partidas %>% count(round,copa) %>% print(n=Inf)

partidas %>% filter(copa=="depois",round==3)

rodadas=list()

for(i in 1:38){
  rodadas[[i]]=read_csv(paste0("https://raw.githubusercontent.com/henriquepgomide/caRtola/master/data/2018/rodada-",i,".csv"))
}

map_df(.x=rodadas,~tibble(linha=dim(.x)[1],coluna=dim(.x)[2]),.id="rodada")

colnames(rodadas[[1]])

colnames(rodadas[[5]])

setdiff(colnames(rodadas[[1]]),colnames(rodadas[[5]]))
setdiff(colnames(rodadas[[5]]),colnames(rodadas[[1]]))
