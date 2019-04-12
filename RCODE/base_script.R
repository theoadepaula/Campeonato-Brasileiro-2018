pacman::p_load("tidyverse","lubridate","ggthemes")

partidas=read_csv("https://raw.githubusercontent.com/henriquepgomide/caRtola/master/data/2018/2018_partidas.csv")

partidas = partidas %>% mutate(date=dmy_hm(date))

partidas = partidas %>% mutate(copa=if_else(date<dmy("14/06/2018"),"antes","depois"))

partidas %>% count(round,copa) %>% print(n=Inf)

partidas %>% filter(copa=="depois",round==3)

rodadas=list()

for(i in 1:38){
  rodadas[[i]]=read_csv(paste0("https://raw.githubusercontent.com/henriquepgomide/caRtola/master/data/2018/rodada-",i,".csv"))
}

map_df(.x=rodadas_limpas,~tibble(linha=dim(.x)[1],coluna=dim(.x)[2]),.id="rodada") %>%
  mutate(rodada=as.numeric(rodada),copa=if_else(rodada<13,"Antes da Copa","Depois da Copa")) %>%
  ggplot(aes(rodada,linha,fill=copa)) + geom_col()+ expand_limits(x=1)+
  scale_x_continuous(breaks = seq(1,38))+ coord_cartesian(ylim=c(750,860)) +
  scale_y_continuous(breaks = seq(750,860,10))+
  labs(x="Rodada",y="Número de Jogadores", title = "Número de jogadores no Cartola por rodada",
       fill="")+
  theme_tufte()
?labs
rodadas_limpas=map(.x=rodadas,~select(.x,-c(X1,atletas.slug,atletas.foto,atletas.clube_id,atletas.pontos_num,atletas.preco_num,
             atletas.variacao_num,atletas.media_num)))

for (i in 1:4) {
  rodadas_limpas[[i]]=mutate(rodadas_limpas[[i]], PP=NA,DP=NA)
  
}
colnames(rodadas[[1]])
rodadas[[1]][5,c(1,20:25)]

colnames(rodadas[[5]])
rodadas_limpas[[1]] %>%  replace(is.na(.), 0) %>% select(10:25)

rodadas_limpas_1= map(rodadas_limpas,~.x %>% replace(is.na(.), 0))

tab_completa= bind_rows(rodadas_limpas_1)

tab_completa %>% filter(atletas.posicao_id=="gol", atletas.clube.id.full.name=="Corinthians",
                        atletas.apelido=="Cássio") %>% select(atletas.apelido,atletas.clube.id.full.name,
                                                              10:25) %>% print(n=Inf)
tab_completa %>% filter(atletas.posicao_id=="gol", atletas.clube.id.full.name=="Corinthians",
                        atletas.apelido=="Cássio") %>% select(atletas.apelido,atletas.clube.id.full.name,
                                                              DD) %>%
  mutate(DD_dif=c(DD[1],diff(DD))) %>% 
  print(n=Inf)

write_excel_csv2(tab_completa%>%arrange(atletas.atleta_id,atletas.rodada_id),"CSV/campeonato_brasileiro_agregado.csv")

tab_completa %>%View()

tab_completa1=tab_completa %>% arrange(atletas.atleta_id,atletas.rodada_id) %>% group_split(atletas.atleta_id) %>%
map(~.x %>% mutate(
  FC=c(FC[1],diff(FC)),
  FD=c(FD[1],diff(FD)),
  FF=c(FF[1],diff(FF)),
  FS=c(FS[1],diff(FS)),
  G=c(G[1],diff(G)),
  I=c(I[1],diff(I)),
  RB=c(RB[1],diff(RB)),
  CA=c(CA[1],diff(CA)),
  PE=c(PE[1],diff(PE)),
  A=c(A[1],diff(A)),
  SG=c(SG[1],diff(SG)),
  DD=c(DD[1],diff(DD)),
  FT=c(FT[1],diff(FT)),
  GS=c(GS[1],diff(GS)),
  CV=c(CV[1],diff(CV)),
  GC=c(GC[1],diff(GC)),
  PP=c(PP[1],diff(PP)),
  DP=c(DP[1],diff(DP))
)) %>% bind_rows()

tab_completa1[1:38,] %>% View("desagregado")
tab_completa%>%arrange(atletas.atleta_id,atletas.rodada_id)%>% slice(1:38) %>% View("agregado")

write_excel_csv2(tab_completa1,"CSV/campeonato_brasileiro_desagregado.csv")



rm(rodadas,rodadas_limpas,rodadas_limpas_1)

partidas_comp=partidas %>% gather(condicao,Time,c(home_team,away_team)) %>% 
  mutate(condicao=if_else(condicao=="home_team","mandante","visitante"),
         score=if_else(condicao=="visitante",stringi::stri_reverse(score),score)) %>% arrange(round,game,condicao)

partidas_comp = partidas_comp %>% mutate(Time=str_squish(Time),Time=case_when(
  Time=="Atlético - PR" ~ "Atlético-PR",
  Time=="Atlético - MG" ~ "Atlético-MG",
  Time=="América - MG" ~ "América-MG",
  !Time %in% c("Atlético-PR","Atlético-MG","América-MG") ~ str_remove(Time," - [:alpha:]{2}")),
  Time=if_else(Time=="Vasco da Gama","Vasco",Time))

partidas_comp %>% distinct(Time)

tab_completa1[,4:7]

tab_completa=read_csv2("CSV/campeonato_brasileiro_agregado.csv")
tab_completa1=read_csv2("CSV/campeonato_brasileiro_desagregado.csv")

tab_junta= tab_completa %>% left_join(partidas_comp,by=c("atletas.clube.id.full.name"="Time",
                                                         'atletas.rodada_id'="round"))

tab_junta1= tab_completa1 %>% left_join(partidas_comp,by=c("atletas.clube.id.full.name"="Time",
                                                         'atletas.rodada_id'="round"))

tab_completa %>% distinct(atletas.clube.id.full.name)

partidas_comp %>% distinct(Time)



tab_junta1 %>% filter(atletas.clube.id.full.name=="Corinthians",
                      atletas.posicao_id=="gol",atletas.apelido=="Cássio") %>%
               group_by(atletas.apelido,condicao) %>%
              summarise(media_dd=mean(DD), media_pe=mean(PE),dp=mean(CA),gs=mean(GS))

tab_junta1 %>% filter(atletas.posicao_id=="gol") %>%
  group_by(condicao) %>%
  summarise(media_dd=mean(DD), media_pe=mean(PE),dp=mean(CA),gs=mean(GS),sg=mean(SG))

tab_junta1 %>% filter(atletas.posicao_id=="gol") %>%
  group_by(condicao,atletas.clube.id.full.name) %>%
  summarise(dd=mean(DD), pe=mean(PE),dp=mean(CA),gs=mean(GS),sg=mean(SG)) %>%
  arrange(desc(gs))

tab_junta1 %>% filter(is.na(condicao)) %>% distinct(atletas.clube.id.full.name)



colnames(tab_junta1) %>% View()
partidas_comp %>% colnames()
