library(RSelenium)
library(rvest)
library(tidyverse)
rD= rsDriver(port=4444L)
remDr <- rD[["client"]]

remDr$navigate("https://www.cartolafcbrasil.com.br/scouts/cartola-fc-2018/rodada-1")
#remDr$screenshot(display = T)

#Achar o filtro de Status e alterar para 'Todos'
opc_status=remDr$findElements(using="css","#ctl00_cphMainContent_drpStatus option")
opc_status[[1]]$clickElement()
Sys.sleep(1)
#Achar o filtro de Itens por página e alterar para '100'
opc_ipg=remDr$findElements(using="css","#ctl00_cphMainContent_drpPageSize option")
opc_ipg[[3]]$clickElement()
Sys.sleep(1)
#Achar o botão filtrar e apertar
bot_filtro1=remDr$findElements(using="css","#ctl00_cphMainContent_btnFiltrar")
bot_filtro1[[1]]$clickElement()
Sys.sleep(1)

# cb como tibble e cbclube como vetor vazio 
cb=as_tibble()
cbclube=NULL

#webscrape do site
for (i in 1:8) {
  remDr$executeScript(paste0("javascript:__doPostBack('ctl00$cphMainContent$gvList','Page$",i,"')"))
  Sys.sleep(1)
  page <- read_html(remDr$getPageSource()[[1]])
  if(i==1){
    cb=page %>% html_node(".table") %>% html_table(fill=T) %>% .[1:100,1:25]
    cbclube= page %>% html_node(".table") %>% html_nodes("img") %>% html_attr("alt")
  } else {
    if(i<8){
      cb=cb %>% bind_rows(page %>% html_node(".table") %>% html_table(fill=T) %>% .[1:100,1:25])
      cbclube= c(cbclube,page %>% html_node(".table") %>% html_nodes("img") %>% html_attr("alt"))
    } else {
      cb=cb %>% bind_rows(page %>% html_node(".table") %>% html_table(fill=T) %>% .[1:88,1:25])
      cbclube= c(cbclube,page %>% html_node(".table") %>% html_nodes("img") %>% html_attr("alt"))
    }
    
  }

}

#Arrumando o banco de dados

cb$Clube=cbclube

cb1 = cb
cb1[is.na(cb1)]=0

cb2= cb1 %>% separate(Nome, into =  c("Nome","Posição"),sep=" \\(") %>% mutate(Posição=str_replace(Posição,"\\)",""))

#Escrevendo os bancos de dados em CSV
write.csv(cb,"camp_bras_rodada_1_bruto.csv",fileEncoding = 'ISO-8859-1')
write.csv(cb1,"camp_bras_rodada_1_semi_bruto.csv",fileEncoding = 'ISO-8859-1')
write.csv(cb2,"camp_bras_rodada_1_final.csv",fileEncoding = 'ISO-8859-1')

# Tabela de significados

# J - Jogos
# 
# SCOUTS POSITIVOS
# 
# RB - Roubada de bolas (+ 1,5)
# 
# G - Gol (+8,0)
# 
# A - Assistência (+5,0)
# 
# SG - Jogos sem sofrer gols (+5,0)
# 
# FS - Falta sofrida ( +0,5)
# 
# FF - Finalização para fora (+0,8)
# 
# FD - Finalização defendida (+1,2)
# 
# FT - Finalização na trave (+3,0)
# 
# DD - Defesa difícil (+3,0)
# 
# DP - Defesa de pênalti (+7,0)
# 
# SCOUTS NEGATIVOS
# 
# GC - Gol contra (-5,0)
# 
# CV - Cartão vermelho (-5,0)
# 
# CA - Cartão amarelo (-2,0)
# 
# GS - Gol sofrido (-2,0)
# 
# PP - Pênalti perdido (-4,0)
# 
# FC - Falta cometida (-0,5)
# 
# I - Impedimento (-0,5)
# 
# PE - Passe errado (-0,3)

#análise dos dados da primeira rodada

cb2 %>% count(Posição)

cb2 %>% group_by(Nome,Clube) %>% summarise(total_rb=sum(RB)) %>% arrange(desc(total_rb))

#Encerramento do Servidor
remDr$close()
rD[["client"]]$quit()
