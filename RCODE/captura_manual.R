nrod=22
remDr$navigate(paste0("https://www.cartolafcbrasil.com.br/scouts/cartola-fc-2018/rodada-",nrod))

#Achar o filtro de Status e alterar para 'Todos'
opc_status=remDr$findElements(using="css","#ctl00_cphMainContent_drpStatus option")
opc_status[[1]]$clickElement()
Sys.sleep(1)
#Achar o filtro de Itens por página e alterar para '100'
opc_ipg=remDr$findElements(using="css","#ctl00_cphMainContent_drpPageSize option")
opc_ipg[[3]]$clickElement()
Sys.sleep(1)
#Achar o bot?o filtrar e apertar
bot_filtro1=remDr$findElements(using="css","#ctl00_cphMainContent_btnFiltrar")
bot_filtro1[[1]]$clickElement()
Sys.sleep(1)

cb=list()
clube=list()

for(i in 1:8){
  remDr$executeScript(paste0("javascript:__doPostBack('ctl00$cphMainContent$gvList','Page$",i,"')"))
  Sys.sleep(2)
  page <- read_html(remDr$getPageSource()[[1]])
  cb[[i]]=page %>% html_node(".table") %>% html_table(fill=T) %>% .[1:100,1:25]
  clube[[i]]=page %>% html_node(".table") %>% html_nodes("img") %>% html_attr("alt")
  Sys.sleep(2)
}

for(i in 2:8){
  remDr$executeScript(paste0("javascript:__doPostBack('ctl00$cphMainContent$gvList','Page$",i,"')"))
  Sys.sleep(2)
  page <- read_html(remDr$getPageSource()[[1]])
  cb[[i]]=page %>% html_node(".table") %>% html_table(fill=T) %>% .[1:100,1:25]
  clube[[i]]=page %>% html_node(".table") %>% html_nodes("img") %>% html_attr("alt")
  Sys.sleep(2)
}


cb[[2]] %>% slice(1:10) %>% select(Nome)
clube[[8]]


remDr$executeScript(paste0("javascript:__doPostBack('ctl00$cphMainContent$gvList','Page$",9,"')"))
page <- read_html(remDr$getPageSource()[[1]])
clube[[9]]=page %>% html_node(".table") %>% html_nodes("img") %>% html_attr("alt")

clube[[9]]
clube[[9]] %>% length() ->npag 

cb[[9]]=page %>% html_node(".table") %>% html_table(fill=T) %>% .[1:npag,1:25]

cb[[9]]


jrodada=bind_rows(cb)
jclube=flatten_chr(clube)
cb = jrodada 
cb$Clube=jclube

#Arrumando o banco de dados

cb$Clube=cbclube

cb1 = cb
cb1[is.na(cb1)]=0

cb2= cb1 %>% separate(Nome, into =  c("Nome","Posição"),sep=" \\(") %>% mutate(Posição=str_replace(Posição,"\\)",""))

#Escrevendo os bancos de dados em CSV
write.csv(cb,paste0("CSV/camp_bras_rodada_",nrod,"_bruto.csv"),fileEncoding = 'ISO-8859-1')
write.csv(cb1,paste0("CSV/camp_bras_rodada_",nrod,"_semi_bruto.csv"),fileEncoding = 'ISO-8859-1')
write.csv(cb2,paste0("CSV/camp_bras_rodada_",nrod,"_final.csv"),fileEncoding = 'ISO-8859-1')

