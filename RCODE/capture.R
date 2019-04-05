rodada=function(nrod){
  remDr$navigate(paste0("https://www.cartolafcbrasil.com.br/scouts/cartola-fc-2018/rodada-",nrod))
  #Achar o filtro de Status e alterar para 'Todos'
  opc_status=remDr$findElements(using="css","#ctl00_cphMainContent_drpStatus option")
  opc_status[[1]]$clickElement()
  Sys.sleep(1)
  #Achar o filtro de Itens por página e alterar para '100'
  opc_ipg=remDr$findElements(using="css","#ctl00_cphMainContent_drpPageSize option")
  opc_ipg[[3]]$clickElement()
  Sys.sleep(1)
  #Achar o botao filtrar e apertar
  bot_filtro1=remDr$findElements(using="css","#ctl00_cphMainContent_btnFiltrar")
  bot_filtro1[[1]]$clickElement()
  Sys.sleep(1)
  
  # cb como tibble e cbclube como vetor vazio 
  cb=as_tibble()
  cbclube=NULL
  
  for (i in 1:9) {
    remDr$executeScript(paste0("javascript:__doPostBack('ctl00$cphMainContent$gvList','Page$",i,"')"))
    Sys.sleep(1)
    page <- read_html(remDr$getPageSource()[[1]])
    if(i==1){
      cb=page %>% html_node(".table") %>% html_table(fill=T) %>% .[1:100,1:25]
      #cbclube= page %>% html_node(".table") %>% html_nodes("img") %>% html_attr("alt")
    } else {
      if(i<8){
        cb=cb %>% bind_rows(page %>% html_node(".table") %>% html_table(fill=T) %>% .[1:100,1:25])
        #cbclube= c(cbclube,page %>% html_node(".table") %>% html_nodes("img") %>% html_attr("alt"))
      } else {
        cb=cb %>% bind_rows(page %>% html_node(".table") %>% html_table(fill=T) %>% .[1:3,1:25])
        #cbclube= c(cbclube,page %>% html_node(".table") %>% html_nodes("img") %>% html_attr("alt"))
      }
      
    }
    
  }
  
  #cb$Clube=cbclube
  
  cb1 = cb
  cb1[is.na(cb1)]=0
  
  cb2= cb1 %>% separate(Nome, into =  c("Nome","Posição"),sep=" \\(") %>% mutate(Posição=str_replace(Posição,"\\)",""))
  
  #Escrevendo os bancos de dados em CSV
  write.csv(cb,paste0("camp_bras_rodada_",nrod,"_bruto.csv"),fileEncoding = 'ISO-8859-1')
  write.csv(cb1,paste0("camp_bras_rodada_",nrod,"_semi_bruto.csv"),fileEncoding = 'ISO-8859-1')
  write.csv(cb2,paste0("camp_bras_rodada_",nrod,"_final.csv"),fileEncoding = 'ISO-8859-1')
  
}

rodada(16)
