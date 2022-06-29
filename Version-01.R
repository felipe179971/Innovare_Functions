################################################################################
##########################Pacotes###############################################
################################################################################
carregando_pacotes<-function(){
  #Meus pacotes############
  Sys.setenv(JAVA_HOME='C:/Java64/jdk-11') #Por causa do erro que aparecia ao carregar o 'xlsx' (Caminho manual da pasta do java no meu PC) [NÃO PEGOU DA ÚLTIMA VEZ!!!!]

  #para fazer o rJava e xlsx funcionarem, baixar jdk
  #library(installr)
  #install.java(
  #  version = 11,
  #  page_with_download_url = "http://jdk.java.net/java-se-ri/",
  #  path = "C:/java64"
  #)


  packages<-c("webshot","RMySQL", "openxlsx", "tidyverse", "knitr", "broom", "haven", "lubridate", "stringr",
              "Hmisc","berryFunctions","dplyr","ggplot2","magick","ggtext","cowplot","flextable","grid","readxl")

  package.check <- lapply(
    packages,
    FUN = function(x) {
      if (!require(x, character.only = TRUE)) {
        install.packages(x, dependencies = TRUE)
        library(x, character.only = TRUE)
      }
    }
  );rm(packages,package.check)
}
carregando_pacotes();rm(carregando_pacotes)

################################################################################
#######################Importando a Base e Tratando#############################
################################################################################
options(encoding="utf-8")
# Caminho na rede
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# SQL para pegar a base com as respostas
sql_data<- "SELECT * FROM questionario_resultado WHERE status = 0 AND codificacao_abertas_data IS NOT NULL"
#SQL labels
sql_quest_opcoes<-"SELECT opcao_id,opcao_variavel,opcao_cod,opcao_label FROM `quest_opcoes`"
sql_quest_perguntas<-"SELECT * FROM `quest_perguntas`"
sql_quest_abertas<-"SELECT * FROM `questao_aberta`,resposta_aberta where questao_aberta.questaoaberta_id=resposta_aberta.questaoaberta_id"
###Consultando ID's para colocar

# Conexão MySQL
con <- dbConnect(MySQL(), user = "sistema", password = "1nn05p35", host = "innovarepesquisa.com.br", dbname = "sinapsis_nao_resid_2021")

#Conecta na base de dados do sistema e 'baixa' as tabelas necessárias #
# Cria a conexão
#brasilprev_adm_planos_2021 <- dbConnect(MySQL(), user = "innovare_db", password = "1N};us3=9}*&", host = "innovarepesquisa.com.br", dbname = "brasilprev_adm_planos_2021")

# Tabelas
sinapsis_nao_resid_2021 <- dbGetQuery(conn = con,statement = sql_data)%>%as_tibble(.name_repair = "unique")%>%dplyr::mutate(across(where(is.character), ~iconv(.x,from='UTF-8', to='latin1')))
label <- dbGetQuery(conn = con,statement = sql_quest_opcoes)%>%as_tibble(.name_repair = "unique")%>%dplyr::mutate(across(where(is.character), ~iconv(.x,from='UTF-8', to='latin1')))
quest_perguntas<-dbGetQuery(conn = con,statement = sql_quest_perguntas)%>%as_tibble(.name_repair = "unique")%>%dplyr::mutate(across(where(is.character), ~iconv(.x,from='UTF-8', to='latin1')))
quest_abertas<-dbGetQuery(conn = con,statement = sql_quest_abertas)%>%as_tibble(.name_repair = "unique")%>%dplyr::mutate(across(where(is.character), ~iconv(.x,from='UTF-8', to='latin1')))
# Desconecta do sistema
dbDisconnect(conn = con)

# Remove os objetos desnecessarios
suppressWarnings(rm(con,sql_data,sql_quest_opcoes,sql_quest_perguntas,sql_quest_abertas,sql_serie_historica))

Processamento<-rbind(label)%>%data.frame()
#Se está na aberta, substitui o processamento
b<-quest_abertas%>%dplyr::select(questaoaberta_id...1,questaoaberta_variavel,respostaaberta_cod,respostaaberta_label)%>%data.frame()
colnames(b)<-colnames(Processamento)
a<-Processamento[-which(Processamento[,2]%in%b[,2]),]
Processamento<-rbind(a,b)
Processamento<-Processamento[order(Processamento[,1]),]
rm(a,b)

################################################################################
################################################################################
#Função para estilo SPSS
#Parâmetros#############################
dataset<-sinapsis_nao_resid_2021
splits<-c("Geral","v1","v13")
peso_splits<-c("peso_geral",F,"peso_geral")
#variaveis_quero<-colnames(data)[-c(1:9,which(str_sub(colnames(data),start=-4)=="orig"),which(str_sub(colnames(data),end=4)=="peso"),which(colnames(data)%in%c("telefone","ddd","data_sincronizacao","data_upload","entr_usuario","id_aparelho","latitude","longitude","re_checagem_equipe_id","re_checagem_data","codificacao_abertas_data","status","revisao_data","revisao_equipe_id","checagem_data","checagem_equipe_id","controlequalidade_data"     ,"controlequalidade_equipe_id","codificacao_data","peso_geral","peso_Belo Horizonte","peso_Brasília","peso_Rio de Janeiro"        ,"peso_São Paulo" )) )]
#variaveis_quero<-list("v1","v120")
variaveis_quero<-list(c("G95MRG",paste0("v",95:98)),"v1") #If it's a simple MRG, put (name, var1,var2,...,varn)
#valores_n_entra_na_media<-c("Não mudaria o uso dos equipamentos")

dataset_labels<-Processamento #Dataset contendo o label
coluna_var_labels<-c("opcao_variavel") #coluna que tem as variáveis: var1, var2, var3,....
coluna_cod_labels<-c("opcao_cod")#código var1: 1,2,3;var2: 1,2,3,4
coluna_text_labels<-c("opcao_label")#Onde tenho o significado do código (Ex.: 1=Sim, 2=Não...
caminho_com_nome_saida<-"F:/42 statistic/Innovare/GitHub/00.Functions/Felipe_teste.xlsx"
tipo_saida<-"PowerBi" #PowerBi

#################Para testar labels que não estão no dataset mas estão no arquivo de labels
#a<-dataset_labels[1,];a[1,][,which(colnames(a)==coluna_var_labels)]<-"v2";a[1,][,which(colnames(a)==coluna_text_labels)]<-"Teste";a[1,][,which(colnames(a)==coluna_cod_labels)]<-55;dataset_labels<-bind_rows(dataset_labels,a);rm(a);dataset_labels<-dataset_labels%>%unique()
#################Para testar MRG: Preciso do MRG criado no arquivo de labels
dataset_labels<-bind_rows(dataset_labels,dataset_labels%>%dplyr::filter(opcao_variavel%in%c("95"))%>%dplyr::mutate(opcao_variavel="G95MRG"))%>%unique()


########################################
#Initial check
error<-0
if(length(splits)!=length(peso_splits)){
  warning("The first splits match with the first peso_splits, the second splits match with the second peso_splits,...")
  warning("If it's a frequency without weight, please put the corresponding split_peso equal 'F' or 'FALSE'.\n Example: splits<-c('Geral','v2','v3')\n if 'v2' don't need weight but 'Geral' use 'peso_geral' and 'v3' use 'var_outros' how weight =>> peso_splits<-c('peso_geral',F,'var_outros');\n if all don't need weight ==> peso_splits<-rep(F,length(splits)) ")
  error<-error+1
  errorCondition("'splits' and 'peso_splits' must have the same length.")
}
if(splits[1]!="Geral"){
  warning("The first element of splits must be 'Geral' and the first element of peso_splits must be the column name that contain the weight apply to 'Geral'.")
  warning("If it's a frequency without weight, please put the corresponding split_peso equal 'F' or 'FALSE'.\n Example: splits<-c('Geral','v2','v3')\n if 'v2' don't need weight but 'Geral' use 'peso_geral' and 'v3' use 'var_outros' how weight =>> peso_splits<-c('peso_geral',F,'var_outros');\n if all don't need weight ==> peso_splits<-rep(F,length(splits)) ")
  error<-error+1
  errorCondition("'splits' is wrong")
}

if(any(lapply(variaveis_quero,length)>1)){
  warning("It's have ",length(which(lapply(variaveis_quero,length)>1)), " MRG to be calculated:")
  a<-variaveis_quero[[which(lapply(variaveis_quero,length)>1)]][[1]]
  for(i in 1:length(a)){
    warning(stringr::str_remove(a[i],"_citou"))
  }
}

if(any(as.logical(lapply(variaveis_quero,function(x){stringr::str_detect(stringr::str_sub(x[[1]][[1]], start = -1),"m")})) ) ){
  warning("It's have ",length(which(as.logical(lapply(variaveis_quero,function(x){stringr::str_detect(stringr::str_sub(x[[1]][[1]], start = -1),"m")})))), " average to be calculated:")
  a<-variaveis_quero[[which(as.logical(lapply(variaveis_quero,function(x){stringr::str_detect(stringr::str_sub(x[[1]][[1]], start = -1),"m")})))]][[1]]
  for(i in 1:length(a)){
    warning(stringr::str_remove(a[i],"_citou"))
  }
}


#Calculating the frequency by splits
Resultado<-list(inicial=list())
description<-data.frame(split_id=NA,Var1=NA,Cod_Var1=NA,Var2=NA,Weight=NA)
Base<-list(inicial=list())
if(error==0){
  #rm(error)
  #making a column with all elements equal 1 for frequency without weight
  if(any(peso_splits==F)){
    dataset$sem_peso19848421fsdfwefsd38<-1
    peso_splits[which(peso_splits==F)]<-"sem_peso19848421fsdfwefsd38"
  }
  #Making "Geral" in dataset and "dataset_labels"
  dataset$Geral<-1
  a<-dataset_labels[1,];a[1,][,which(colnames(a)==coluna_cod_labels)]<-1;a[1,][,which(colnames(a)==coluna_var_labels)]<-"Geral";a[1,][,which(colnames(a)==coluna_text_labels)]<-"Geral";dataset_labels<-bind_rows(dataset_labels,a);rm(a);dataset_labels<-dataset_labels%>%unique()
  #by splits
  for(s in 1:length(splits)){
    #s=1
    #by variable
    for(v in 1:length(variaveis_quero)){
      #v=1
      nome_lista_resultado=100*s
      #Possible results for that splits
      resultados_possiveis_split<-dataset_labels%>%dplyr::select(all_of(coluna_var_labels),all_of(coluna_cod_labels))%>%dplyr::rename("var"=colnames(.)[1],"cod"=colnames(.)[2])%>%dplyr::filter(var==splits[s])%>%dplyr::select(cod)%>%unique()%>%first()
      for(rps in 1:length(resultados_possiveis_split)){
        nome_lista_resultado<-nome_lista_resultado+1
        #rps=1
        dataset_filtred<-dataset[which(dataset[,which(colnames(dataset)==splits[s])]==resultados_possiveis_split[rps]),]

        #Normal frequency and Average
        if(length(variaveis_quero[v][[1]])==1){
          #if(        #Average
            #if( stringr::str_detect(stringr::str_sub(variaveis_quero[v][[1]], start = -1),"m")){
            #})

          Freq<-full_join(bind_cols(
            tibble(split_id=nome_lista_resultado),tibble(variavel=variaveis_quero[v][[1]][1]),
            dataset_filtred%>%dplyr::select(variaveis_quero[v][[1]],peso_splits[s])%>%dplyr::rename("codigo"=colnames(.)[1],"peso"=colnames(.)[2])%>%dplyr::mutate(v=1)%>%dplyr::filter(!is.na(codigo))%>%dplyr::group_by(codigo)%>%dplyr::summarise(n=sum(v,na.rm =T),n_peso=sum(peso,na.rm =T))%>%dplyr::mutate(pct=n/sum(n)*100,pct_peso=n_peso/sum(n_peso)*100)%>%dplyr::mutate(codigo=as.character(codigo))
          ),
          dataset_labels%>%dplyr::select(coluna_cod_labels,coluna_var_labels)%>%dplyr::rename("cod"=colnames(.)[1],"var"=colnames(.)[2])%>%dplyr::filter(var==variaveis_quero[v][[1]][[1]])%>%dplyr::mutate(cod=as.character(cod))%>%dplyr::select(cod)
          ,by = c("codigo" = "cod"))%>%dplyr::mutate(split_id=ifelse(is.na(split_id),nome_lista_resultado,split_id),variavel=ifelse(is.na(variavel),variaveis_quero[v][[1]][1],variavel))%>%dplyr::mutate(across(colnames(.),~ifelse(is.na(.),0,.)))

          base_Geral<-bind_cols(tibble(split_id=nome_lista_resultado),tibble(variavel=variaveis_quero[v][[1]][1]),
                                dataset_filtred%>%dplyr::select(variaveis_quero[v][[1]],peso_splits[s])%>%dplyr::rename("codigo"=colnames(.)[1],"peso"=colnames(.)[2])%>%dplyr::mutate(n_sem=ifelse(!is.na(codigo),1,0),n_com=ifelse(!is.na(codigo),peso,0))%>%dplyr::summarise(total=sum(n_sem,na.rm =T),total_peso=sum(n_com,na.rm =T),pct_base=total/nrow(dataset)*100,pct_base_peso=total_peso/sum(dataset[,which(colnames(dataset)==peso_splits[s])],na.rm = T)*100)%>%data.frame()
          )
        }#end Normal frequency
        #MRG
        if(length(variaveis_quero[v][[1]])>1){
          #Normal MRG
          Freq<-full_join(bind_cols(
            tibble(split_id=nome_lista_resultado),tibble(variavel=variaveis_quero[v][[1]][1]),
            dataset_filtred%>%dplyr::select(variaveis_quero[v][[1]][-1],peso_splits[s])%>%tidyr::pivot_longer(cols=variaveis_quero[v][[1]][-1])%>%dplyr::select(1,3)%>%dplyr::rename("peso"=colnames(.)[1],"codigo"=colnames(.)[2])%>%dplyr::mutate(v=1)%>%dplyr::filter(!is.na(codigo))%>%dplyr::group_by(codigo)%>%dplyr::summarise(n=sum(v,na.rm =T),n_peso=sum(peso,na.rm =T))%>%dplyr::mutate(pct=n/sum(n)*100,pct_peso=n_peso/sum(n_peso)*100)%>%dplyr::mutate(codigo=as.character(codigo))
          ),
          dataset_labels%>%dplyr::select(coluna_cod_labels,coluna_var_labels)%>%dplyr::rename("cod"=colnames(.)[1],"var"=colnames(.)[2])%>%dplyr::filter(var==variaveis_quero[v][[1]][[1]])%>%dplyr::mutate(cod=as.character(cod))%>%dplyr::select(cod)
          ,by = c("codigo" = "cod")
          )%>%dplyr::mutate(split_id=ifelse(is.na(split_id),nome_lista_resultado,split_id),variavel=ifelse(is.na(variavel),variaveis_quero[v][[1]][1],variavel))%>%dplyr::mutate(across(colnames(.),~ifelse(is.na(.),0,.)))

          base_Geral<-bind_cols(tibble(split_id=nome_lista_resultado),tibble(variavel=variaveis_quero[v][[1]][1]),
                                dataset_filtred%>%dplyr::select(all_of(variaveis_quero[v][[1]][-1]),peso_splits[1])%>%dplyr::rename("peso"=colnames(.)[ncol(.)])%>%dplyr::mutate(across(variaveis_quero[v][[1]][-1],~ifelse(!is.na(.x),1,0)))%>%dplyr::mutate(entra=rowSums(.[-ncol(.)],na.rm = T))%>%dplyr::filter(entra!=0)%>%dplyr::summarise(total=sum(entra,na.rm =T),total_peso=sum(peso,na.rm =T),pct_base=total/nrow(dataset)*100,pct_base_peso=total_peso/sum(dataset[,which(colnames(dataset)==peso_splits[s])],na.rm = T)*100)%>%data.frame()
          )

        }

        #Description
        if(all(is.na(description$split_id))){
          description<-rbind(description,c(nome_lista_resultado,splits[s],ifelse(splits[s]=="Geral","Geral",resultados_possiveis_split[rps]),ifelse(stringr::str_detect(variaveis_quero[v][[1]][1],"X"),"FAZER DEPOIS","Todas especificadas em 'variaveis_quero'"),ifelse(peso_splits[s]=="sem_peso19848421fsdfwefsd38","Sem Peso",peso_splits[s])))%>%unique()
        }else{
          if(!any(description$split_id==nome_lista_resultado)){
            description<-rbind(description,c(nome_lista_resultado,splits[s],ifelse(splits[s]=="Geral","Geral",resultados_possiveis_split[rps]),ifelse(stringr::str_detect(variaveis_quero[v][[1]][1],"X"),"FAZER DEPOIS","Todas especificadas em 'variaveis_quero'"),ifelse(peso_splits[s]=="sem_peso19848421fsdfwefsd38","Sem Peso",peso_splits[s])))%>%unique()
          }};if(all(is.na(description[1,]))){description[1,]<-description[-1,]};description<-description%>%unique()

        #Results (Freq)
        if(!any(names(Resultado)==nome_lista_resultado)){
          Resultado[[length(Resultado)+1]]<-list(Freq);names(Resultado)[length(Resultado)]<-nome_lista_resultado;names(Resultado[[length(Resultado)]])<-variaveis_quero[v][[1]][1]
        }else{
          Resultado[[which(names(Resultado)==nome_lista_resultado)]][[length(Resultado[[which(names(Resultado)==nome_lista_resultado)]])+1]]<-Freq;names(Resultado[[which(names(Resultado)==nome_lista_resultado)]])[length(Resultado[[which(names(Resultado)==nome_lista_resultado)]])]<-variaveis_quero[v][[1]][1]
        }
        #Results (Base)
        if(!any(names(Base)==nome_lista_resultado)){
          Base[[length(Base)+1]]<-list(base_Geral);names(Base)[length(Base)]<-nome_lista_resultado;names(Base[[length(Base)]])<-variaveis_quero[v][[1]][1]
        }else{
          Base[[which(names(Base)==nome_lista_resultado)]][[length(Base[[which(names(Base)==nome_lista_resultado)]])+1]]<-base_Geral;names(Base[[which(names(Base)==nome_lista_resultado)]])[length(Base[[which(names(Base)==nome_lista_resultado)]])]<-variaveis_quero[v][[1]][1]
        }
        print(paste0(variaveis_quero[v][[1]][1]," [split:",splits[rps]," | peso: ", ifelse(peso_splits[rps]=="sem_peso19848421fsdfwefsd38","Sem",peso_splits[rps]),"] (var ",v," de ",length(variaveis_quero),") {",nome_lista_resultado,"}" ))
      }#end Possible results for that splits (rps)
    }#end by variable (v)
  } #end by split (s)


  #Out
  if(tipo_saida=="PowerBi"){
    for(i in 2:length(Resultado)){
      for(j in 1:length(Resultado[[i]])){
        a<-Resultado[[i]][[j]]
        b<-Base[[i]][[j]]
        if(i==2){
          Resultado_Freq<-a
          Resultado_Base<-b
        }else{
          Resultado_Freq<-dplyr::bind_rows(Resultado_Freq,a)
          Resultado_Base<-dplyr::bind_rows(Resultado_Base,b)
        }
      }
    }
  }

}#End function###########




rm(a,b,j,tipo_saida,caminho_com_nome_saida,valores_n_entra_na_media,x,dataset_filtred,base_Geral,ID_split,coluna_var_labels,citou_nao_citou,coluna_cod_labels,coluna_text_labels,error,i,nome_lista_resultado,numero_split,peso_splits,resultados_possiveis_split,rps,s,splits,v,Freq,variaveis_quero,dataset,dataset_labels)%>%suppressWarnings()
