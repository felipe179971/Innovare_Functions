#-------------------------------------------------------------------------------------------------------------------
#-------- PACOTES --------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------

library(tidyverse)
`%nin%` = Negate(`%in%`)










#-------------------------------------------------------------------------------------------------------------------
#-------- FUNÇÕES DE FREQUÊNCIA ------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------
  
# Funções que geram tabelas de frequência por variável.



### Função Isoladas (variáveis comuns, nem MRG e nem citou/não citou)

FUN_isoladas <- function(TABELA, variaveis) {
  
  out <- vector("list", length = length(variaveis))
  
  for (i in seq_along(out)) {
    
    out[[i]] <- TABELA %>% 
      select(all_of( {{variaveis}}[[i]] ), peso) %>%
      mutate("n_base" = ifelse(test = rowSums(!is.na(across(all_of( {{variaveis}}[[i]] )))) > 0, 1, 0)) %>% 
      mutate("n_base" = sum(`n_base`)) %>%
      mutate("pct_base" = (100 * `n_base`) / nrow(.)) %>% 
      mutate("n_base_peso" = ifelse(test = rowSums(!is.na(across(all_of( {{variaveis}}[[i]] )))) > 0, peso, 0)) %>% 
      mutate("n_base_peso" = sum(`n_base_peso`)) %>%
      mutate("pct_base_peso" = (100 * `n_base_peso`) / sum(peso)) %>% 
      group_by(across(all_of( {{variaveis}}[[i]] ))) %>% 
      summarise("n_peso" = sum(peso), "n" = n(), .groups = "drop", across(c(n_base, pct_base, n_base_peso, pct_base_peso))) %>%
      distinct(across(everything())) %>%
      func_totalbase(.)
    
  }
  
  return(out)
  
}

### Função recebe: 
    ### uma tabela com entrevistas na linhas e perguntas nas colunas, assim como vem de um SQL: "SELECT * FROM `questionario_pos_campo`"
    ### um vetor com varáveis (ex.: c("v1", "v2", "v10"))

### Função retorna:
    ### lista com tabelas de frequência para cada variável

#------------------------------------------





### Função MRG (variáveis MRG, fechadas ou abertas)

FUN_MRG <- function(TABELA, lista_variaveis) {
  
  out <- vector("list", length = length(lista_variaveis))
  
  for (i in seq_along(out)) {
    
    out[[i]] <- TABELA %>% 
      select(all_of( {{lista_variaveis}}[[i]] ), peso) %>% 
      mutate("n_base" = ifelse(test = rowSums(!is.na(across(all_of( {{lista_variaveis}}[[i]] )))) > 0, 1, 0)) %>% 
      mutate("n_base" = sum(`n_base`)) %>%
      mutate("pct_base" = (100 * `n_base`) / nrow(.)) %>% 
      mutate("n_base_peso" = ifelse(test = rowSums(!is.na(across(all_of( {{lista_variaveis}}[[i]] )))) > 0, peso, 0)) %>% 
      mutate("n_base_peso" = sum(`n_base_peso`)) %>%
      mutate("pct_base_peso" = (100 * `n_base_peso`) / sum(peso)) %>% 
      pivot_longer(
        cols = {{lista_variaveis}}[[i]],
        names_to = "Variavel",
        values_to = "Respostas"
      ) %>%
      group_by(`Respostas`) %>% 
      summarise("n_peso" = sum(peso), "n" = n(), .groups = "drop", across(c(n_base, pct_base, n_base_peso, pct_base_peso))) %>%
      distinct(across(everything())) %>% 
      rename_with(
        .cols = 1,
        .fn = ~ str_c(lista_variaveis[[i]][[1]], "mrg") %>% str_replace("_\\d+", "")
      ) %>% 
      func_totalbase(.)
    
  }
  
  return(out)

}

### Função recebe: 
    ### uma tabela com entrevistas na linhas e perguntas nas colunas, assim como vem de um SQL: "SELECT * FROM `questionario_pos_campo`"
    ### uma lista com varáveis (ex.: list("v1mrg" = c("v1", "v2", "v3"), "v5mrg" = c("v5_1", "v5_2", "v5_3"))

### Função retorna:
    ### lista com tabelas de frequência para cada variável

#------------------------------------------





### Função Citou (variáveis citou/não citou)

FUN_Citou <- function(TABELA, lista_variaveis) {
  
  out <- TABELA %>% 
    {
      map(
        .x = {{ lista_variaveis }}, 
        .f = function(x) select(., all_of(x), peso)
      )
    } %>% 
    map(
      .x = .,
      .f = function(x) {
        DEBUG <- tibble(x)
        DEBUG %>% 
          mutate("n_base" = ifelse(test = rowSums(!is.na(across(-c(peso)))) > 0, 1, 0)) %>% 
          mutate("n_base" = sum(`n_base`)) %>%
          mutate("pct_base" = (100 * `n_base`) / nrow(.)) %>% 
          mutate("n_base_peso" = ifelse(test = rowSums(!is.na(across(-c(peso, n_base, pct_base)))) > 0, peso, 0)) %>% 
          mutate("n_base_peso" = sum(`n_base_peso`)) %>%
          mutate("pct_base_peso" = (100 * `n_base_peso`) / sum(peso)) %>% 
          pivot_longer(
            cols = -c(peso, n_base, pct_base, n_base_peso, pct_base_peso),
            names_to = "Variavel",
            values_to = "Respostas"
          ) %>% 
          filter(Respostas == 1) %>% 
          group_by(`Variavel`) %>% 
          summarise("n_peso" = sum(peso), "n" = n(), .groups = "drop", across(c(n_base, pct_base, n_base_peso, pct_base_peso))) %>% 
          distinct(across(everything())) %>% 
          rename(!!str_c(.[[1]][1], "mrg") := 1) %>% 
          func_totalbase(., citou = T)
      }
    )
  
  return(out)
  
}

### Função recebe: 
    ### uma tabela com entrevistas na linhas e perguntas nas colunas, assim como vem de um SQL: "SELECT * FROM `questionario_pos_campo`"
    ### uma lista com varáveis (ex.: list("G019" = c("v19", "v20", "v21"), "G025" = c("v25", "v26", "v27"))

### Função retorna:
    ### lista com tabelas de frequência para cada variável

#------------------------------------------










### Função de Média (aparada e não aparada)

FUN_media <- function(TABELA, variaveis, vars_na = c(-88, -99), aparada = F) {
  
  out <- vector("list", length = length(variaveis))
  
  for (i in seq_along(out)) {
    out[[i]] <- TABELA %>%
      select(peso, variaveis[i]) %>% 
      mutate(across(2, ~ replace(., . %in% vars_na, NA))) %>% 
      {
        if (aparada) {
          tibble(.) %>% 
          filter(ntile(.[2], 100) <= 97.5) %>% 
          filter(ntile(.[2], 100) >= 2.5) 
        } else {
          tibble(.)
        }
      } %>% 
      # summarise(across(2, ~ weighted.mean(., w = peso, na.rm = T))) %>% 
      summarise(
        "media" = mean(x = .[[2]], na.rm = T), 
        "media_peso" = weighted.mean(x = .[[2]], w = .[[1]], na.rm = T),
        "n" = n(),
        "n_peso" = sum(peso),
        "desvp" = sd(x = .[[2]], na.rm = T),
        "desvp_peso" = sqrt(Hmisc::wtd.var(x = .[[2]], weights = .[[1]], na.rm = T)),
        "erro" = desvp/sqrt(n),
        "erro_peso" = desvp_peso/sqrt(n)
      ) %>% 
      mutate("variavel" = variaveis[i], .before = 1)
  }
  
  return(bind_rows(out))
}

### Função recebe: 
### uma tabela com entrevistas na linhas e perguntas nas colunas, assim como vem de um SQL: "SELECT * FROM `questionario_pos_campo`"
### um vetor com varáveis (ex.: c("v1", "v2", "v10"))
### um vetor com variáveis para transformar em NA

### Função retorna:
### lista com tabelas com a média para cada variável

#------------------------------------------










#-------------------------------------------------------------------------------------------------------------------
#-------- FUNÇÕES DE CRUZAMENTO ------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------

# Funções que geram tabelas de frequência para cruzamentos de duas variáveis



### Variável isolada x variável isolada

FUN_cruzaVar <- function(TABELA, tablabel, var1, var2) {
  
  TABELA %>% 
    select({{var1}}, {{var2}}, peso) %>% 
    mutate(across(var1, ~ factor(., levels = tablabel %>% filter(opcao_variavel == var1) %>% pull(opcao_cod)))) %>% 
    mutate(across(var2, ~ factor(., levels = tablabel %>% filter(opcao_variavel == var2) %>% pull(opcao_cod)))) %>% 
    group_by(across(c({{var1}}, {{var2}})), .drop = T) %>% 
    summarise(
      "n" = n(),
      "n_peso" = sum(peso)
    ) %>% 
    mutate(
      "pct" = (100 * n / sum(n)),
      "pct_peso" = (100 * n_peso / sum(n_peso))
    ) %>% 
    ungroup() %>% 
    mutate("varlin" = str_c(var2), "varcol" = str_c(var1)) %>% 
    rename("codlin" = str_c(var2), "codcol" = str_c(var1)) %>% 
    relocate(varlin, varcol, codlin, codcol, n, n_peso, pct, pct_peso)
  
}

### Função recebe: 
    ### uma tabela com entrevistas na linhas e perguntas nas colunas, assim como vem de um SQL: "SELECT * FROM `questionario_pos_campo`"
    ### uma variável isolada (ex.: "v10")
    ### outra variável isolada (ex.: "v24")

### Função retorna:
    ### Tabela de frequência com cruzamento das duas variáveis fornecidas

#------------------------------------------





### Variável MRG x variável isolada

FUN_cruzaMRG <- function(TABELA, tablabel, var1, varMRG) {
  
  TABELA %>% 
    select(all_of(c( {{var1}}, {{varMRG}}[[1]] )), peso) %>% 
    pivot_longer(
      cols = {{varMRG}}[[1]],
      values_to = names(varMRG)
    ) %>%
    select(-name) %>% 
    mutate(across(var1, ~ factor(., levels = tablabel %>% filter(opcao_variavel == var1) %>% pull(opcao_cod)))) %>% 
    mutate(across(names(varMRG), ~ factor(., levels = tablabel %>% filter(opcao_variavel == names(varMRG)) %>% pull(opcao_cod)))) %>% 
    group_by(across(c(var1, names(varMRG))), .drop = T) %>% 
    summarise(
      "n" = n(),
      "n_peso" = sum(peso)
    ) %>% 
    mutate(
      "pct" = (100 * n / sum(n)),
      "pct_peso" = (100 * n_peso / sum(n_peso))
    ) %>% 
    ungroup() %>% 
    mutate("varlin" = str_c(names(varMRG)), "varcol" = str_c(var1)) %>% 
    rename("codlin" = str_c(names(varMRG)), "codcol" = str_c(var1)) %>% 
    relocate(varlin, varcol, codlin, codcol, n, n_peso, pct, pct_peso)
  
}

### Função recebe: 
    ### uma tabela com entrevistas na linhas e perguntas nas colunas, assim como vem de um SQL: "SELECT * FROM `questionario_pos_campo`"
    ### um vetor unitário com variável isolada (ex.: "v10")
    ### uma lista com variável MRG (ex.: list("v10mrg" = c("v10", "v11", "v12")))

### Função retorna:
    ### Tabela de frequência com cruzamento das duas variáveis fornecidas

#------------------------------------------





### Variável isolada x índice/média/grau 

FUN_cruzaMedia <- function(TABELA, tablabel, var1, ind, vars_na = c(-88, -99)) {
  
  TABELA %>% 
    select(all_of(c(var1, ind, "peso"))) %>% 
    mutate(across(var1, ~ factor(., levels = tablabel %>% filter(opcao_variavel == var1) %>% pull(opcao_cod)))) %>% 
    mutate(across(ind, ~ replace(., . %in% vars_na, NA))) %>% 
    group_by(across(c({{var1}})), .drop = T) %>% 
    summarise(
      "media" = mean(x = .data[[ind]], na.rm = T), 
      "media_peso" = weighted.mean(x = .data[[ind]], w = .data[["peso"]], na.rm = T),
      "n" = n(),
      "n_peso" = sum(peso),
      "desvp" = sd(x = .data[[ind]], na.rm = T),
      "desvp_peso" = sqrt(Hmisc::wtd.var(x = .data[[ind]], weights = .data[["peso"]], na.rm = T)),
      "erro" = desvp/sqrt(n),
      "erro_peso" = desvp_peso/sqrt(n)
    ) %>% 
    ungroup() %>% 
    mutate("varmedia" = str_c(ind), "varcol" = str_c(var1)) %>% 
    rename("codcol" = str_c(var1)) %>% 
    relocate(varmedia, varcol, codcol)
  
}

### Função recebe: 
    ### uma tabela com entrevistas na linhas e perguntas nas colunas, assim como vem de um SQL: "SELECT * FROM `questionario_pos_campo`"
    ### uma variável isolada (ex.: "v10")
    ### umavariável de indice/média/grau (ex.: "v20m")

### Função retorna:
    ### Tabela de frequência com cruzamento das duas variáveis fornecidas

#------------------------------------------










#----------------------------------------------------------------------------------------------------------------
#-------- FUNÇÕES AUXILIARES ------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------

# Funções auxiliares (usadas por outras funções)



### Adiciona total, base e número de entrevistas em uma tabela de frequência

func_totalbase <- function(TABFREQ, citou = F) {
  
  out <- TABFREQ
  
  # Bases
  n_base <- out %>% 
    distinct(n_base) %>% 
    pull()
  
  pct_base <- out %>% 
    distinct(pct_base) %>% 
    pull()
  
  n_base_peso <- out %>% 
    distinct(n_base_peso) %>% 
    pull()
  
  pct_base_peso <- out %>% 
    distinct(pct_base_peso) %>% 
    pull()
  
  
  # Clausula para tabelas vazias
  if (length(n_base) == 0) {
    n_base <- 0
  }
  
  
  # Tabela final
  if (n_base == 0) {
    
    out <- tibble(
      !!names(out[1]) := c("Total", "Base"),
      "n" = c(NA_integer_, NA_integer_),
      "n_peso" = c(NA_integer_, NA_integer_),
      "pct" = c(NA_integer_, NA_integer_),
      "pct_peso" = c(NA_integer_, NA_integer_)
    )
    
  } else {
    
    out <- out %>%
      select(!contains("base")) %>% 
      na.exclude() 
    
    if (citou) {
      
      out <- out %>% 
        mutate("pct" = (100 * n / n_base)) %>%
        mutate("pct_peso" = (100 * n_peso / n_base_peso)) %>%
        mutate(across(1, .fns = as.character)) %>% 
        map_df(.f = ~ c(., ifelse(is.numeric(.), sum(., na.rm = TRUE), "Total"))) %>%
        relocate(1, n, n_peso, pct, pct_peso) %>% 
        rbind(list("Base", n_base, n_base_peso, pct_base, pct_base_peso))
      
    } else {
      
      out <- out %>% 
        mutate("pct" = (100 * n / sum(n))) %>% 
        mutate("pct_peso" = (100 * n_peso / sum(n_peso))) %>% 
        mutate(across(1, .fns = as.character)) %>% 
        map_df(.f = ~ c(., ifelse(is.numeric(.), sum(., na.rm = TRUE), "Total"))) %>%
        relocate(1, n, n_peso, pct, pct_peso) %>% 
        rbind(list("Base", n_base, n_base_peso, pct_base, pct_base_peso))
      
    }
    
  }
  
  return(out)
  
}

### Função recebe: 
    ### uma tabela de frequência sem total e base

### Função retorna:
    ### Tabela de frequência com total e base

### Função utilizada no final das funções de cálculo de frequência (ex.: FUN_isoladas, FUN_MRG, FUN_citou)

#------------------------------------------





### Adiciona labels em uma tabela de frequência (tanto variável isolada/MRG quanto variável citou/não citou)

func_labels <- function(TABELA, tablabel, citou = F) {
  
  if (citou) {

    vars <- TABELA[[1]] %>% 
      str_subset("^v\\d")
    
    # Clausula para tabelas vazias
    if (length(vars) == 0) {
      vars <- tablabel %>% filter(pergunta_num_quest == names(TABELA)[1]) %>% distinct(opcao_variavel) %>% pull()
    }
    
    out <- TABELA %>% 
      right_join(
        tablabel %>%
          filter(opcao_variavel %in% vars) %>% 
          distinct(opcao_variavel, pergunta_enunciado) %>% 
          rename(!!names(TABELA)[1] := "opcao_variavel") %>%
          rbind(list("Total", "Total")) %>% 
          rbind(list("Base", "Base"))
      ) %>%      
      rename(!!str_c(names(TABELA)[1], "_label") := "pergunta_enunciado") 
    
    
  } else {
    
    var <- colnames(TABELA)[1]
    
    if (var %nin% c(tablabel %>% distinct(opcao_variavel) %>% pull())) {
      stop(str_c("Variavel ", var, " nao presente no dicionario"))
    }
    
    out <- TABELA %>% 
      right_join(
        tablabel %>%
          filter(str_detect(opcao_variavel, paste0(var, "(_|$)"))) %>% 
          distinct(opcao_cod, opcao_label) %>% 
          rename(!!var := "opcao_cod") %>%
          mutate(across(all_of(var), .fns = as.character)) %>% 
          rbind(list("Total", "Total")) %>% 
          rbind(list("Base", "Base"))
      ) %>%      
      rename(!!str_c(colnames(TABELA)[1], "_label") := "opcao_label") %>%
      arrange(match(.[[1]], c(
        tablabel %>%
          filter(str_detect(opcao_variavel, paste0(var, "(_|$)"))) %>%
          distinct(opcao_cod) %>%
          pull(),
        "Total", "Base"
      ))) %>% 
      arrange(.[[1]] %in% "-88") %>% 
      arrange(.[[1]] %in% "-99") %>% 
      arrange(.[[1]] %in% "Total") %>% 
      arrange(.[[1]] %in% "Base") 
    
  }
  
  return(out)
  
}

### Função recebe: 
    ### uma tabela de frequência
    ### uma tabela com o dicionário (contendo pelo menos as colunas "opcao_variavel", "opcao_cod", "opcao_label")
    ### uma opção booleana sobre ser variável do tipo citou/não citou (por padrão, falsa)

### Função retorna:
    ### Tabela de frequência com os labels (mantém uma coluna com os códigos)

### A tabela de dicionário precisa ter na coluna "opcao_variavel" as variáveis com os mesmos nomes das variáveis a tabela de frequência.
### Especialmente importante para as variáveis criadas, agregadas e MRG que por padrão não vêm na tabela de dicionário do SQL.

#------------------------------------------





### Transforma tabela de frequência por variável para um dataframe estilo banco de dados

func_freqToDF <- function(TABELA) {
  
  TABELA %>% 
    rename_with(.fn = ~ "opcao_label", .cols = ends_with("_label")) %>% 
    mutate("variavel" = colnames(.)[1]) %>% 
    rename("codigo" = 1) %>% 
    mutate("total" = n[which(.[[1]] == "Total")]) %>% 
    mutate("total_peso" = n_peso[which(.[[1]] == "Total")]) %>% 
    mutate("pct_base" = pct[which(.[[1]] == "Base")]) %>% 
    mutate("pct_base_peso" = pct_peso[which(.[[1]] == "Base")]) %>% 
    filter(.[[1]] %nin% c("Total", "Base")) %>% 
    select(variavel, codigo, n, n_peso, pct, pct_peso, total, total_peso, pct_base, pct_base_peso) 
  
}

### Função recebe: 
    ### uma tabela de frequência (com ou sem labels) com total e base nas linhas

### Função retorna:
    ### Tabela de frequência com total e base em colunas, coluna com nome da variável e coluna com tipo (n/perc)

#------------------------------------------





### Transforma tabela de frequência por variável para um dataframe estilo SPSS

func_freqToSPSS <- function(TABELA) {
  
  TABELA %>% 
    select(1, pct_peso, n_peso) %>% 
    t() %>%
    as_tibble(rownames = "teste") %>%
    janitor::row_to_names(row_number = 1) %>% 
    arrange(.[[1]] %in% c("n_peso")) %>% 
    mutate(across(-1, as.numeric)) %>% 
    mutate(across(where(is.numeric), ~ replace_na(., 0)))
  
}

### Função recebe: 
    ### uma tabela de frequência (com ou sem labels) com total e base nas linhas

### Função retorna:
    ### Tabela de frequência com "n" e "perc" nas linhas e cada opção nas colunas (incluindo uma coluna de total)

#------------------------------------------





### Transforma tabela com listas de frequência por variável para uma tabela comparando os splits

func_freqToCROSS <- function(TABELA_lista) {
  
  TABELA_lista %>% 
    unnest(data) %>% 
    mutate(var = map_chr(data, ~ names(.)[1]), .after = 1) %>% 
    group_nest(var) %>% 
    mutate(data = map(data, .f = function(x) {
      unnest(x, data) %>% 
        pivot_wider(names_from = splits, values_from = c(n, n_peso, pct, pct_peso)) %>% 
        janitor::clean_names() %>% 
        mutate(across(where(is.numeric), ~ replace_na(., 0)))
    }))
  
}

### Função recebe: 
    ### uma tabela com uma coluna com listas de tabelas de frequência.
    ### para cada linha, que contem um split, uma lista de tabelas de frequência para cada variável.

### Função retorna:
    ### uma tabela com uma coluna com listas de tabelas de frequência.
    ### para cada linha, que contem uma variável, uma lista de tabelas de frequência comparando os splits.

#------------------------------------------