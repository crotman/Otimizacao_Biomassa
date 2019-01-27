library(data.table)
library(pryr)
library(tidyverse)





tipo_de_otimizacao = "VP"
flag_calcula_mojo = FALSE
MoJo <- 0


#Lendo municipios e informacoes de energia e carga




#Lendo parâmetros adicionais

MWH_por_GJ = 0.27777777777777777777777778

anos_vida_util_usina = 30 

#parametros <-  read.csv("C:\\Mestrado\\parametros.csv",sep = ";", encoding = "UTF-8",dec = "," ) %>% as_tibble()

parametros <-  read.csv("c:\\temp\\parametros_novo_v2.csv",sep = ",", encoding = "UTF-8",dec = ".", stringsAsFactors = FALSE ) %>% as_tibble() 


str(parametros)

head(parametros)




#lendo os caches de tir

#Caches completos
#for (i in 1:31)
#{
  
#  TIRs <- TIRs %>% 
#    bind_rows(read.csv(paste("C:\\temp\\tirs_",i,".csv",sep='' )))
    
#}

#TIRs <- read.csv("C:\\temp\\minitirs.csv")



#TIRs <- TIRs %>% 
#  #mutate(usina = as.integer(usina/100000), anuais = as.integer(anuais/100000)) %>% 
#  mutate(usina = as.intinstaeger(usina/100000), anuais = as.integer(anuais/100000)) %>% 
#  arrange(usina,anuais)





#Funcoes usadas no algoritmo

insere_receita_custo_lucro <- function(num_cenario) {

  
  #inserindo a receita e o custo relacionados aa biomassa. Falta o custo da usina
    
  custo_colheita = as.double( parametros[num_cenario,"custo_colheita_por_t"] )
  custo_armazenamento = as.double( parametros[num_cenario,"custo_armazenamento_por_t"])
  custo_carga = as.double( parametros[num_cenario,"custo_carga_por_t"])
  custo_transporte = as.double( parametros[num_cenario,"custo_transporte_por_t_km"])
  premio_produtor = as.double( parametros[num_cenario,"premio_produtor"])
  receita_por_mwh = as.double( parametros[num_cenario,"receita_por_mwh"])
  fator_disponibilidade = as.double(parametros[num_cenario,"fator_disponibilidade"])

  
  matriz <- matriz %>% 
    mutate( 
      custo = 
        as.double(
          (
            custo_colheita * Carga_y +
              custo_armazenamento * Carga_y +
              custo_carga * Carga_y +
              premio_produtor * Preco_Total_y
          )
          + custo_transporte * Carga_y * Distancia
        )) %>% 
    
    mutate(  
      
      receita = as.double(Energia_y * MWH_por_GJ * receita_por_mwh * fator_disponibilidade)
    ) %>% 
    
    mutate ( lucro = receita - custo) %>% 
    
    filter (lucro >0) %>% 
    
    arrange( desc(lucro) ) 
  
}


calcula_heuristica_1 <- function(municipios_escopo) {
  
  #Heuristica:
  #Enquanto houver municipios
  #Cria uma Particao com o municipio de maior energia disponivel
  #Adiciona vizinhos deste municipio aa particao, em ordem decrescente de lucro, ate que nao seja possivel inclur devido ao maximo de producao
  
  
  municipios_escopo_h <- municipios_escopo %>% arrange(desc(Energia)) 
  
  municipios_escopo_h_desalocados <- municipios_escopo_h
  
  
  #inserindo a particao vazia
  particoes_h <- tibble(sede = integer(), cidades = integer(), label = integer()  )
  
  fim <- FALSE
  
  cod_municipio_tratar <- as.integer(municipios_escopo_h[1,"CD"])
  
  label <- 1
  
  while (!fim)
  {
    print(particoes_h)
    
    
    #pegando os vizinhos do municipio tratado ainda nao alocados que dao lucro
    vizinhos <- matriz %>% 
      filter(CD_x == cod_municipio_tratar) %>% 
      inner_join( municipios_escopo_h_desalocados, by = c("CD_y" = "CD")  ) %>% 
      select (CD_y)
    
    
    if (nrow(vizinhos) > 0)
    {
      #Criando a particao com os vizinhos que dao lucro
      particoes_h = add_row(particoes_h, sede = cod_municipio_tratar, cidades = vizinhos$CD_y, label = label )
      label <- label + 1
      #Retirando os alocados
      municipios_escopo_h_desalocados <- municipios_escopo_h_desalocados %>% 
        anti_join( vizinhos, by = c("CD" = "CD_y" ))
      #Pegando o proximo nao alocado 
      cod_municipio_tratar <- as.integer(municipios_escopo_h_desalocados[1,"CD"])
    }
    else
    {
      #Se nem o proprio municipio e viavel, alocamos todos os nao alocados numa particao vazia
      resto <- (municipios_escopo_h_desalocados %>% select(CD))$CD
      if (length(resto) > 0)
      {
        particoes_h <- add_row(particoes_h, sede = -1, cidades = resto, label = 0 ) 
      }
      fim <- TRUE
    }
    
  }
  
  particoes_h <- particoes_h %>% 
    mutate(sede = as.integer(sede), label = as.integer(label))
  
  particoes_h
  
}



calcula_heuristica_2 <- function(municipios_escopo, cenario) {
  
  #Heuristica:
  #Caminhando pelos municipios em ordem de energia.
  #Calcular o aumento de lucro que o municipio daria a cada particao, guardar o maior aumento de lucro e a maior particao
  #Comparar com o lucro que o municipio daria com sua propria particao
  #Escolher se cria uma nova particao ou coloca  municipio na particao onde ele aumentaria mais o lucro
  #Se todos os lucros forem negativos, coloca numa particao vazia

  alfa = as.double( parametros[cenario,"alfa_custo_usina"])
  beta = as.double( parametros[cenario,"beta_custo_usina"])
  perc_opex = as.double( parametros[cenario,"percentual_OPEX"])
  fator_disponibilidade = as.double(parametros[cenario,"fator_disponibilidade"])
  
    
  
  municipios_escopo_h <- municipios_escopo %>% arrange(desc(Energia)) 
  
  alfa = as.double( parametros[cenario,"alfa_custo_usina"])
  
  #inserindo a particao vazia
  particoes_h <- tibble(sede = integer(), cidades = integer()  )
  
  fim <- FALSE
  
  
  for (m in 1:nrow(municipios_escopo_h)){
    
    municipio_corrente <- pull(slice(municipios_escopo_h,m),CD)
    
    lucro_sozinho_tib <- matriz %>% 
      filter(CD_x == municipio_corrente & CD_y == municipio_corrente ) 
    
    lucro_sozinho_tib <- lucro_sozinho_tib %>% 
      mutate(custo_usina = calcula_custo_usina_v2(energia_GJ = Energia_y,num_cenario = i, fator_disponibilidade = fator_disponibilidade, alfa = alfa, beta = beta,perc_opex = perc_opex ) ) 
    
    lucro_sozinho_tib <- lucro_sozinho_tib %>%    
      mutate(lucro = lucro - custo_usina)
    
    lucro_sozinho <- lucro_sozinho_tib %>% pull(lucro)
    

    lucros_municipio_corrente <- matriz %>% 
      filter(CD_y == municipio_corrente)
    
    sedes_atuais <- particoes_h %>% 
      filter(sede != -1) %>% 
      group_by(sede) %>% 
      summarise()
    
    if (nrow(sedes_atuais) > 0)
    {
    
      lucros_adicionados_por_particao <- sedes_atuais %>% 
        left_join(lucros_municipio_corrente , by = c("sede"="CD_x")) %>% 
        mutate(adic_usina = calcula_custo_usina_v2(energia_GJ = Energia_y,num_cenario = i, fator_disponibilidade = fator_disponibilidade, alfa = 0, beta = beta,perc_opex = perc_opex )  ) %>% 
        mutate(lucro = lucro - adic_usina)
      
      maior_lucro_adicionado <- lucros_adicionados_por_particao %>% 
        top_n(1,lucro)
      
      if (nrow(maior_lucro_adicionado) > 0)
      {
        maior_lucro_adicionado_escalar <- pull(maior_lucro_adicionado,lucro)
      }
      else
      {
        maior_lucro_adicionado_escalar <- -1
      }
      

    }
    else
    {
      maior_lucro_adicionado_escalar <- -1
      
    }

    
    
    if (maior_lucro_adicionado_escalar > lucro_sozinho )
    {
      if (maior_lucro_adicionado_escalar > 0)
      {
        alocacao <- tibble(sede = pull(maior_lucro_adicionado, sede), cidades = municipio_corrente  )
      }
      else
      {
        alocacao <- tibble(sede = -1, cidades = municipio_corrente  )
        
      }
        
    }
    else
    {
      if (lucro_sozinho > 0)
      {
        alocacao <- tibble(sede = municipio_corrente, cidades = municipio_corrente  )
      }
      else
      {
        alocacao <- tibble(sede = -1, cidades = municipio_corrente  )
        
      }
      
    }

    particoes_h <- particoes_h %>% 
      bind_rows(alocacao)
    

  }
  
  particoes_h %>% 
    select(sede, cidades) %>% 
    mutate(label = dense_rank(sede))
  
}



calcula_heuristica_rand <- function(municipios_escopo) {
  
  num_regioes <- floor(nrow(municipios_escopo)/50)
  
  sedes <- sample_n(municipios_escopo,num_regioes, replace = FALSE) %>% 
    select(CD) %>% 
    rename(sede = CD)
  
  sedes_dos_municipios <- sample_n(sedes, nrow(municipios_escopo), replace = TRUE )
  
  particoes_h <- sedes_dos_municipios %>% 
    bind_cols(municipios_escopo) %>% 
    rename(cidades = CD)  %>% 
    select(sede, cidades) %>% 
    mutate(label = dense_rank(sede))
  
}





calcula_custo_usina <- function(energia_GJ, num_cenario){
  
  fator_disponibilidade = as.double(parametros[num_cenario,"fator_disponibilidade"])
  alfa = as.double( parametros[num_cenario,"alfa_custo_usina"])
  beta = as.double( parametros[num_cenario,"beta_custo_usina"])
  perc_opex = as.double( parametros[num_cenario,"percentual_OPEX"])
  
  #capacidade necessaria a potencia que a usina tem que ter para fazer frente a energia disponibilizada
  capacidade_necessaria = energia_GJ * MWH_por_GJ / fator_disponibilidade / (24*365)
  
  (alfa + beta * capacidade_necessaria) + (alfa + beta * capacidade_necessaria) * perc_opex * pv_um_real
  
}


calcula_custo_usina_v2 <- function(energia_GJ, num_cenario, fator_disponibilidade, alfa, beta, perc_opex){
  #fator_disponibilidade = as.double(parametros[num_cenario,"fator_disponibilidade"])
  #alfa = as.double( parametros[num_cenario,"alfa_custo_usina"])
  #beta = as.double( parametros[num_cenario,"beta_custo_usina"])
  #perc_opex = as.double( parametros[num_cenario,"percentual_OPEX"])
  
  #capacidade necessaria a potencia que a usina tem que ter para fazer frente a energia disponibilizada
  capacidade_necessaria = energia_GJ * MWH_por_GJ / fator_disponibilidade / (24*365)
  
  (alfa + beta * capacidade_necessaria) + (alfa + beta * capacidade_necessaria) * perc_opex * pv_um_real
  #(alfa + beta * energia_GJ * MWH_por_GJ / fator_disponibilidade / (24*365) ) * ( 1 + perc_opex )
}


calcula_lucro_varias_especificacoes <- function(particoes){
  
  
  #Recebe um dataframe com as colunas sede e cidades para cada configuracao, indexada pela coluna indice
  

  #matriz_para_join_ordenada <- matriz_ordenada %>% 
  #  select(CD_x, CD_y, lucro, Energia_y )


  #particoes_matriz <-  particoes %>%
  #  inner_join(matriz_para_join_ordenada, c("sede" = "CD_x", "cidades" = "CD_y" )) 


  particoes_matriz <-  particoes
  
  fator_disponibilidade = as.double(parametros[i,"fator_disponibilidade"])
  alfa = as.double( parametros[i,"alfa_custo_usina"])
  beta = as.double( parametros[i,"beta_custo_usina"])
  perc_opex = as.double( parametros[i,"percentual_OPEX"])
  
  

  #agrupando para calcular, para cada candidata, o lucro e carga da regiÃ£o (sem custo da usina), depois o cutso da usina
  especificacoes_limpa_negativas <- particoes_matriz %>% 
    group_by(indice,sede) 
    
  especificacoes_limpa_negativas <- especificacoes_limpa_negativas %>% 
    summarise( lucro_total_sem_usina = sum(lucro), energia_total = sum(Energia_y), n = n()) 
    #mutate ( lucro_final_sede = lucro_total_sem_usina - custo_usina) %>% 

  especificacoes_limpa_negativas <-
    especificacoes_limpa_negativas %>% 
    mutate ( 
      custo_usina = sign(energia_total) * (alfa + beta * energia_total * MWH_por_GJ / fator_disponibilidade / (24*365) ) + sign(energia_total) * (alfa + beta * energia_total * MWH_por_GJ / fator_disponibilidade / (24*365) ) * perc_opex * pv_um_real 
        
    
    )   
  
  
    
  #if (tipo_de_otimizacao == "VP")
  #{
  #  especificacoes_limpa_negativas <- especificacoes_limpa_negativas %>% 
  #  #AQUI MUDADO PARA CALCULAR VALOR PRESENTE A UMA TAXA DE 15% REAL
  #  mutate (lucro_final_sede = -custo_usina - pv(taxa,anos_vida_util_usina,0,lucro_total_sem_usina/anos_vida_util_usina,0)   ) %>% 
  #  mutate ( lucro_final_sede = if_else(lucro_final_sede < 0 , 0, lucro_final_sede) )
  #}
  #else
  #{
  #  if (tipo_de_otimizacao == "TIR")
  #  {
  #    especificacoes_limpa_negativas <- especificacoes_limpa_negativas %>% 
  #    mutate( fluxo_usina_para_cache = as.integer(-1 * round(custo_usina,-5)), fluxo_anual_para_cache =  as.integer(round(lucro_total_sem_usina/anos_vida_util_usina, -5 ))) %>%
  #    left_join(TIRs, by = c("fluxo_usina_para_cache" = "usina", "fluxo_anual_para_cache" = "anuais")) %>% 
  #    mutate(lucro_final_sede = tir ) 
  #    
  #  }
  #}
    
  

  #escolhendo o melhor lucro
  if (tipo_de_otimizacao == "VP")
  {
    lucros <- especificacoes_limpa_negativas %>% 
      group_by( indice ) %>% 
      summarise(custo_usina = sum(custo_usina), lucro_total_sem_usina = sum(lucro_total_sem_usina)) %>% 
      mutate( lucro = -custo_usina + lucro_total_sem_usina  ) 
      
    melhor_lucro <- lucros %>% 
      top_n( 1, lucro)
  }
  else
  {
    if (tipo_de_otimizacao == "TIR")
    {
      melhor_lucro <- especificacoes_limpa_negativas %>% 
        group_by( indice ) %>% 
        summarise(custo_usina = sum(custo_usina), lucro_total_sem_usina = sum(lucro_total_sem_usina)) %>% 
        mutate( fluxo_usina_para_cache = as.integer(-1 * round(custo_usina,-5)/100000), fluxo_anual_para_cache =  as.integer(round(lucro_total_sem_usina/anos_vida_util_usina, -5 )/100000)) %>%
        left_join(TIRs, by = c("fluxo_usina_para_cache" = "usina", "fluxo_anual_para_cache" = "anuais")) %>% 
        mutate(lucro = tir ) %>% 
        top_n( 1, lucro)
      
    }
  }
  
  
  
    
  indice_do_melhor = first(melhor_lucro$indice)
  lucro_do_melhor = first(melhor_lucro$lucro)

  resposta <- especificacoes_limpa_negativas %>% 
    filter( indice == indice_do_melhor ) %>% 
    mutate( lucro = lucro_do_melhor ) %>% 
    top_n(1)
  

  particoes <- particoes %>%
    filter( indice == first(resposta$indice) ) %>%
    mutate( lucro = first(resposta$lucro) ) %>% 
    select( sede, cidades, lucro, label )
  
  particoes
  

}
  

calcula_lucro_escolhendo_sede <- function(particoes) {
  
  #Recebe um dataframe com as colunas sede e cidades para uma configuracao
  
  #Devolve o dataframe com as melhores sedes. E o lucro repetido na coluna lucro
  

  particoes <- particoes %>% 
    select(sede, cidades, label)
  
  #self join para testar todas as cidades como sede
  particoes_com_candidatas <- particoes %>% 
    inner_join( particoes, by = c("sede" = "sede")) %>% 
    rename( sede_candidata = cidades.x, cidade = cidades.y  ) %>% 
    rename( sede_original = sede )
  
  #join com a matriz de informacoes para calcular o lucro de cada cidade
  particoes_com_candidatas_matriz <-  particoes_com_candidatas %>%
    inner_join(matriz_so_lucro_energia_ordenada, c("sede_candidata" = "CD_x", "cidade" = "CD_y" )) 

  
  #agrupando para calcular, para cada candidata, o lucro e carga da regiÃ£o (sem custo da usina), depois o cutso da usina
  lucro_candidatas <- particoes_com_candidatas_matriz %>% 
    group_by(sede_original, sede_candidata) %>% 
    summarise( lucro_total_sem_usina = sum(lucro), energia_total = sum(Energia_y)) %>% 
    mutate ( custo_usina = calcula_custo_usina(energia_GJ = energia_total, num_cenario = i)  ) %>% 
    #AQUI MUDADO PARA CALCULAR A TIR
    mutate ( lucro_final_abs = lucro_total_sem_usina - custo_usina) %>% 
    mutate ( lucro_final = lucro_final_abs) %>% 
    #mutate( fluxo_usina_para_cache = as.integer(-1 * round(custo_usina,-5)), fluxo_anual_para_cache =  as.integer( round(lucro_total_sem_usina/anos_vida_util_usina, -5 ))) %>%
    #left_join(TIRs, by = c("fluxo_usina_para_cache" = "usina", "fluxo_anual_para_cache" = "anuais")) %>% 
    #mutate(lucro_final = tir )
    identity()
  
  
  #if (tipo_de_otimizacao == "VP")
  #{
  #  #AQUI MUDADO PARA CALCULAR VALOR PRESENTE A UMA TAXA FIXA
  #  lucro_candidatas <- lucro_candidatas %>% 
  #    mutate (lucro_final = -custo_usina - pv(taxa,anos_vida_util_usina,0,lucro_total_sem_usina,0)   ) %>% 
  #    identity()
  #}
  #else
  #{
  #  if (tipo_de_otimizacao == "TIR")
  #  {
  #    lucro_candidatas <- lucro_candidatas %>% 
  #      mutate( fluxo_usina_para_cache = as.integer(-1 * round(custo_usina,-5)/100000), fluxo_anual_para_cache =  as.integer( round(lucro_total_sem_usina/anos_vida_util_usina, -5 )/100000)) %>%
  #      left_join(TIRs, by = c("fluxo_usina_para_cache" = "usina", "fluxo_anual_para_cache" = "anuais")) %>% 
  #      mutate(lucro_final = tir ) %>% 
  #      identity()
   # }
  #}  
      
              
  #lucro_candidatas <- lucro_candidatas %>%
  #  mutate( fluxo_usina_para_cache = as.integer(-1 * round(custo_usina,-5)/100000), fluxo_anual_para_cache =  as.integer( round(lucro_total_sem_usina/anos_vida_util_usina, -5 )/100000))
  
  #lucro_candidatas <- lucro_candidatas %>%
  #  arrange(fluxo_usina_para_cache,fluxo_anual_para_cache) %>% 
  #  left_join(TIRs, by = c("fluxo_usina_para_cache" = "usina", "fluxo_anual_para_cache" = "anuais"))
    
  #lucro_candidatas <- lucro_candidatas %>%
  #  mutate(lucro_final = tir )
    
    
  maiores_lucros <- lucro_candidatas %>% 
    group_by(sede_original) %>% 
    summarise(maior_lucro = max(lucro_final))
  
  #selecionando as candidatas de maior lucro
  candidatas_vencedoras <- lucro_candidatas %>% 
    inner_join(maiores_lucros) %>% 
    filter( lucro_final == maior_lucro ) %>% 
    select( sede_original, sede_candidata, lucro_final ) %>% 
    #selecionando uma sÃ³ candidata arbitrariamente se duas tiverem o mesmo lucro
    group_by( sede_original ) %>% 
    summarise( candidata_vencedora = min(sede_candidata), lucro = max(lucro_final) )
  
  #substituindo a sede pela mais lucrativa
  particoes <- particoes %>% 
    inner_join( candidatas_vencedoras, by = (c("sede" = "sede_original" ))) %>% 
    mutate( sede = if_else(sede != as.integer(-1), as.integer(candidata_vencedora), as.integer(-1) )) %>% 
    select( sede, cidades, lucro, label) %>% 
    #limpando as particoes negativas para a vazia
    #mutate( sede = if_else(lucro < 0, as.integer(-1), sede) ) %>% 
    #mutate( label = if_else(lucro < 0, as.integer(0), label) ) %>% 
    
    
    #o lucro da vazia eh zero
    #mutate( lucro = if_else( sede == -1, 0, lucro )  )  %>% 
    
    arrange( sede )
  
  #Jogando as cidades sem lucro para a região para a partição vazia
  #particoes <- particoes %>% 
  #  left_join(matriz_so_lucro_energia, c("sede" = "CD_x", "cidades" = "CD_y" )) %>% 
  #  mutate(sede = if_else(is.na(lucro.y), as.integer(-1), sede ), label = if_else(is.na(lucro.y), as.integer(0), label ), lucro.x = if_else(is.na(lucro.y), 0, lucro.x ) ) %>% 
  #  rename(lucro = lucro.x) %>% 
  #  select(sede, cidades, lucro, label) %>% 
  #  identity()
      

  lucro_escalar <- particoes %>% 
    select(sede, lucro) %>% 
    distinct (sede, lucro) 
    
  lucro_escalar <- sum(lucro_escalar$lucro)
  
  particoes <- particoes %>% 
    mutate(lucro = lucro_escalar, sede_mexeu = 0)
  
    
  particoes
  
  
}






calcula_lucro_escolhendo_sede_da_sede_que_mexeu <- function(particoes) {
  
  #Recebe um dataframe com as colunas sede e cidades para uma configuracao
  
  #Devolve o dataframe com as melhores sedes. E o lucro repetido na coluna lucro

  
  fator_disponibilidade = as.double(parametros[i,"fator_disponibilidade"])
  alfa = as.double( parametros[i,"alfa_custo_usina"])
  beta = as.double( parametros[i,"beta_custo_usina"])
  perc_opex = as.double( parametros[i,"percentual_OPEX"])
  
  
  sede_que_mexeu <- particoes %>% 
    group_by(sede) %>% 
    summarise(sede_mexeu = max(sede_mexeu)) %>% 
    filter(sede_mexeu == 1)
  
  if (count(sede_que_mexeu) == 1)
  {
    sede_que_mexeu_escalar = first(sede_que_mexeu$sede)
  }
  else
  {
    sede_que_mexeu_escalar = -1
  }
  
  
  if (sede_que_mexeu_escalar != -1)
  {
  
    particao_que_mexeu_com_candidatas <- particoes %>% 
      inner_join(sede_que_mexeu, by = c("sede" = "sede")   ) %>% 
      inner_join( particoes, by = c("sede" = "sede")) %>% 
      rename( sede_candidata = cidades.x, cidade = cidades.y  ) %>% 
      rename( sede_original = sede )
    
  
    #join com a matriz de informacoes para calcular o lucro de cada cidade
    particoes_com_candidatas_matriz <-  particao_que_mexeu_com_candidatas %>%
      inner_join(matriz_so_lucro_energia_ordenada, c("sede_candidata" = "CD_x", "cidade" = "CD_y" )) 
    
    
    #agrupando para calcular, para cada candidata, o lucro e carga da regiÃ£o (sem custo da usina), depois o cutso da usina
    lucro_candidatas <- particoes_com_candidatas_matriz %>% 
      group_by(sede_original, sede_candidata) %>% 
      summarise( lucro_total_sem_usina = sum(lucro), energia_total = sum(Energia_y)) %>% 
      mutate ( custo_usina = calcula_custo_usina(energia_GJ = energia_total, num_cenario = i)  ) %>% 
      #AQUI MUDADO PARA CALCULAR A TIR
      mutate ( lucro_final_abs = lucro_total_sem_usina - custo_usina) %>% 
      mutate ( lucro_final = lucro_final_abs) %>% 
      #mutate( fluxo_usina_para_cache = as.integer(-1 * round(custo_usina,-5)), fluxo_anual_para_cache =  as.integer( round(lucro_total_sem_usina/anos_vida_util_usina, -5 ))) %>%
      #left_join(TIRs, by = c("fluxo_usina_para_cache" = "usina", "fluxo_anual_para_cache" = "anuais")) %>% 
      #mutate(lucro_final = tir )
      identity()
    
    
    #if (tipo_de_otimizacao == "VP")
    #{
    #  #AQUI MUDADO PARA CALCULAR VALOR PRESENTE A UMA TAXA FIXA
    #  lucro_candidatas <- lucro_candidatas %>% 
    #    mutate (lucro_final = -custo_usina - pv(taxa,anos_vida_util_usina,0,lucro_total_sem_usina,0)   ) %>% 
    #    identity()
    #}
  
  
    maiores_lucros <- lucro_candidatas %>% 
      group_by(sede_original) %>% 
      summarise(maior_lucro = max(lucro_final))
    
    #selecionando as candidatas de maior lucro
    candidatas_vencedoras <- lucro_candidatas %>% 
      inner_join(maiores_lucros) %>% 
      filter( lucro_final == maior_lucro ) %>% 
      select( sede_original, sede_candidata, lucro_final ) %>% 
      #selecionando uma sÃ³ candidata arbitrariamente se duas tiverem o mesmo lucro
      group_by( sede_original ) %>% 
      summarise( candidata_vencedora = min(sede_candidata), lucro = max(lucro_final) )

      #substituindo a sede pela mais lucrativa
    particoes <- particoes %>% 
        left_join( candidatas_vencedoras, by = (c("sede" = "sede_original" ))) %>% 
        mutate( sede = if_else( is.na(candidata_vencedora) , sede, as.integer(candidata_vencedora) )) %>% 
        select( sede, cidades, label) %>% 
        #limpando as particoes negativas para a vazia
        #mutate( sede = if_else(lucro < 0, as.integer(-1), sede) ) %>% 
        #mutate( label = if_else(lucro < 0, as.integer(0), label) ) %>% 
        arrange( sede )

  }
      

  
  particoes_calculo_lucro <-  particoes %>%
    select(sede, cidades, label) %>% 
    left_join(matriz_so_lucro_energia_ordenada, c("sede" = "CD_x", "cidades" = "CD_y" )) %>% 
    mutate( lucro = if_else(sede == -1, 0, lucro), Energia_y = if_else(sede == -1, 0, Energia_y)  ) %>% 
    replace_na(list(lucro = -1, Energia_y = 0  )  ) %>%     
    group_by(sede) %>% 
    summarise(lucro_total_sem_usina = sum(lucro), energia_total = sum(Energia_y)  ) %>%    
    mutate(custo_usina = sign(energia_total) * (alfa + beta * energia_total * MWH_por_GJ / fator_disponibilidade / (24*365) ) + sign(energia_total) * (alfa + beta * energia_total * MWH_por_GJ / fator_disponibilidade / (24*365) ) * perc_opex * pv_um_real   )  %>% 
    #mutate (lucro_final = -custo_usina - pv(taxa,anos_vida_util_usina,0,lucro_total_sem_usina,0)   ) %>% 
    mutate (lucro_final = -custo_usina + lucro_total_sem_usina   ) %>% 
    mutate (lucro_final_sem_negativa = if_else(lucro_final < 0,0, lucro_final )   ) 
    
  particoes_calculo_lucro_debug <-  particoes_calculo_lucro
    
    
  particoes_calculo_lucro <- particoes_calculo_lucro %>% 
    summarise(lucro_final = sum(lucro_final), lucro_final_sem_negativa = sum(lucro_final_sem_negativa) ) %>% 
    identity()
    
  lucro_escalar = first(particoes_calculo_lucro$lucro_final)
  lucro_sem_negativas_escalar = first(particoes_calculo_lucro$lucro_final_sem_negativa)
  

  particoes <- particoes %>% 
    mutate(lucro = lucro_escalar, lucro_sem_negativas = lucro_sem_negativas_escalar )

  
}




realiza_pedaco_passo_busca_local_backup <- function(sedes, ind_municipios, particoes_com_unitario, max_label)
{
  
  
  matriz_para_join_ordenada <- matriz_ordenada %>% 
    select(CD_x, CD_y, lucro, Energia_y )
  
  
  ind_sedes <- sedes %>% 
    select( indice, unitario, sede, label ) %>% 
    #adicionando uma sede "zero", que significa levar o municipio para uma sede dele
    add_row( indice = as.integer(0), unitario = as.integer(1), sede = as.integer(0), label = as.integer(-1)  ) %>% 
    #adicionando uma sede -1, que significa levar o municipio região que não fornece
    add_row( indice = as.integer(-1), unitario = as.integer(1), sede = as.integer(-1), label = as.integer(0)  ) %>% 
    rename (ind_sede = indice)
  
  trocas <- inner_join( ind_sedes, ind_municipios ) %>% 
    mutate (ind_troca = cumsum(unitario) )
  
  particoes_com_unitario <- particoes_com_unitario %>% 
    rename (ind_municipio = indice) %>% 
    select( -lucro)
  
  
  trocas <- trocas %>% 
    select(ind_municipio, sede, label, ind_troca)
  
  particoes_com_unitario <- particoes_com_unitario %>% 
    left_join(matriz_para_join_ordenada, c("sede" = "CD_x", "cidades" = "CD_y" )) %>% 
    mutate( lucro = if_else(sede == -1, 0, lucro), Energia_y = if_else(sede == -1, 0, Energia_y)  ) %>% 
    replace_na(list(lucro = -1, Energia_y = 0  )  )
  
  
  
  particoes_com_unitario <- particoes_com_unitario %>% 
    select(sede, ind_municipio, label, cidades, lucro, Energia_y )
  
  label_mais_1 <- as.integer(max_label + 1)
  
  #FORMA ALTERNATIVA DAS TROCAS
  
  particoes_com_troca_pre <- trocas  %>% 
    crossing(particoes_com_unitario)  
  
  particoes_com_troca_pre <- particoes_com_troca_pre %>% 
    rename( sede.x = sede1, sede.y = sede, ind_municipio.x = ind_municipio1, ind_municipio.y = ind_municipio, label.x = label1, label.y = label, indice = ind_troca )
  
  
  particoes_com_troca_pre_onde_troca <- particoes_com_troca_pre %>% 
    filter(ind_municipio.x == ind_municipio.y)
  
  
  #particoes_com_troca_pre_onde_NAO_troca <- particoes_com_troca_pre %>%  
  #filter(ind_municipio.x != ind_municipio.y)
  
  particoes_com_troca_pre_onde_troca <- particoes_com_troca_pre_onde_troca %>% 
    mutate(
      nao_sede_0 = sede.y != 0, 
      sede_0 = sede.y == 0,
      nao_label_menos_1 = label.y != -1 ,
      label_menos_1 =  label.y == -1,
      cidade_eh_sede = cidades == sede.x,
      cidade_nao_sede = cidades != sede.x
      
    ) 
  
  particoes_com_troca_pre_onde_troca <- particoes_com_troca_pre_onde_troca %>% 
    mutate ( sede_nova =  
               nao_sede_0 * sede.y + 
               sede_0 * cidades,
             label_novo = 
               nao_label_menos_1 * label.y + 
               label_menos_1 * cidade_eh_sede * label.x +  
               label_menos_1 * cidade_nao_sede * label_mais_1 
             
    )
  
  
  particoes_com_troca_pre_onde_troca <- particoes_com_troca_pre_onde_troca %>% 
    rename (sede = sede_nova, label = label_novo) %>%
    select(-lucro, -Energia_y) %>% 
    left_join(matriz_para_join_ordenada, c("sede" = "CD_x", "cidades" = "CD_y" )) %>% 
    mutate( lucro = if_else(sede == -1, 0, lucro), Energia_y = if_else(sede == -1, 0, Energia_y)  ) %>% 
    replace_na(list(lucro = -1, Energia_y = 0  )  ) %>% 
    select(sede, cidades, indice, label, lucro, Energia_y)
  
  
  
  particoes_com_troca_pre <- 
    particoes_com_troca_pre %>%  
    rename(sede = sede.x, label = label.x) %>% 
    select(sede,cidades,indice, label, ind_municipio.x, ind_municipio.y, lucro, Energia_y ) %>% 
    mutate(troca = ind_municipio.x == ind_municipio.y) %>% 
    filter(!troca) %>% 
    select(sede,cidades,indice, label, lucro, Energia_y ) 
  
  
  particoes_com_troca_pre <- particoes_com_troca_pre %>% 
    bind_rows(particoes_com_troca_pre_onde_troca)
  
  
  #FIM FORMA ALTERNATIVA
  
  #A PARTIR DAQUI FAZ AS TROCAS
  
  
  #particoes_com_troca_pre <- particoes_com_unitario %>% 
  #  crossing(trocas)  
  
  #particoes_com_troca_pre <- particoes_com_troca_pre %>% 
  #  rename( sede.x = sede, sede.y = sede1, ind_municipio.x = ind_municipio, ind_municipio.y = ind_municipio1, label.x = label, label.y = label1 )
  
  #particoes_com_troca_pre <- particoes_com_troca_pre %>% 
  #  mutate(
  #    nao_troca = ind_municipio.x != ind_municipio.y, 
  #    troca = ind_municipio.x == ind_municipio.y, 
  #    nao_sede_0 = sede.y != 0, 
  #    sede_0 = sede.y == 0,
  #    nao_label_menos_1 = label.y != -1 ,
  #    label_menos_1 =  label.y == -1,
  #    cidade_eh_sede = cidades == sede.x,
  #    cidade_nao_sede = cidades != sede.x
  
  #  ) 
  
  
  
  
  #    particoes_com_troca_pre <- particoes_com_troca_pre %>% 
  #     mutate ( sede_nova =  nao_troca * sede.x + 
  #                 troca * nao_sede_0 * sede.y + 
  #                 troca * sede_0 * cidades,
  #               label_novo = 
  #                 nao_troca * label.x + 
  #                 troca * nao_label_menos_1 * label.y + 
  #                troca * label_menos_1 * cidade_eh_sede * label.x +  
  #                 troca * label_menos_1 * cidade_nao_sede * label_mais_1 
  
  #      )
  
  
  #  particoes_com_troca_pre <- particoes_com_troca_pre %>% 
  #        rename (sede = sede_nova, indice = ind_troca, label = label_novo) 
  
  
  
  #ATE AQUI FAZ AS TROCAS
  
  particoes_com_troca <- particoes_com_troca_pre %>%  
    select (sede, cidades, indice, label,  lucro, Energia_y ) 
  
  
  particoes <- calcula_lucro_varias_especificacoes(particoes_com_troca)  
  
  #particoes_com_troca <- particoes_com_troca %>% 
  
  
  
  #  mutate(sede= as.factor(sede),cidades = as.factor(cidades))
  
  #particoes <- calcula_lucro_varias_especificacoes(particoes_com_troca)  
  particoes
  
  
}



realiza_pedaco_passo_busca_local <- function(sedes, ind_municipios, particoes_com_unitario, max_label)
{

  
  fator_disponibilidade = as.double(parametros[i,"fator_disponibilidade"])
  alfa = as.double( parametros[i,"alfa_custo_usina"])
  beta = as.double( parametros[i,"beta_custo_usina"])
  perc_opex = as.double( parametros[i,"percentual_OPEX"])
  
  
  matriz_para_join_ordenada <- matriz_ordenada %>% 
    select(CD_x, CD_y, lucro, Energia_y )
  

  ind_sedes <- sedes %>% 
    select( indice, unitario, sede, label ) %>% 
    #adicionando uma sede "zero", que significa levar o municipio para uma sede dele
    add_row( indice = as.integer(0), unitario = as.integer(1), sede = as.integer(0), label = as.integer(-1)  ) %>% 
    #adicionando uma sede -1, que significa levar o municipio região que não fornece
    add_row( indice = as.integer(-1), unitario = as.integer(1), sede = as.integer(-1), label = as.integer(0)  ) %>% 
    rename (ind_sede = indice)
  
  trocas <- inner_join( ind_sedes, ind_municipios ) %>% 
    mutate (ind_troca = cumsum(unitario) )
  
  particoes_com_unitario <- particoes_com_unitario %>% 
    rename (ind_municipio = indice) %>% 
    select( -lucro)


  trocas <- trocas %>% 
    select(ind_municipio, sede, label, ind_troca)
  
  particoes_com_unitario <- particoes_com_unitario %>% 
    left_join(matriz_para_join_ordenada, c("sede" = "CD_x", "cidades" = "CD_y" )) %>% 
    mutate( lucro = if_else(sede == -1, 0, lucro), Energia_y = if_else(sede == -1, 0, Energia_y)  ) %>% 
    replace_na(list(lucro = -1, Energia_y = 0  )  )
    
    
    
  particoes_com_unitario <- particoes_com_unitario %>% 
    group_by(sede) %>% 
    mutate(n_sede = n()) %>%
    ungroup() %>% 
    select(sede, ind_municipio, label, cidades, lucro, Energia_y, n_sede )
  
  
  sedes_da_particao <- particoes_com_unitario %>% group_by(sede) %>% summarise(n = n())  
  
  label_mais_1 <- as.integer(max_label + 1)
  
  #FORMA ALTERNATIVA DAS TROCAS
  
  
  trocas_e_original <- trocas %>% 
    rename(sede_nova = sede, label_novo = label ) %>% 
    inner_join(particoes_com_unitario, by = c("ind_municipio" = "ind_municipio") ) %>% 
    mutate(sede_nova = if_else(sede_nova == 0, cidades, sede_nova ), label_novo = if_else(sede_nova == 0, label_mais_1, label_novo  )) %>% 
    filter(sede_nova != sede) %>% 
    left_join(matriz_so_lucro_energia_ordenada, by = c("sede_nova" = "CD_x", "cidades" = "CD_y"  )) %>% 
    mutate(lucro.y = if_else(sede_nova == -1, 0, lucro.y)) %>% 
    mutate(lucro.y = if_else(is.na(lucro.y), -1, lucro.y)) %>% 
    replace_na( list(Energia_y.y = 0 ) ) %>% 
    mutate(delta_lucro_sem_usina = lucro.y - lucro.x) %>% 
    left_join(sedes_da_particao, by = c("sede_nova" = "sede" )) %>% 
    #n_usinas na sede antiga antes
    mutate(n_usinas_sede_antiga_antes = if_else( sede!=-1 , 1, 0 )) %>% 
    #n_usinas na sede nova antes
    mutate(n_usinas_sede_nova_antes = if_else( sede_nova !=-1 & !is.na(n), 1, 0 )) %>% 
    #n_usinas na sede antiga agora
    mutate(n_usinas_sede_antiga_agora = if_else( sede == -1 | (sede_nova != sede & n_sede == 1), 0, 1 )) %>% 
    #n_usinas na sede nova agora
    mutate(n_usinas_sede_nova_agora = if_else( sede_nova !=-1 , 1, 0 )) %>% 
    mutate(diff_usinas = (n_usinas_sede_nova_agora + n_usinas_sede_antiga_agora - 
                         (n_usinas_sede_antiga_antes + n_usinas_sede_nova_antes))
          ) %>% 
    mutate(delta_custo_usina = diff_usinas * alfa + diff_usinas * alfa * perc_opex * pv_um_real   ) %>% 
    mutate(delta_energia = Energia_y.y - Energia_y.x) %>% 
    mutate(delta_custo_usina = delta_custo_usina + sign(delta_energia) * (beta * delta_energia * MWH_por_GJ / fator_disponibilidade / (24*365) ) + sign(delta_energia) * (beta * delta_energia * MWH_por_GJ / fator_disponibilidade / (24*365) ) * perc_opex * pv_um_real                 )  %>% 
    #mutate(delta_lucro = -delta_custo_usina - pv(taxa,anos_vida_util_usina,0,delta_lucro_sem_usina,0)  )
    mutate(delta_lucro = -delta_custo_usina + delta_lucro_sem_usina  )
    
    
  trocas_e_original_debug <- trocas_e_original
    
  trocas_e_original <- trocas_e_original %>%         
    top_n(1, delta_lucro) %>%
    rename(sede_velha = sede) %>% 
    select(sede_nova, sede_velha,cidades, label_novo, delta_lucro) %>% 
    identity()
  
  trocas_e_original <- trocas_e_original[1,]
  
  
  
  lucro_adicional_escalar <- first(trocas_e_original$delta_lucro)
  
  particoes_com_unitario <- particoes_com_unitario %>%  
    left_join(trocas_e_original, by = c("cidades"="cidades" )   ) %>% 
    mutate(sede = if_else(is.na(sede_nova), sede, sede_nova ), label = if_else(is.na(sede_nova), label, label_novo ), sede_mexeu = if_else(is.na(sede_nova), 0, 1 ), lucro_adicional = lucro_adicional_escalar   ) %>% 
    select(sede, cidades, label, sede_mexeu, lucro_adicional) %>% 
    #left_join( trocas_e_original, by = c("sede"="sede_velha" )) %>% 
    #mutate(sede_mexeu = if_else(is.na(sede_velha), sede_mexeu, 1 )  ) %>% 
    identity()
  

  

      
  particoes_com_unitario
  

}

junta_pedacos_busca_local <- function(sedes, ind_municipios, particoes_com_unitario, max_label, n_pedacos)
{
  
  #tam_pedaco <- as.integer(count(ind_municipios) %/% n_pedacos + 1)
  
  #lucro_melhor_pedaco <- -1000000000000
  
  #for (i in 1:n_pedacos)
  #{
  #  ind_municipios_pedaco <- ind_municipios %>% 
  #    mutate(divisao = ind_municipio %/% tam_pedaco + 1) %>% 
  #    filter(divisao == i) %>% 
  #    select(ind_municipio, unitario)
    
  #  melhor_do_pedaco <- realiza_pedaco_passo_busca_local(sedes = sedes, ind_municipios = ind_municipios_pedaco, particoes_com_unitario = particoes_com_unitario, max_label = max_label)
  #  if (max(melhor_do_pedaco$lucro) > lucro_melhor_pedaco)
  #  {
  #    resposta <- melhor_do_pedaco
  #    lucro_melhor_pedaco <- max(melhor_do_pedaco$lucro)
  #  }
  #  

  #}
  
  resposta <- realiza_pedaco_passo_busca_local(sedes = sedes, ind_municipios = ind_municipios, particoes_com_unitario = particoes_com_unitario, max_label = max_label)
  
  resposta

}


realiza_passo_busca_local <- function(particoes) {
  
  
  max_label <- max(particoes$label)
  

  particoes_com_unitario <- particoes %>% 
    mutate( unitario = as.integer(1)) %>% 
    mutate( indice = cumsum(unitario) )


  sedes <- particoes %>% 
    select(sede, label) %>% 
    distinct (sede, label) %>% 
    mutate( unitario = as.integer(1)) %>% 
    mutate( indice = cumsum(unitario) )

  
  #montando as trocas
  #cada municipio troca para cada regiao
  ind_municipios <- particoes_com_unitario %>% 
    select( indice, unitario ) %>% 
    rename (ind_municipio = indice)
  
  
  #particoes_2 <- junta_pedacos_busca_local(sedes = sedes, ind_municipios = ind_municipios, particoes_com_unitario = particoes_com_unitario, max_label = max_label, n_pedacos = 2)
  
  particoes_1 <- junta_pedacos_busca_local(sedes = sedes, ind_municipios = ind_municipios, particoes_com_unitario = particoes_com_unitario, max_label = max_label, n_pedacos = 1)
  
  #particoes_2 <- junta_pedacos_busca_local(sedes = sedes, ind_municipios = ind_municipios, particoes_com_unitario = particoes_com_unitario, max_label = max_label, n_pedacos = 5)
  
  #particoes_3 <- junta_pedacos_busca_local(sedes = sedes, ind_municipios = ind_municipios, particoes_com_unitario = particoes_com_unitario, max_label = max_label, n_pedacos = 10)
  
  #particoes_4 <- junta_pedacos_busca_local(sedes = sedes, ind_municipios = ind_municipios, particoes_com_unitario = particoes_com_unitario, max_label = max_label, n_pedacos = 20)
  
  particoes_1
      
}


  




gera_mapa_da_particao <- function(particoes){
  
  
  
  particoes_com_nome <- particoes %>%
    mutate( iteracao_perturbacao = perturbacao * 1000 + iteracao  ) %>% 
    left_join( municipios_escopo, by = c("sede" = "CD") ) %>% 
    select (sede, cidades, Nome, iteracao, iteracao_perturbacao) %>% 
    rename( regiao = Nome ) %>% 
    mutate (regiao = fct_drop( regiao )) %>% 
    mutate( nome_sede = regiao ) %>% 
    mutate( nome_sede = (ifelse(sede == cidades, as.character(nome_sede), NA )) ) %>% 
    mutate(eh_sede = ifelse(sede == cidades, 1, 0) ) %>% 
    filter (iteracao %% intervalo == 1 | iteracao == n_distinct(.$iteracao)   )
  
  
  cidades <- get_brmap(geo = "City")
  
  UFs <- get_brmap(geo = "State")
  
  cidades <- sp::merge(cidades, particoes_com_nome, by.x = "City", by.y = "cidades"  )
  
  sedes_das_particoes <- particoes %>% 
    group_by(sede) %>% 
    select(sede, iteracao) %>% 
    filter(sede != -1) %>% 
    distinct(sede)
  
  
  
  sedes <- sp::merge(cidades, sedes_das_particoes, by.x = "City", by.y = "sede"  )
  
  #  obj_tm_shape <- tm_shape(cidades) +
  #    tm_fill(col ="regiao" ) +
  #    tm_text(text = "nome_sede") +
  #    tm_facets(along = "iteracao") +
  #    tm_shape(sedes) +
  #    tm_borders() +
  #    tm_facets(along = "iteracao")
  
  
  obj_tm_shape <- tm_shape(cidades) +
    tm_fill(col ="regiao" ) +
    tm_bubbles(size ="eh_sede", size.lim = c(0.1, 1.1), scale = 0.5  ) +     
    tm_text(text = "nome_sede") +
    tm_facets(along = "iteracao_perturbacao")
  
  
  
  
  
  animation_tmap(obj_tm_shape, "c:\\temp\\graficoanimado.mpg")
  
  
  
  
}

  
  

gera_video_das_particoes<- function(particoes, intervalo){
  
  
  particoes_grupo <- particoes %>% 
    group_by(iteracao, perturbacao) %>% 
    summarise(lucro_group = max(lucro)) %>% 
    arrange( desc(lucro_group) )
  
  ultima_iteracao_das_perturbacoes <- particoes %>% 
    group_by(perturbacao) %>% 
    summarise(ultima_iteracao = max(iteracao), lucro_perturbacao = max(lucro) ) %>% 
    select(perturbacao, ultima_iteracao, lucro_perturbacao )
    
  
  particoes_com_nome <- particoes %>%
    mutate( iteracao_perturbacao = perturbacao * 1000 + iteracao  ) %>% 
    left_join( municipios_escopo, by = c("sede" = "CD") ) %>% 
    select (sede, cidades, Nome, iteracao, iteracao_perturbacao, perturbacao, label) %>% 
    rename( regiao = Nome ) %>% 
    mutate (regiao = fct_drop( regiao )) %>% 
    mutate( nome_sede = regiao ) %>% 
    mutate( nome_sede = (ifelse(sede == cidades, as.character(nome_sede), NA )) ) %>% 
    mutate(eh_sede = ifelse(sede == cidades, 1, 0) ) %>% 
    left_join( ultima_iteracao_das_perturbacoes  ) %>% 
    filter (iteracao %% intervalo == 1 | iteracao ==  ultima_iteracao ) %>% 
    mutate (label = as.factor(label))
    
  
  cidades <- get_brmap(geo = "City")
  
  UFs <- get_brmap(geo = "State")
  
  cidades <- sp::merge(cidades, particoes_com_nome, by.x = "City", by.y = "cidades"  )
  
  sedes_das_particoes <- particoes %>% 
    group_by(sede) %>% 
    select(sede, iteracao) %>% 
    filter(sede != -1) %>% 
    distinct(sede)
  
  
  
  sedes <- sp::merge(cidades, sedes_das_particoes, by.x = "City", by.y = "sede"  )
  
#  obj_tm_shape <- tm_shape(cidades) +
#    tm_fill(col ="regiao" ) +
#    tm_text(text = "nome_sede") +
#    tm_facets(along = "iteracao") +
#    tm_shape(sedes) +
#    tm_borders() +
#    tm_facets(along = "iteracao")

  
  obj_tm_shape <- tm_shape(cidades) +
    tm_fill(col ="label" ) +
    tm_bubbles(size ="eh_sede", size.lim = c(0.1, 1.1), scale = 0.5  ) +     
    tm_text(text = "nome_sede") +
    tm_facets(along = "iteracao_perturbacao")
  
    
  print(particoes_grupo)   
  
  print(ultima_iteracao_das_perturbacoes)

  animation_tmap(obj_tm_shape, "c:\\temp\\graficoanimado.gif", delay = 50)
  
  
  
  
}


limpa_particoes <- function(particoes){
  #Jogando as cidades sem lucro para a região para a partição vazia
  
  
#  particoes <- particoes %>% 
#    left_join(matriz_so_lucro_energia, c("sede" = "CD_x", "cidades" = "CD_y" )) %>% 
#    mutate(sede = if_else(is.na(lucro.y), as.integer(-1), sede ), label = if_else(is.na(lucro.y), as.integer(0), label ) ) %>% 
#    rename(lucro = lucro.x) %>% 
#    select(sede, cidades, lucro, label, iteracao, perturbacao, MoJo_ate_melhor, inv_prob_sede_existente) %>% 
#    identity()

  
  particoes <- particoes %>% 
    left_join(matriz_so_lucro_energia, c("sede" = "CD_x", "cidades" = "CD_y" )) %>% 
    replace_na(list(lucro.y = -1)) %>% 
    mutate(sede = 
             (lucro.y == -1) * as.integer(-1) + 
             (lucro.y != -1) * sede , 
         
           label = 
             (lucro.y == -1) *  as.integer(0) + 
             (lucro.y != -1) * label 
           )  %>% 
    rename(lucro = lucro.x) %>% 
    select(sede, cidades, lucro, label, iteracao, perturbacao, MoJo_ate_melhor, inv_prob_sede_existente) %>% 
    identity()
  
  
  particoes
  
}
  
  

  

calculaMoJo <- function(A, B){
  
  
  A <- A %>% 
    mutate (sede = dense_rank(label), cidades = dense_rank(cidades))
  
  B <- B %>% 
    mutate (sede = dense_rank(label), cidades = dense_rank(cidades))
  
    
  #colocando as tags de B em A
  #Figura 2
  A <- A %>% 
    inner_join(B,by = c("cidades", "cidades")) %>% 
    transmute(sede = sede.x, tag = sede.y, cidades = cidades) 
  
  #calculando as interseções v_i,j
  
  
  i <- new_tibble(list(i = 1:max(A$sede), unitario = rep(1,max(A$sede)))) 
  
  j <- new_tibble(list(j = 1:max(B$sede), unitario = rep(1,max(B$sede)))) 
  
  
  
  v <- i %>% 
    inner_join(j) %>% 
    left_join(A, c("i" = "sede", "j" = "tag" ) ) %>% 
    mutate(bateu = if_else(is.na(cidades),0,1)) %>% 
    group_by(i,j) %>% 
    summarise( v = sum(bateu)) %>% 
    identity()
  
  
  vmax <- v %>% 
    group_by(i) %>% 
    summarise(max = max(v))
  
  #Figura 4
  
  Ai_em_Gk <- v %>% 
    inner_join(vmax, by = c("i"="i", "v"="max" )) %>% 
    ungroup(i) %>% 
    transmute( i = i, k = j  ) %>% 
    mutate(unitario = 1) %>% 
    identity()
  
  
  vertices_bipartido <- Ai_em_Gk %>% 
    mutate(k = -k)
  
  
  grafo <- graph_from_data_frame(vertices_bipartido, directed = FALSE)
  
  
  matches <- maxmatching(grafo)
  
  matches.tibble <- as.tibble(matches$matching) %>% 
    rownames_to_column( var = "i") %>% 
    transmute( i = as.integer(i), k = as.integer(value) ) %>% 
    filter( i > 0) %>% 
    mutate( k = -1 * k) %>% 
    mutate(i = as.integer(i)) %>% 
    mutate(k = as.integer(k)) %>% 
    identity()
  
  As_sem_match <- matches.tibble %>% 
    filter ( (is.na(k)) ) %>% 
    left_join(Ai_em_Gk, by = c("i","i")) %>% 
    group_by(i) %>% 
    summarise( k = max(k.y) ) %>% 
    mutate( i = as.integer(i)) %>% 
    mutate(k = as.integer(k)) %>% 
    identity()
  
  #Figura 6
  
  Gs <- matches.tibble %>% 
    filter( !is.na(k)) %>% 
    union(As_sem_match) %>% 
    transmute(Ai = i, Gk = k ) %>% 
    identity()
  
  n_groups_nao_vazios <- Gs %>% 
    select(Gk) %>% 
    distinct() %>% 
    nrow()
  
  
  #empty cluster to each empty group
  
  
  maior_sede_A <- A %>% 
    select(sede) %>% 
    top_n(1, sede) %>% 
    distinct() %>%
    as.integer()
  
  grupos_vazios <- B %>% 
    select(sede) %>% 
    distinct() %>% 
    anti_join(Gs,by = c("sede" = "Gk") ) %>% 
    mutate(unitario = 1, contador = cumsum(unitario)) %>% 
    identity()
  
  clusters_vazios <- grupos_vazios %>% 
    transmute(unitario = 1) %>% 
    transmute(Ai = maior_sede_A + cumsum(unitario), contador = cumsum(unitario) ) %>% 
    identity()
  
  clusters_em_grupos_adicionais <- grupos_vazios %>% 
    inner_join(clusters_vazios) %>% 
    transmute(Ai = Ai, Gk = sede) %>% 
    identity()
  
  Gs <- Gs %>% 
    union(clusters_em_grupos_adicionais)
  
  #Figura 6 com elementos
  tags_com_Gs <- A %>% 
    inner_join(Gs, by = c("sede" = "Ai") )
  
  
  #For each group Gk, 
  #we move all objects tagged with Tk that belong to clusters in other groups to any cluster in Gk
  
  moves <- tags_com_Gs %>% 
    filter (tag != Gk) %>% 
    inner_join(Gs) %>% 
    group_by(cidades) %>% 
    summarise(Ai = min(Ai)) 
  
  n_moves <- nrow(moves)
  
  n_moves_formula <- nrow(A) - sum(vmax$max)
  
  
  #Calculando o número de joins
  
  n_clusters_A <- A %>% 
    select(sede) %>%
    distinct() %>% 
    nrow()
  
  n_joins <- n_clusters_A - n_groups_nao_vazios
  
  
  MoJo <- n_joins + n_moves
  

  MoJo  
  
  
}
  

pv_multi <- function(r, n, fv, pmt, type )
{
  
  
  fluxo <- tibble(ano = c(1:n), fv = pmt) %>% 
    mutate(desconto = (1+r)^ano ) %>% 
    mutate(pv = fv /desconto ) %>% 
    identity()
  
  -sum(fluxo$pv)
  
}


#Execucao do algoritmo

#Loop principal 




#heuristicas = c("H1", "H2", "HRand")


particoes_iteracoes <- tibble(iteracao = integer(), perturbacao = integer(), sede = integer(), cidades = integer(), lucro = double(), label = integer(), MoJo_ate_melhor = integer(), inv_prob_sede_existente = integer())

#inv_prob_sede_existente_params <- c(5,7,10,15,20)

inv_prob_sede_existente_params <- c(15)



particoes_com_troca_pre <- tibble()





for ( rodada in 1:1)
{



      
      
      
      for (i in GL_CENARIO_INICIO:GL_CENARIO_FIM)#nrow(parametros))  
      {
        
        
        for (h in (1:1))
        {

          cat("\014")  
          
          particoes_iteracoes <- tibble(iteracao = integer(), perturbacao = integer(), sede = integer(), cidades = integer(), lucro = double(), label = integer(), MoJo_ate_melhor = integer(), inv_prob_sede_existente = integer())
          
          
          #municipios <-  read.csv("C:\\Mestrado\\junta.csv",sep = ";", encoding = "UTF-8",dec = "," ) %>% as_tibble()
          municipios <-  read.csv("c:\\temp\\carga_energia_v2.csv",sep = ",", encoding = "UTF-8",dec = "." ) %>% as_tibble() 
          
          #Lendo dados de distÃ¢ncias reais entre municipios
          
          #distancias <-  read.csv("C:\\Mestrado\\distanciarealmetrossede.csv",sep = "," ) %>% 
          #  as_tibble() 
          
          distancias <-  read.csv("C:\\temp\\distanciarealmetrossede_v2.csv",sep = "," ) %>% 
            as_tibble() 
          
          
          distancias_inv <- distancias %>% 
            rename(CD_yold = CD_y) %>%
            rename(CD_y = CD_x) %>%
            rename(CD_x = CD_yold)
          
          distancias <- distancias %>% bind_rows(distancias_inv)
          
          distancias_inv <- 0
          
          
          semaforo <- readRDS("c:\\temp\\semaforo.rds")
          
          while(!semaforo)
          {
            Sys.sleep(3)
            print("esperando")
            semaforo <- readRDS("c:\\temp\\semaforo.rds")
          }
          
          semaforo <- FALSE
          
          saveRDS(semaforo, "c:\\temp\\semaforo.rds")
          
          UF_scalar <- as.character(parametros[i,"UF"])
          
          
          inv_prob_sede_existente = as.integer(parametros[i,"inv_prob_sede_existente"])
          
          inv_prob_sede_qualquer = as.integer(parametros[i,"inv_prob_sede_qualquer"])

          max_avaliacoes_fitness = as.integer(parametros[i,"max_avaliacoes_fitness"])
          
          cod_cenario_sem_ils_escalar  = as.integer(parametros[i,"cod_cenario_sem_ils"])
          
          heuristica = as.integer(parametros[i,"heuristica"])

          
          inclui_crescimento <- as.integer(parametros[i,"expansao"])
          
          #Escolhendo escopo de local da execucao
          
          
          if (inclui_crescimento == 0)
          {
            
            todos_os_anos <- tibble(Ano = 0:29)
            
            municipios <- municipios %>% 
              filter(Ano == 0) %>% 
              select(-Ano) %>% 
              crossing(todos_os_anos)

          }
            
            
            
          
          municipios_escopo_original <- municipios %>% 
            filter( UF == UF_scalar ) %>% 
            group_by(Cod,Ano) %>% 
            summarise(Energia = sum(Energia)) %>% 
            group_by(Cod) %>% 
            summarise(Energia = max(Energia)) %>%  
            rename(CD = Cod) 

            
          

          matriz <- municipios_escopo_original %>%
            select(CD) %>% 
            mutate(unidade = 1) %>% #Para faxer um full outer join
            full_join(.,., by = c("unidade" = "unidade"), suffix = c("_x", "_y") ) %>% 
            left_join( distancias  ) %>% 
            #left_join( distancias_retas ) %>%
            #distÃ¢ncias sem 
            #replace_na( list(distancia_reta = 10000) ) %>% 
            rename( Distancia  =  distancia ) %>% 
            mutate( Distancia = ifelse (Distancia <0, 10000, Distancia )) %>% 
            mutate( Distancia = ifelse (CD_x == CD_y , 0, Distancia )) %>% 
            select(CD_x, CD_y, Distancia)
          
          municipios_join <- municipios %>% 
            select(Cod, Residuos, Ano, Carga, Energia, Preco_Total) %>% 
            filter(Carga > 0)
          
          
          
          
          matriz <- matriz %>% 
            left_join(municipios_join, by = c("CD_y" = "Cod"))
          
          
          gc()  
            
          matriz <- matriz %>%   
            select(CD_x, CD_y, Distancia, Residuos, Ano, Carga, Energia, Preco_Total) %>% 
            filter(Carga > 0) %>% 
            rename(Carga_y = Carga, Energia_y = Energia, Preco_Total_y = Preco_Total)
          
          
          
          
          
          #matriz_ordem <- matriz %>% 
          #  arrange(CD_X,CD_Y)
          
          
          

          
          
                    
          avaliacoes_fitness <- 0
          taxa <- parametros[i,]$taxa/100
          cod_cenario <- parametros[i,]$cod_cenario
          
          pv_um_real <- -pv_multi(taxa, 30, 0, 1, 1)
            
          
          #Inserindo os custos, receitas e lucros para cada par de cidades
          matriz <- insere_receita_custo_lucro(i)
          
          matriz <- matriz %>% 
            filter(lucro >= 0) %>% 
            group_by(CD_x, CD_y, Ano) %>% 
            summarise(lucro = sum(lucro), Energia_y = sum(Energia_y) )
          
          desconto <- tibble(Ano = c(0:29)) %>% 
            mutate(desconto = (1+ taxa)^Ano)

          matriz <- matriz %>% 
            left_join(desconto, by = c("Ano"="Ano") ) %>% 
            mutate(lucro = lucro / desconto ) %>% 
            group_by(CD_x, CD_y ) %>% 
            summarise(lucro = sum(lucro), Energia_y = max(Energia_y))

          
          matriz_so_lucro_energia <- matriz %>% 
            select(CD_x, CD_y, lucro, Energia_y)
          
          matriz_ordenada <- matriz %>% 
            arrange(CD_x, CD_y)
          
          matriz_so_lucro_energia_ordenada <- matriz_so_lucro_energia %>% 
            arrange(CD_x, CD_y)
          
          
          municipios_com_lucro <- matriz %>% 
            select(CD_y) %>% 
            distinct()
          
          municipios_escopo <- municipios_escopo_original %>% 
            semi_join(municipios_com_lucro, by = c("CD" = "CD_y"))
      

          
          semaforo <- TRUE
          
          saveRDS(semaforo, "c:\\temp\\semaforo.rds")
          
          
          
          #vendo se já tem depois primeira perturbacao
          #cache <- read_csv("c:\\temp\\cache_antes_perturbacao.csv") %>% 
          #  filter(cod_cenario_sem_ils == cod_cenario_sem_ils_escalar)
          
          #if (nrow(cache) == 0)
          #{
          
            #Preparando as particoes apos heuristica    
          if (heuristica == 1)
          {
            particoes <- calcula_heuristica_1(municipios_escopo)
          }
          else
          {
            if (heuristica == 2)
            {
              particoes <- calcula_heuristica_2(municipios_escopo, i)
            }
            else
            {
              if (heuristica == 3)
              {
                particoes <- calcula_heuristica_rand(municipios_escopo)
              }
            }
          }
          
          #}
          #else
          #{
          #  particoes <- cache    
          #}
          
          #particoes <- calcula_heuristica_rand(municipios_escopo)
          
          #escolhendo melhor sede
          particoes <- calcula_lucro_escolhendo_sede(particoes)
          
          particoes <- calcula_lucro_escolhendo_sede_da_sede_que_mexeu( particoes )
          
          continua <- TRUE
          maior_lucro_busca_local<- first(particoes$lucro)
          maior_lucro <- first(particoes$lucro)
          melhor_solucao <- particoes
          
          iteracao <- 0
          
          municipios <- 0
          municipios_join <- 0
          matriz <- 0
          distancias <- 0
          
          gc()
          
          
          perturbacao <- 0
          
          continua_perturbacao <- TRUE
          
          while (continua_perturbacao)
          {
            continua = TRUE
          
            maior_lucro_busca_local <- -1000000000000
            ##busca local
            while (continua){
              
            
              iteracao <- iteracao + 1
              
              particoes_anterior <- particoes
              

              

              particoes <- realiza_passo_busca_local( particoes )
              
              lucro_adicional_escalar <- first(particoes$lucro_adicional)
              
              avaliacoes_fitness <- avaliacoes_fitness + 1
              
              
              #if (iteracao %% 10 == 1)
              #{
                particoes <- calcula_lucro_escolhendo_sede(particoes)   
              #}
              particoes <- calcula_lucro_escolhendo_sede_da_sede_que_mexeu( particoes )
              lucro_atual <- first( particoes$lucro ) 
              lucro_atual_sem_negativas <- first( particoes$lucro_sem_negativas ) 
              
              sedes_anterior <- particoes_anterior %>% 
                distinct(sede)
              
              sedes_atual <- particoes %>% 
                distinct(sede)

              if (!isTRUE(all_equal(sedes_anterior, sedes_atual )))
              {
                para <- TRUE
              } 
              
              if (lucro_atual > maior_lucro_busca_local){
                print(particoes %>% count(sede, sort = TRUE))
                print("lucro atual")
                print(format(lucro_atual, big.mark = "."))
                print("lucro sem negativas atual")
                print(format(lucro_atual_sem_negativas, big.mark = "."))
                print("maior_lucro_busca_local")
                print(format(maior_lucro_busca_local, big.mark = "."))
                print("maior_lucro")
                print(format(maior_lucro, big.mark= "."))
                print("param")
                print(inv_prob_sede_existente)
                print("perturba")
                print(perturbacao)
                print("iteracao")
                print(iteracao)
                print("avaliação")
                print(avaliacoes_fitness)
                print("cenario")
                print(cod_cenario)
                print("rodada")
                print(rodada)
                maior_lucro_busca_local = lucro_atual
                melhor_solucao_busca_local <- particoes
                continua = TRUE 
                if (flag_calcula_mojo)
                {
                  MoJo <- calculaMoJo(particoes, melhor_solucao)
                }
                else
                {
                  MoJo = 0
                }
                particoes <- particoes %>% 
                  mutate(taxa = taxa) %>% 
                  mutate(cod_cenario = cod_cenario) %>% 
                  mutate(rodada = rodada) %>% 
                  mutate(iteracao = iteracao) %>% 
                  mutate(perturbacao = perturbacao) %>% 
                  mutate(MoJo_ate_melhor = MoJo) %>% 
                  mutate(inv_prob_sede_existente = inv_prob_sede_existente) %>% 
                  mutate(tempo = Sys.time())
                
                
                #particoes_limpa <- limpa_particoes(particoes)
                
                particoes_iteracoes <- particoes_iteracoes %>% bind_rows(particoes)
                
              }
              else{
                continua = FALSE
                if (maior_lucro_busca_local > maior_lucro){
                  maior_lucro <- maior_lucro_busca_local
                  melhor_solucao <- melhor_solucao_busca_local
                }
                lucro_atual <- maior_lucro_busca_local

                particoes <- calcula_lucro_escolhendo_sede(particoes)   
                particoes <- calcula_lucro_escolhendo_sede_da_sede_que_mexeu( particoes )
                
                particoes <- particoes %>% 
                  mutate(taxa = taxa) %>% 
                  mutate(cod_cenario = cod_cenario) %>% 
                  mutate(rodada = rodada) %>% 
                  mutate(iteracao = iteracao) %>% 
                  mutate(perturbacao = perturbacao) %>% 
                  mutate(MoJo_ate_melhor = MoJo) %>% 
                  mutate(inv_prob_sede_existente = inv_prob_sede_existente) %>% 
                  mutate(tempo = Sys.time())
                
                #particoes_limpa <- limpa_particoes(particoes)
                
                particoes_iteracoes <- particoes_iteracoes %>% bind_rows(particoes)
                
              }
              
              #if (iteracao == 40){
              #  continua = FALSE
              #  if (maior_lucro_busca_local > maior_lucro){
              #    maior_lucro <- maior_lucro_busca_local
              #    melhor_solucao <- melhor_solucao_busca_local
              
              #}
                
              
              
              
              
        
            }
          
            #perturba particao
            #Cada municipio tem 1/20 de mudar para uma sede existente 
             #e 1/300 de mudar para uma sede qualquer 
            
      
            
      
            sedes_existentes <- particoes %>% 
              select(sede, label) %>% 
              distinct(sede, label) 
            
            n_sedes_existentes <- nrow(sedes_existentes)
            
            municipios_existentes <- particoes %>% 
              select(cidades) %>% 
              distinct( cidades)
            
            n_municipios_existentes = nrow(municipios_existentes)
              
            print("perturba")
            print(perturbacao)
            print("maior lucro perturbacao")
            print(format(maior_lucro_busca_local, big.mark = "."))
            print("maior_lucro")
            print(format(maior_lucro, big.mark= "."))
            
            particoes_perturbada <- melhor_solucao %>%  
              mutate (muda_sede_existente = (sample(1:inv_prob_sede_existente,size = n(), replace = TRUE) == 1 )) %>% 
              mutate (ind_sede_existente_destino = sample(1:n_sedes_existentes,size = n(), replace = TRUE)  ) %>% 
              mutate (muda_sede_qualquer = (sample(1:inv_prob_sede_qualquer,size = n(), replace = TRUE) == 1 )) %>% 
              mutate (ind_municipio_existente_destino = sample(1:n_municipios_existentes,size = n(), replace = TRUE)  ) %>% 
              mutate (sede = ifelse(muda_sede_existente, sedes_existentes$sede[ind_sede_existente_destino], sede )) %>% 
              mutate (label = ifelse(muda_sede_existente, sedes_existentes$label[ind_sede_existente_destino], label )) %>% 
              mutate (sede = ifelse(muda_sede_qualquer, cidades, sede )) %>% 
              mutate (label = ifelse(muda_sede_qualquer, cidades, label )) %>% 
              select(sede, cidades, label)
            
            particoes <- calcula_lucro_escolhendo_sede(particoes = particoes_perturbada )    
      
            

            #if (lucro_atual > maior_lucro){
            #  maior_lucro_busca_local = lucro_atual
            #  print(lucro_atual)
            #  continua_perturbacao = TRUE 
            #}
            #else{
            #  continua_perturbacao = TRUE
            #}  
            
            
            if (avaliacoes_fitness > max_avaliacoes_fitness){
              continua_perturbacao = FALSE
            }
            else 
            {      
              perturbacao <- perturbacao + 1
              iteracao <- 1
              
              #MoJo <- calculaMoJo(particoes, melhor_solucao)
        
              particoes <- particoes %>% 
                mutate(iteracao = iteracao) %>% 
                mutate(perturbacao = perturbacao) %>% 
                #mutate(MoJo_ate_melhor = MoJo) %>% 
                mutate(inv_prob_sede_existente = inv_prob_sede_existente) %>% 
                mutate(tempo = Sys.time())
              
      
              #particoes_limpa <- limpa_particoes(particoes)
              
              if (perturbacao %% 10 == 0)
              {
                write_csv(particoes_iteracoes,paste("c:\\temp\\final_",as.character(GL_CENARIO_INICIO), "_ATE_", as.character(GL_CENARIO_FIM), "-perturb-", as.character(perturbacao),"-rodada-", as.character(rodada), "-codcenario-", cod_cenario, ".csv", sep=""))
                particoes_iteracoes <- particoes %>% 
                  mutate(taxa = taxa) %>% 
                  mutate(cod_cenario = cod_cenario) %>% 
                  mutate(rodada = rodada)

                cat("\014")  
                
              }
              else
              {
                particoes <- particoes %>% 
                  mutate(taxa = taxa) %>% 
                  mutate(cod_cenario = cod_cenario) %>% 
                  mutate(rodada = rodada)
                particoes_iteracoes <- particoes_iteracoes %>% bind_rows(particoes)
              }
            }
          }
          
          write_csv(particoes_iteracoes,paste("c:\\temp\\final_",as.character(GL_CENARIO_INICIO), "_ATE_", as.character(GL_CENARIO_FIM), "-perturb-", as.character(perturbacao),"-rodada-", as.character(rodada), "-codcenario-", cod_cenario, ".csv", sep=""))
          
          cat("\014")  
          #gera_video_das_particoes (particoes_iteracoes, intervalo = 1 )
          
        
        }
      }
}


      
    
