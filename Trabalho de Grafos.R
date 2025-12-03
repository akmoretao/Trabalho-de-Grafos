# -------------------------------------------------------------------------
# ANÁLISE DE GRAFOS DE REDES SOCIAIS
# Matemática Discreta - Professora Bianca S. de Mira
# -------------------------------------------------------------------------

# -------------------- IMPORTAÇÃO E PREPARAÇÃO DO DATASET --------------------

# Lê o arquivo de dados .txt selecionado pelo usuário.
# O arquivo deve conter as colunas: from (Pessoa), to (Gênero) e weight (Peso).
dados <- read.csv(file.choose(), header = TRUE, sep = ",")

# -------------------- GERAÇÃO E OPERAÇÃO DAS MATRIZES --------------------

# MATRIZ DE INCIDÊNCIA (M)
# A função 'xtabs' cria a matriz M, somando os 'weights' onde há ligação entre 'from' (Linhas) e 'to' (Colunas).
matriz_inc <- xtabs(weight ~ from + to, data = dados)
# Transforma o objeto 'table' em uma matriz simples para permitir operações matriciais.
matriz_inc <- unclass(matriz_inc)
# Exibe a Matriz de Incidência.
print(matriz_inc)

# MATRIZ DE SIMILARIDADE (S)
# Cálculo da Matriz de Similaridade: S = M x M^T (Matriz Incidência vezes sua Transposta).
# Esta matriz projeta a rede nas Pessoas (1ª Unidade de Análise).
s <- matriz_inc %*% t(matriz_inc)
# Zera a diagonal: remove a similaridade de uma pessoa consigo mesma (o quebra-cabeça).
diag(s)<-0
# Exibe a Matriz de Similaridade.
print(s)

# MATRIZ DE COOCORRÊNCIA (C)
# Cálculo da Matriz de Coocorrência: C = M^T x M (Transposta da Matriz Incidência vezes a Matriz Incidência).
# Esta matriz projeta a rede nos Gêneros (2ª Unidade de Análise).
c <- t(matriz_inc) %*% matriz_inc
# Zera a diagonal: remove a coocorrência de um gênero consigo mesmo.
diag(c)<-0
# Exibe a Matriz de Coocorrência.
print(c)


# -------------------- CRIAÇÃO DOS OBJETOS GRAFO com iGRAPH --------------------

# Instala o pacote 'igraph'.
install.packages('igraph') 
# Carrega o pacote 'igraph' para manipulação e análise de grafos.
library('igraph')

# Grafo de Incidência (Bipartido): Usa a matriz M.
grafo_inc<-graph_from_biadjacency_matrix(matriz_inc, directed = T, mode="out", weighted = T) 

# Grafo de Similaridade: Usa a matriz S.
grafo_sim <- graph_from_adjacency_matrix(s, weighted = T, mode = "undirected")

# Grafo de Coocorrência: Usa a matriz C.
grafo_co <- graph_from_adjacency_matrix(c, weighted = T, mode = "undirected")


# -------------------- VISUALIZAÇÃO DOS GRAFOS --------------------

# Plotagem do Grafo de Incidência.
plot(grafo_inc, edge.width=E(grafo_inc)$weight, edge.arrow.size=0.1)
# Plotagem do Grafo de Similaridade.
plot(grafo_sim, edge.width=E(grafo_sim)$weight)
# Plotagem do Grafo de Coocorrência.
plot(grafo_co, edge.width=E(grafo_co)$weight)


# -------------------- CÁLCULO DAS MÉTRICAS TOPOLÓGICAS (GRAFO SIMILARIDADE) --------------------

# Identifica e lista os vértices do grafo de Similaridade.
V(grafo_sim) 
# Contagem do número total de vértices (pessoas).
length(V(grafo_sim))
# Identifica e lista as arestas (ligações) do grafo de Similaridade.
E(grafo_sim) 
# Contagem do número total de arestas (ligações).
ecount(grafo_sim)
# Grau de Vértice: Número de ligações de cada pessoa.
degree(grafo_sim)
# Grau Médio da Rede: Média das ligações.
mean(degree(grafo_sim))
# Exibe os pesos (força) das arestas (o quão similares as pessoas são).
E(grafo_sim)$weight
# Força de Conectividade Média: Média dos pesos das arestas.
mean(E(grafo_sim)$weight)
# Densidade da Rede: Proporção de ligações existentes vs. possíveis.
edge_density(grafo_sim, loops = FALSE)

# Métrica de Grau (Similaridade).
print("--- MÉTRICAS: GRAFO DE SIMILARIDADE ---")
print("Grau (degree):")
print(degree(grafo_sim))
# Centralidade de Intermediação (Betweenness): O quão 'ponte' cada nó é.
print("Intermediação (betweenness):")
print(betweenness(grafo_sim, normalized = TRUE))
# Centralidade de Proximidade (Closeness): Quão perto o nó está de todos os outros.
print("Proximidade (closeness):")
print(closeness(grafo_sim, normalized = TRUE))
# Diâmetro da Rede: O caminho mais longo entre dois nós.
print("Diâmetro:")
print(paste(diameter(grafo_sim)))

# -------------------- CÁLCULO DAS MÉTRICAS TOPOLÓGICAS (GRAFO COOCORRÊNCIA) --------------------

# Métrica de Grau (Coocorrência).
print("--- MÉTRICAS: GRAFO DE COOCORRÊNCIA ---")
print("Grau (degree):")
print(degree(grafo_co))
# Centralidade de Intermediação (Betweenness) para Gêneros.
print("Intermediação (betweenness):")
print(betweenness(grafo_co, normalized = TRUE))
# Densidade da Rede: O quão conectados os Gêneros estão.
print("Densidade da Rede:")
print(paste(edge_density(grafo_co, loops = FALSE)))

# -------------------- CÁLCULO DAS MÉTRICAS TOPOLÓGICAS (GRAFO INCIDÊNCIA) --------------------

# Métrica de Grau (Incidência).
print("--- MÉTRICAS: GRAFO DE INCIDÊNCIA ---")
print("Grau (degree):")
print(degree(grafo_inc))
# Centralidade de Intermediação (Betweenness) para Incidência.
print("Intermediação (betweenness):")
print(betweenness(grafo_inc, normalized = TRUE))