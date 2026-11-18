
# PCA para ST - Aplicação a dados de inflação de alimentos

options(scipen = 999)

library(tidyverse)
library(tseries)
library(boot)

# Dados -------------------------------------------------------------------

dados <- read_rds("./ipca.rds")

itens_nome <- unique(names(dados[,-1]))

dados_tidy <- dados |> 
   pivot_longer(cols = 2:17, names_to = "itens", values_to = "valor") |> 
   mutate(itens = factor(itens, levels = itens_nome))

## Matricial ----
X <- as.matrix(dados[,-1])
n <- nrow(X)
p <- ncol(X)


# Exploratória ------------------------------------------------------------

## Estacionariedade ----

X_estacionario <- X
variaveis_diferenciadas <- c()
alpha <- 0.05


for (i in 1:p) {
   teste_adf <- adf.test(X[, i], alternative = "stationary")
   p_valor <- teste_adf$p.value
   
   # Se o p-valor for maior que 0.05, a série NÃO é estacionária
   if (p_valor > alpha) {
      cat(sprintf("Série '%s': NÃO estacionária (p-valor = %.4f). Aplicando 1ª diferença...\n", 
                  colnames(X)[i], p_valor))
      
      # Aplica a primeira diferença e preenche a 1ª posição com NA para não quebrar a matriz
      X_estacionario[, i] <- c(NA, diff(X[, i]))
      
      # Registra o nome da variável alterada
      variaveis_diferenciadas <- c(variaveis_diferenciadas, colnames(X)[i])
      
   } else {
      cat(sprintf("Série '%s': É estacionária (p-valor = %.4f). Mantida original.\n", 
                  colnames(X)[i], p_valor))
   }
}


if (length(variaveis_diferenciadas) > 0) {
   X_final <- na.omit(X_estacionario)
   cat("\n[!] Operação concluída. A 1ª linha da matriz geral foi removida devido ao processo de diferenciação.\n")
} else {
   X_final <- X_estacionario
   cat("\n[!] Operação concluída. Nenhuma série precisou ser diferenciada.\n")
}


if (length(variaveis_diferenciadas) > 0) {
   cat("\n=== RE-TESTE DAS SÉRIES DIFERENCIADAS ===\n")
   for (var in variaveis_diferenciadas) {
      p_val_novo <- adf.test(X_final[, var], alternative = "stationary")$p.value
      cat(sprintf("Série '%s' APÓS diferença: p-valor = %.4f -> %s\n", 
                  var, 
                  p_val_novo, 
                  ifelse(p_val_novo <= alpha, "OK (Estacionária)", "ALERTA (Ainda Não-Estacionária)")))
   }
}

X <- X_final

## Gráficos ----

dados_tidy |> 
   ggplot(aes(x = data, y = valor, group = itens)) +
   geom_line() +
   facet_wrap(~ itens, nrow = 2, scales = "free")


dados_tidy |> 
   ggplot(aes(sample = scale(valor))) +
   stat_qq() +
   stat_qq_line() +
   facet_wrap(~ itens, nrow = 2, scales = "free") +
   labs(
      x = "Theoretical Quantiles", 
      y = "Sample Quantiles"
   )



# PCA ---------------------------------------------------------------------

## Estimativas Pontuais ----
Cn <- cov(X)
pca_eig <- eigen(Cn)
eigenvalues <- pca_eig$values
loadings <- pca_eig$vectors * (-1)

# loadings[, c(4, 5, 7, 8)] <- -loadings[, c(4, 5, 7, 8)]
loadings[, c(1, 6:9, 11, 12, 14, 15)] <- -loadings[, c(1, 6:9, 11, 12, 14, 15)]
t(loadings) |> round(3)

## Proporção de variação para os 4 primeiros componentes ----
prop_variation <- cumsum(eigenvalues) / sum(eigenvalues)
round(prop_variation[4] * 100, 2)

plot(1:p, prop_variation, type = "b", 
     pch = 19, col = "#073A70",
     main = "Proporção de variação (%)",
     xlab = "Número do Componente",
     ylab = " ",
     axes = TRUE)


## ggplot2 ----
df_var <- data.frame(
   componente = seq_along(prop_variation),
   prop_variation = prop_variation
)

p_var <- ggplot(df_var, aes(x = componente, y = prop_variation)) +
   geom_line(color = "#073A70", linewidth = 2) +
   geom_point(color = "#073A70", size = 5) +
   scale_x_continuous(breaks = seq_along(prop_variation)) +
   labs(
      title = "Proporção de variação (%)",
      x = "Número do Componente",
      y = NULL
   ) +
   theme_minimal(base_size = 28, base_family = "Montserrat") +
   theme(
      legend.position = "none",
      panel.grid.minor = element_blank(),
      plot.title = element_text(hjust = 0.5, face = "bold"),
      axis.title.x = element_text(face = "bold")
   )

# ggsave(
#    "fig.png",
#    plot = p_var,
#    width = 22,
#    height = 24,
#    units = "cm",
#    dpi = 300
# )





## Inferência via Moving Block Bootstrap - MBB (Teorema 1) ----

### Função para MBB
boot_pca_loadings <- function(ts_data, indices) {
   resampled_data <- ts_data[indices, ]
   eig_boot <- eigen(cov(resampled_data))
   
   # Alinhamento de sinais
   # Multiplica o autovetor gerado pelo sinal da correlação com o autovetor original
   aligned_vectors <- eig_boot$vectors
   for (i in 1:ncol(aligned_vectors)) {
      if (sum(aligned_vectors[,i] * loadings[,i]) < 0) {
         aligned_vectors[,i] <- -aligned_vectors[,i]
      }
   }
   return(as.vector(aligned_vectors))
}

### Definição do tamanho do bloco (~ n^(1/3))
block_size <- round(n^(1/3))
boot_rep <- 500  # (baseado no artigo)

### Execução do MBB
boot_results <- tsboot(tseries = X,                    # Matriz X
                       statistic = boot_pca_loadings,  # Função MBB
                       R = boot_rep,                   # Repetições reamostragem
                       l = block_size,                 # Tamanho do bloco
                       sim = "fixed")                  # Blocos de tamanho fixo

### Erros Padrão Assintóticos (Teorema 1)
se_mbb <- matrix(apply(boot_results$t, 2, sd), nrow = p, ncol = p)



## Inferência via Teoria Clássica (Corolário 4) ----
se_ind <- matrix(0, nrow = p, ncol = p)
for (k in 1:p) {
   Sigma_k <- matrix(0, p, p)
   for (i in 1:p) {
      if (i != k) {
         const <- (eigenvalues[k] * eigenvalues[i]) / ((eigenvalues[k] - eigenvalues[i])^2)
         Sigma_k <- Sigma_k + const * (loadings[,i] %*% t(loadings[,i]))
      }
   }
   se_ind[,k] <- sqrt(diag(Sigma_k) / n)
}




# Teste de Hipóteses ------------------------------------------------------
## H0: a_{kk'} = 0

## Nível de significância de 5% (Z-score crítico aprox. 1.96)
z_critical <- 1.96

## Matrizes de t-stats
t_stat_mbb <- loadings / se_mbb
t_stat_ind <- loadings / se_ind

## Matrizes binárias indicando rejeição da nula (True = Significativo)
sig_mbb <- abs(t_stat_mbb) > z_critical
sig_ind <- abs(t_stat_ind) > z_critical




# Resultados --------------------------------------------------------------

## Table 6 - Componentes 1 a 4
## Sinais (+/-) indicam loadings significativos. '0' indica não significativo
## Resultado obtido difere ligeiramente do artigo original. Provavelmente devido a diferença de semente fixada para o MBB pelos autores (não especificam)

format_significance <- function(loadings_mat, sig_mask) {
   res <- matrix("0", nrow = p, ncol = 4)
   rownames(res) <- itens_nome
   colnames(res) <- paste0("PC", 1:4)
   for(i in 1:p) {
      for(j in 1:4) {
         if(sig_mask[i, j]) {
            res[i, j] <- ifelse(loadings_mat[i, j] > 0, "+", "-")
         }
      }
   }
   return(t(res))
}

## MBB (Teorema 1)
format_significance(loadings, sig_mbb)

## PCA Clássico (Corolário 4)
format_significance(loadings, sig_ind)







