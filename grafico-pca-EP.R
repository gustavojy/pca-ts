
# Gráfico Erro padrão assintótico x clássico

#### Rodar com `pca-ts-inflacao.R`

library(ggplot2)

# 16 grupos alimentícios
categorias <- c("Cereais, leguminosas e oleaginosas", "Farinhas, féculas e massas", 
                "Tubérculos, raízes e legumes", "Açúcares e derivados", 
                "Hortaliças e verduras", "Frutas", "Carnes", "Pescados", 
                "Carnes e peixes industrializados", "Aves e ovos", 
                "Leites e derivados", "Panificados", "Óleos e gorduras", 
                "Bebidas e infusões", "Enlatados e conservas", "Sal e condimentos")

# Data Frame para os 4 primeiros Componentes Principais
k_pcs <- 4
df_razao <- data.frame(
  Categoria = factor(rep(categorias, times = k_pcs), levels = rev(categorias)),
  Indice_Var = rep(1:length(categorias), times = k_pcs),
  PC = factor(rep(paste0("PC", 1:k_pcs), each = length(categorias)), 
              levels = paste0("PC", 1:k_pcs)),
  SE_MBB = as.vector(se_mbb[, 1:k_pcs]),
  SE_IND = as.vector(se_ind[, 1:k_pcs]),
  Sig_MBB = as.vector(sig_mbb[, 1:k_pcs]),
  Sig_IND = as.vector(sig_ind[, 1:k_pcs])
)

# Cálculo das Métricas de Comparação
# Razão Direta: Quantas vezes o erro real (MBB) é maior que o erro clássico (IND)
df_razao$Razao_SE <- df_razao$SE_MBB / df_razao$SE_IND

# Diferença Percentual (Equação 14 do artigo): (SE_IND / SE_MBB - 1) * 100
df_razao$Delta_Pct <- (df_razao$SE_IND / df_razao$SE_MBB - 1) * 100

# Identificação do impacto na inferência (Falsos Positivos do método clássico)
df_razao$Impacto <- ifelse(df_razao$Sig_IND & !df_razao$Sig_MBB, 
                           "Falso Positivo no Clássico", 
                           "Decisão Convergente")


# Gráfico Razão SE_MBB / SE_IND ----
p1 <- ggplot(df_razao, aes(x = Razao_SE, y = Categoria, color = Impacto)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray30", linewidth = 0.8) +
  geom_segment(aes(x = 1, xend = Razao_SE, y = Categoria, yend = Categoria), 
               linewidth = 0.7, alpha = 0.8) +
  geom_point(size = 2.8) +
  facet_wrap(~ PC, ncol = 4) +
  scale_color_manual(values = c("Falso Positivo no Clássico" = "#E41A1C", 
                                "Decisão Convergente" = "#1B4F72")) +
  theme_minimal() +
  labs(title = "Subestimação da Incerteza: Razão entre Erros Padrão (MBB / Clássico)",
       subtitle = "Valores > 1 indicam a magnitude do erro ignorado pelo PCA Clássico; pontos vermelhos geraram falsos positivos",
       x = "Razão do Erro Padrão (SE Assintótico / SE Clássico)",
       y = "",
       color = "Efeito no Teste (5%)") +
  theme(legend.position = "bottom",
        axis.text.y = element_text(size = 9, face = "bold"),
        strip.text = element_text(size = 11, face = "bold"),
        panel.spacing = unit(1, "lines"),
        plot.title = element_text(face = "bold", size = 12))

print(p1)
