### Tratando a planilha ###

pim<-readxl::read_xlsx("pim.xlsx")

colnames(pim)[colnames(pim) %in% c("FALSE...1", "abertura da nadadeira", "FALSE...3", "aspect ratio", "FALSE...5")] <- c("sp", "abert_nad", "area_nad", "aspect_ratio", "length")

pim<- na.exclude(pim)

pim$Classe<-factor(pim$Classe)

pim <- pim[-44, ]

pim$aspect_ratio <- as.numeric(gsub(",", ".", pim$aspect_ratio))
pim$area_nad<- as.numeric(gsub(",", ".", pim$area_nad))
pim$aspect_ratio<- as.numeric(gsub(",", ".", pim$aspect_ratio))
pim$length<- as.numeric(gsub(",", ".", pim$length))
pim$abert_nad<- as.numeric(gsub(",", ".", pim$abert_nad))

### Tirando o efeito do tamanho sobre aspect_ratio ###

# Passo 1: Ajustar uma regressão linear
modelo <- lm(aspect_ratio ~ length, data = pim)
summary(modelo)

# Passo 2: Obter os resíduos (que representam o aspect ratio ajustado, sem o efeito de tamanho)
resid <- residuals(modelo)

# Passo 3: Adicionar os resíduos ao dataframe (opcional, mas útil para manter o dataframe organizado)
pim$resid <- resid

### Análises exploratórias 

plot(density(pim$resid))# Não me parece precisar de normalização

ggplot(pim, aes(x=Classe,y=resid))+
  geom_boxplot()+
  theme_classic() 


### Definindo K ótimo

pimnum<- subset(pim, select = c(aspect_ratio))

# NbClust - aplica 30 índices diferentes e compara seus resultados

result2 <- NbClust(pimnum, min.nc = 2, max.nc = 10, method = "ward.D2")
print(result$Best.nc)
warnings()

### Clusterização

# Calcular a matriz de distâncias (aqui, estamos usando a distância euclidiana)
dist_matrix <- dist(pimnum)

# Realizar o clustering hierárquico (método de ligação completa, mas você pode escolher outros métodos como "single", "average", etc.)
hc <- hclust(dist_matrix, method = "ward.D2")

# Plotar o dendrograma com os clusters
plot(hc, main = paste("Dendrograma com", 7, "Clusters"),
     xlab = "Observações", ylab = "Distância Euclidiana")

# Adicionar linhas verticais para mostrar os clusters
rect.hclust(hc, k = 6, border = "red")


### ANOVA

# Cortar o dendrograma em clusters
clusters <- cutree(hc, k = 3)

# Adicionar o cluster resultante aos dados filtrados para análise adicional
pim$Cluster <- clusters

pim$Cluster<-factor(pim$Cluster)

anova<-aov(resid~Cluster, data = pim)

tukey_result <- TukeyHSD(anova)

table(tukey_result)

### PLOT TUKEYHSD

anova<-aov(resid~Cluster, data = pim)
tuk <- glht(anova, linfct = mcp(Cluster = "Tukey"))
pvalues <- adjusted()(tuk)$pvalues
ci.glht <- confint(tuk)
plot(ci.glht, col = ifelse(pvalues < 0.05, "red", "black"), 
     xlab = "Difference in mean levels")

### Média de resid por cluster

media_aspect_por_cluster <- aggregate(aspect_ratio ~ Cluster, data = pim, FUN = mean)

print(media_aspect_por_cluster)

# Plot pizza porcentagem cluster

df_clusters <- as.data.frame(table(pim$Cluster))
names(df_clusters) <- c("cluster", "count")

df_clusters$percentage <- round(100 * df_clusters$count / sum(df_clusters$count), 1)

# Passo 2: Criar o gráfico de pizza usando ggplot2
ggplot(df_clusters, aes(x = "", y = percentage, fill = factor(cluster))) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  theme_void() + 
  geom_text(aes(label = paste0(percentage, "%")), 
            position = position_stack(vjust = 0.5)) +
  labs(fill = "Cluster", title = "Porcentagem de Cada Cluster")

# Boxplot

ggplot(pim, aes(x=Cluster, y=aspect_ratio))+
  geom_boxplot()+
  xlab("Clusters")+ ylab("Aspect ratio")+
  theme(legend.title = element_blank())+theme_classic()+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0),size = 25), 
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0),size = 25), 
        panel.grid.major = element_line(colour="white"), 
        panel.grid.minor = element_line(colour="white"), 
        axis.text.y = element_text(size = 25, colour = "black", margin = margin(t = 20, r = 0, b = 0, l = 0)), 
        axis.text.x = element_text(size = 25, colour = "black", margin = margin(t = 20, r = 0, b = 0, l = 0)), 
        strip.text.y = element_text(size = 15),
        legend.title = element_blank(),
        legend.text = element_text(size=15))
