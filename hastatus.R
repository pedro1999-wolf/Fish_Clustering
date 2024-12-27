

### Tratando a planilha ###

hast<-readxl::read_xlsx("hastatus.xlsx")

colnames(hast)[colnames(hast) %in% c("FALSE...1", "abertura da nadadeira", "FALSE...3", "aspect ratio", "FALSE...5")] <- c("sp", "abert_nad", "area_nad", "aspect_ratio", "length")

hast<- na.exclude(hast)

hast$classe<-factor(hast$classe)


### Análises exploratórias ###

plot(density(hast$resid))# Não me parece precisar de normalização

ggplot(hast, aes(x=classe,y=resid))+
  geom_boxplot()+
  theme_classic() 

### Tirando o efeito do tamanho sobre aspect_ratio ###

modelo <- lm(aspect_ratio ~ length, data = hast)
resid <- residuals(modelo)
hast$resid <- resid


# Definindo K ótimo

hastnum<- subset(hast, select = c(resid))

# NbClust - aplica 30 índices diferentes e compara seus resultados

result <- NbClust(hastnum, min.nc = 2, max.nc = 10, method = "ward.D2")

warnings()

# Silhueta

fviz_nbclust(hastnum, FUN = hcut, method = "silhouette", hc_method = "ward.D2")



# Calcular a matriz de distâncias (aqui, estamos usando a distância euclidiana)
dist_matrix <- dist(hastnum)

# Realizar o clustering hierárquico (método de ligação completa, mas você pode escolher outros métodos como "single", "average", etc.)
hc <- hclust(dist_matrix, method = "ward.D2")


# Plotar o dendrograma com os clusters
plot(hc, main = paste("Dendrograma com", 3, "Clusters"),
     xlab = "Observações", ylab = "Distância Euclidiana")

# Adicionar linhas verticais para mostrar os clusters
rect.hclust(hc, k = 3, border = "red")


# Cortar o dendrograma em clusters
clusters <- cutree(hc, k = 3)

# Adicionar o cluster resultante aos dados filtrados para análise adicional
hast$Cluster <- clusters

hast$Cluster<-factor(hast$Cluster)

### ANOVA e Tukey para ver se os clusters diferem significativamente

anova<-aov(resid~Cluster, data = hast)

tukey_result <- TukeyHSD(anova)

### Plot do TukeyHSD

# specify all pair-wise comparisons among levels of variable "tension"
tuk <- glht(anova, linfct = mcp(Cluster = "Tukey"))
# p-values
pvalues <- adjusted()(tuk)$pvalues
# get confidence intervals
ci.glht <- confint(tuk)
# plot them
plot(ci.glht, col = ifelse(pvalues < 0.05, "red", "black"), 
     xlab = "Difference in mean levels")

# Média de aspect_ratio por cluster

media_aspect_por_cluster <- aggregate(aspect_ratio ~ Cluster, data = hast, FUN = mean)

print(media_aspect_por_cluster)

# Plot pizza porcentagem cluster

df_clusters <- as.data.frame(table(hast$Cluster))
names(df_clusters) <- c("cluster", "count")

df_clusters$percentage <- round(100 * df_clusters$count / sum(df_clusters$count), 1)

ggplot(df_clusters, aes(x = "", y = percentage, fill = factor(cluster))) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  theme_void() + 
  geom_text(aes(label = paste0(percentage, "%")), 
            position = position_stack(vjust = 0.5)) +
  labs(fill = "Cluster", title = "Porcentagem de Cada Cluster")

# Boxplot

ggplot(hast, aes(x=Cluster, y=aspect_ratio))+
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



