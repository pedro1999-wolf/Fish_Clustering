### Tratando a planilha ###

janei<-readxl::read_xlsx("jan.xlsx")

colnames(janei)[colnames(janei) %in% c("FALSE...1", "abertura da nadadeira", "FALSE...3", "aspect ratio", "FALSE...5")] <- c("sp", "abert_nad", "area_nad", "aspect_ratio", "length")

janei<- na.exclude(janei)

janei$classe<-factor(janei$classe)

janei$relcautam <- as.numeric(gsub(",", ".", janei$relcautam))
janei$area_nad<- as.numeric(gsub(",", ".", janei$area_nad))
janei$aspect_ratio<- as.numeric(gsub(",", ".", janei$aspect_ratio))
janei$length<- as.numeric(gsub(",", ".", janei$length))
janei$abert_nad<- as.numeric(gsub(",", ".", janei$abert_nad))


### Análises exploratórias ###

plot(density(janei$aspect_ratio))# Não me parece precisar de normalização

ggplot(janei, aes(x=classe,y=resid))+
  geom_boxplot()+
  theme_classic() 

### Tirando o efeito do tamanho sobre aspect_ratio ###

# Passo 1: Ajustar uma regressão linear

modelo <- lm(aspect_ratio ~ length, data = janei)

summary(modelo)

ggplot(janei, aes(x=length,y=resid))+ 
  geom_jitter(width=0.5, alpha=1, color="azure4")+
  geom_smooth(method="lm",se=F,linewidth=1.2,color="black")+
  xlab("Comprimento padrão (cm)")+ ylab("Aspect ratio da nadadeira caudal")+
  theme(legend.title = element_blank())+theme_classic()+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0),size = 25), 
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0),size = 25), 
        panel.grid.major = element_line(colour="white"), 
        panel.grid.minor = element_line(colour="white"), 
        axis.text.y = element_text(size = 25, colour = "black", margin = margin(t = 20, r = 0, b = 0, l = 0)), 
        axis.text.x = element_text(size = 25, colour = "black", margin = margin(t = 20, r = 0, b = 0, l = 0)), 
        strip.text.y = element_text(size = 15),
        legend.title = element_blank(),
        legend.text = element_text(size=15))+
  scale_x_continuous(breaks = c(1,3,5,7,9,11,13,15))

# Passo 2: Obter os resíduos (que representam o aspect ratio ajustado, sem o efeito de tamanho)
resid <- residuals(modelo)

# Passo 3: Adicionar os resíduos ao dataframe (opcional, mas útil para manter o dataframe organizado)
janei$resid <- resid

# Definindo K ótimo

janeinum<- subset(janei, select = c(resid))

# NbClust - aplica 30 índices diferentes e compara seus resultados

result2 <- NbClust(janeinum, min.nc = 2, max.nc = 10, method = "ward.D2")
print(result$Best.nc)
warnings()

# Calcular a matriz de distâncias (aqui, estamos usando a distância euclidiana)
dist_matrix <- dist(janeinum)

# Realizar o clustering hierárquico (método de ligação completa, mas você pode escolher outros métodos como "single", "average", etc.)
hc <- hclust(dist_matrix, method = "ward.D2")

# Plotar o dendrograma com os clusters
plot(hc, main = paste("Dendrograma com", 4, "Clusters"),
     xlab = "Observações", ylab = "Distância Euclidiana")

# Adicionar linhas verticais para mostrar os clusters
rect.hclust(hc, k = 4, border = "red")

# Cortar o dendrograma em clusters
clusters <- cutree(hc, k = 4)

# Adicionar o cluster resultante aos dados filtrados para análise adicional
janei$Cluster <- clusters

janei$Cluster<-factor(janei$Cluster)

anova<-aov(resid~Cluster, data = janei)

tukey_result <- TukeyHSD(anova)

print(tukey_result)

### set up a one-way ANOVA
anova<-aov(resid~Cluster, data = janei)
### specify all pair-wise comparisons among levels of variable "tension"
tuk <- glht(anova, linfct = mcp(Cluster = "Tukey"))
### p-values
pvalues <- adjusted()(tuk)$pvalues
### get confidence intervals
ci.glht <- confint(tuk)
### plot them
plot(ci.glht, col = ifelse(pvalues < 0.05, "red", "black"), 
     xlab = "Difference in mean levels")

# Média de resid por cluster

media_aspect_por_cluster <- aggregate(aspect_ratio ~ Cluster, data = janei, FUN = mean)

print(media_aspect_por_cluster)

# Plot pizza porcentagem cluster

df_clusters <- as.data.frame(table(janei$Cluster))
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

ggplot(janei, aes(x=Cluster, y=aspect_ratio))+
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

