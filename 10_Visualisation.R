########################## 1) Visualise SOS and EOS per tree
# SOS
ndvi.sos <- ggplot(pheno, aes(x=reorder(SpecieName,NDVI.SOS), y=NDVI.SOS, col=SpecieName))+
  geom_boxplot()+
  #geom_jitter(width = 0.05, col='grey', fill='grey', alpha=0.2)+
  ylim(100,350)+
  labs(title='Start of Season', subtitle =  'NDVI', y='DOY')+
  theme(legend.position = 'none', axis.title.x=element_blank(),
        plot.title = element_text(color = "black", size = 12, face = "bold"),
        plot.subtitle = element_text(color = "black", size=10, face='italic'))+
  stat_summary(fun.y=mean, geom="point", shape=20, size=2, color="black", fill="black", alpha=0.3)
evi.sos <- ggplot(pheno, aes(x=reorder(SpecieName,EVI.SOS, na.rm=T), y=EVI.SOS, col=SpecieName))+
  geom_boxplot()+
  #geom_jitter(width = 0.05, col='grey', fill='grey', alpha=0.2)+
  ylim(100,350)+
  labs(title = ' ', subtitle =  'EVI2', y='DOY')+
  theme(legend.position = 'none', axis.title.x=element_blank(),
        plot.subtitle = element_text(color = "black", size=10, face='italic'))+
  stat_summary(fun.y=mean, geom="point", shape=20, size=2, color="black", fill="black", alpha=0.3)
osavi.sos <- ggplot(pheno, aes(x=reorder(SpecieName,MCOSAVI.SOS, na.rm=T), y=MCOSAVI.SOS, col=SpecieName))+
  geom_boxplot()+
  #geom_jitter(width = 0.05, col='grey', fill='grey', alpha=0.2)+
  ylim(100,350)+
  labs(title = ' ', subtitle =  'OSAVI', y='DOY')+
  theme(legend.position = 'none', axis.title.x=element_blank(),
        plot.subtitle = element_text(color = "black", size=10, face='italic'))+
  stat_summary(fun.y=mean, geom="point", shape=20, size=2, color="black", fill="black", alpha=0.3)
cire.sos <- ggplot(pheno, aes(x=reorder(SpecieName,CIRE.SOS, na.rm=T), y=CIRE.SOS, col=SpecieName))+
  geom_boxplot()+
  #geom_jitter(width = 0.05, col='grey', fill='grey', alpha=0.2)+
  ylim(100,350)+
  labs(title = ' ', subtitle =  'CIRE', y='DOY', fill='Test')+
  theme(axis.title.x=element_blank(),
        plot.subtitle = element_text(color = "black", size=10, face='italic'),
        legend.title = element_blank(),
        legend.position = c(-1,0.92),
        legend.text = element_text(size = 8))+
  guides(color = guide_legend(nrow = 1, byrow = TRUE))+
  stat_summary(fun.y=mean, geom="point", shape=20, size=2, color="black", fill="black", alpha=0.3)

# EOS
ndvi.eos <- ggplot(pheno, aes(x=reorder(SpecieName,NDVI.EOS, na.rm=T), y=NDVI.EOS, col=SpecieName))+
  geom_boxplot()+
  #geom_jitter(width = 0.05, col='grey', fill='grey', alpha=0.2)+
  ylim(100,400)+
  labs(title='End of Season', subtitle = 'NDVI',  y='DOY')+
  theme(legend.position = 'none', axis.title.x=element_blank(),
        plot.title = element_text(color = "black", size = 12, face = "bold"),
        plot.subtitle = element_text(color = "black", size=10, face='italic'))+
  stat_summary(fun.y=mean, geom="point", shape=20, size=2, color="black", fill="black", alpha=0.3)
evi.eos <- ggplot(pheno, aes(x=reorder(SpecieName,EVI.EOS, na.rm=T), y=EVI.EOS, col=SpecieName))+
  geom_boxplot()+
  #geom_jitter(width = 0.05, col='grey', fill='grey', alpha=0.2)+
  ylim(100,400)+
  labs(title = ' ', subtitle = 'EVI2', y='DOY')+
  theme(legend.position = 'none', axis.title.x=element_blank(),
        plot.subtitle = element_text(color = "black", size=10, face='italic'))+
  stat_summary(fun.y=mean, geom="point", shape=20, size=2, color="black", fill="black", alpha=0.3)
osavi.eos <- ggplot(pheno, aes(x=reorder(SpecieName,MCOSAVI.EOS, na.rm=T), y=MCOSAVI.EOS, col=SpecieName))+
  geom_boxplot()+
  #geom_jitter(width = 0.05, col='grey', fill='grey', alpha=0.2)+
  ylim(100,400)+
  labs(title = ' ', subtitle = 'OSAVI', y='DOY')+
  theme(legend.position = 'none', axis.title.x=element_blank(),
        plot.subtitle = element_text(color = "black", size=10, face='italic'))+
  stat_summary(fun.y=mean, geom="point", shape=20, size=2, color="black", fill="black", alpha=0.3)
cire.eos <- ggplot(pheno, aes(x=reorder(SpecieName,CIRE.EOS, na.rm=T), y=CIRE.EOS, col=SpecieName))+
  geom_boxplot()+
  #geom_jitter(width = 0.05, col='grey', fill='grey', alpha=0.2)+
  ylim(100,400)+
  labs(title = ' ', subtitle = 'CIRE', y='DOY')+
  theme(legend.position = 'none', axis.title.x=element_blank(),
        plot.subtitle = element_text(color = "black", size=10, face='italic'))+
  stat_summary(fun.y=mean, geom="point", shape=20, size=2, color="black", fill="black", alpha=0.3)


# Combine the boxplots
tree_sos_eos_plot <- cowplot::plot_grid(ndvi.sos + theme(axis.text.x = element_blank()),
                   evi.sos + 
                     theme(axis.title.y = element_blank(),
                           axis.text.y = element_blank(),
                           axis.text.x = element_blank()),
                   osavi.sos + 
                     theme(axis.title.y = element_blank(),
                           axis.text.y = element_blank(),
                           axis.text.x = element_blank()),
                   cire.sos + 
                     theme(axis.title.y = element_blank(),
                           axis.text.y = element_blank(),
                           axis.text.x = element_blank()),
                   ndvi.eos + theme(axis.text.x = element_blank()),
                   evi.eos + 
                     theme(axis.title.y = element_blank(),
                           axis.text.y = element_blank(),
                           axis.text.x = element_blank()),
                   osavi.eos + 
                     theme(axis.title.y = element_blank(),
                           axis.text.y = element_blank(),
                           axis.text.x = element_blank()),
                   cire.eos + 
                     theme(axis.title.y = element_blank(),
                           axis.text.y = element_blank(),
                           axis.text.x = element_blank()),
                   nrow = 2,
                   ncol=4,
                   allign='v')

rm(ndvi.sos,ndvi.eos,ndvi.gsl,
   evi.sos,evi.eos,evi.gsl,
   osavi.sos,osavi.eos,osavi.gsl,
   cire.sos,cire.eos,cire.gsl)


########################## 2) per-species and per-VI double logistic
# NDVI
ndvi.1 <- ggplot() +
  geom_line(aes(x=sum.ts.ndvi[[1]]$DOY, y=sum.ts.ndvi[[1]]$Predicted), size=1, col='red')+
  geom_ribbon(aes(x=sum.ts.ndvi[[1]]$DOY, ymin=(sum.ts.ndvi[[1]]$Predicted-sum.ts.ndvi[[1]]$SD), 
                  ymax=(sum.ts.ndvi[[1]]$Predicted+sum.ts.ndvi[[1]]$SD)), alpha=0.2)+
  geom_point(aes(x=sum.ts.ndvi[[1]]$DOY, y=sum.ts.ndvi[[1]]$Mean), col='black', size=1)+
  labs(x='DOY', y='NDVI', subtitle=paste0(tree.name[1], ' (n=', obs$Observations[1], ')'))+
  ylim(0.3,1)+
  scale_x_continuous(breaks = round(seq(50, 365, by = 25),50))+
  annotate("text", x = 300, y = 0.95, label = paste0('nRMSE = ', 
                                                     round((mean(sum.ts.ndvi[[1]]$RtSqrtError))/(mean(sum.ts.ndvi[[1]]$Mean)),3)),size=3)
ndvi.2 <- ggplot() +
  geom_line(aes(x=sum.ts.ndvi[[2]]$DOY, y=sum.ts.ndvi[[2]]$Predicted), size=1, col='red')+
  geom_ribbon(aes(x=sum.ts.ndvi[[2]]$DOY, ymin=(sum.ts.ndvi[[2]]$Predicted-sum.ts.ndvi[[2]]$SD), 
                  ymax=(sum.ts.ndvi[[2]]$Predicted+sum.ts.ndvi[[2]]$SD)), alpha=0.2)+
  geom_point(aes(x=sum.ts.ndvi[[2]]$DOY, y=sum.ts.ndvi[[2]]$Mean), col='black', size=1)+
  labs(x='DOY', y='NDVI', subtitle=paste0(tree.name[2], ' (n=', obs$Observations[2], ')'))+
  ylim(0.3,1)+
  scale_x_continuous(breaks = round(seq(50, 365, by = 25),50))+
  annotate("text", x = 300, y = 0.95, label = paste0('nRMSE = ', 
                                                     round((mean(sum.ts.ndvi[[2]]$RtSqrtError))/(mean(sum.ts.ndvi[[2]]$Mean)),3)),size=3)
ndvi.3 <- ggplot() +
  geom_line(aes(x=sum.ts.ndvi[[3]]$DOY, y=sum.ts.ndvi[[3]]$Predicted), size=1, col='red')+
  geom_ribbon(aes(x=sum.ts.ndvi[[3]]$DOY, ymin=(sum.ts.ndvi[[3]]$Predicted-sum.ts.ndvi[[3]]$SD), 
                  ymax=(sum.ts.ndvi[[3]]$Predicted+sum.ts.ndvi[[3]]$SD)), alpha=0.2)+
  geom_point(aes(x=sum.ts.ndvi[[3]]$DOY, y=sum.ts.ndvi[[3]]$Mean), col='black', size=1)+
  labs(x='DOY', y='NDVI', subtitle=paste0(tree.name[3], ' (n=', obs$Observations[3], ')'))+
  ylim(0.3,1)+
  scale_x_continuous(breaks = round(seq(50, 365, by = 25),50))+
  annotate("text", x = 300, y = 0.95, label = paste0('nRMSE = ', 
                                                     round((mean(sum.ts.ndvi[[3]]$RtSqrtError))/(mean(sum.ts.ndvi[[3]]$Mean)),3)),size=3)
ndvi.4 <- ggplot() +
  geom_line(aes(x=sum.ts.ndvi[[4]]$DOY, y=sum.ts.ndvi[[4]]$Predicted), size=1, col='red')+
  geom_ribbon(aes(x=sum.ts.ndvi[[4]]$DOY, ymin=(sum.ts.ndvi[[4]]$Predicted-sum.ts.ndvi[[4]]$SD), 
                  ymax=(sum.ts.ndvi[[4]]$Predicted+sum.ts.ndvi[[4]]$SD)), alpha=0.2)+
  geom_point(aes(x=sum.ts.ndvi[[4]]$DOY, y=sum.ts.ndvi[[4]]$Mean), col='black', size=1)+
  labs(x='DOY', y='NDVI', title='NDVI', subtitle=paste0(tree.name[4], ' (n=', obs$Observations[4], ')'))+
  ylim(0.3,1)+
  scale_x_continuous(breaks = round(seq(50, 365, by = 25),50))+
  annotate("text", x = 300, y = 0.95, label = paste0('nRMSE = ', 
                                                     round((mean(sum.ts.ndvi[[4]]$RtSqrtError))/(mean(sum.ts.ndvi[[4]]$Mean)),3)),size=3)
ndvi.5 <- ggplot() +
  geom_line(aes(x=sum.ts.ndvi[[5]]$DOY, y=sum.ts.ndvi[[5]]$Predicted), size=1, col='red')+
  geom_ribbon(aes(x=sum.ts.ndvi[[5]]$DOY, ymin=(sum.ts.ndvi[[5]]$Predicted-sum.ts.ndvi[[5]]$SD), 
                  ymax=(sum.ts.ndvi[[5]]$Predicted+sum.ts.ndvi[[5]]$SD)), alpha=0.2)+
  geom_point(aes(x=sum.ts.ndvi[[5]]$DOY, y=sum.ts.ndvi[[5]]$Mean), col='black', size=1)+
  labs(x='DOY', y='NDVI', subtitle=paste0(tree.name[5], ' (n=', obs$Observations[5], ')'))+
  ylim(0.3,1)+
  scale_x_continuous(breaks = round(seq(50, 365, by = 25),50))+
  annotate("text", x = 300, y = 0.95, label = paste0('nRMSE = ', 
                                                     round((mean(sum.ts.ndvi[[5]]$RtSqrtError))/(mean(sum.ts.ndvi[[5]]$Mean)),3)),size=3)
ndvi.6 <- ggplot() +
  geom_line(aes(x=sum.ts.ndvi[[6]]$DOY, y=sum.ts.ndvi[[6]]$Predicted), size=1, col='red')+
  geom_ribbon(aes(x=sum.ts.ndvi[[6]]$DOY, ymin=(sum.ts.ndvi[[6]]$Predicted-sum.ts.ndvi[[6]]$SD), 
                  ymax=(sum.ts.ndvi[[6]]$Predicted+sum.ts.ndvi[[6]]$SD)), alpha=0.2)+
  geom_point(aes(x=sum.ts.ndvi[[6]]$DOY, y=sum.ts.ndvi[[6]]$Mean), col='black', size=1)+
  labs(x='DOY', y='NDVI', subtitle=paste0(tree.name[6], ' (n=', obs$Observations[6], ')'))+
  ylim(0.3,1)+
  scale_x_continuous(breaks = round(seq(50, 365, by = 25),50))+
  annotate("text", x = 300, y = 0.95, label = paste0('nRMSE = ', 
                                                     round((mean(sum.ts.ndvi[[6]]$RtSqrtError))/(mean(sum.ts.ndvi[[6]]$Mean)),3)),size=3)
ndvi.9 <- ggplot() +
  geom_line(aes(x=sum.ts.ndvi[[7]]$DOY, y=sum.ts.ndvi[[7]]$Predicted), size=1, col='red')+
  geom_ribbon(aes(x=sum.ts.ndvi[[7]]$DOY, ymin=(sum.ts.ndvi[[7]]$Predicted-sum.ts.ndvi[[7]]$SD), 
                  ymax=(sum.ts.ndvi[[7]]$Predicted+sum.ts.ndvi[[7]]$SD)), alpha=0.2)+
  geom_point(aes(x=sum.ts.ndvi[[7]]$DOY, y=sum.ts.ndvi[[7]]$Mean), col='black', size=1)+
  labs(x='DOY', y='NDVI', subtitle=paste0(tree.name[7], ' (n=', obs$Observations[7], ')'))+
  ylim(0.3,1)+
  scale_x_continuous(breaks = round(seq(50, 365, by = 25),50))+
  annotate("text", x = 300, y = 0.95, label = paste0('nRMSE = ', 
                                                     round((mean(sum.ts.ndvi[[7]]$RtSqrtError))/(mean(sum.ts.ndvi[[7]]$Mean)),3)),size=3)

# EVI
evi.1 <- ggplot() +
  geom_line(aes(x=sum.ts.evi[[1]]$DOY, y=sum.ts.evi[[1]]$Predicted), size=1, col='red')+
  geom_ribbon(aes(x=sum.ts.evi[[1]]$DOY, ymin=(sum.ts.evi[[1]]$Predicted-sum.ts.evi[[1]]$SD), 
                  ymax=(sum.ts.evi[[1]]$Predicted+sum.ts.evi[[1]]$SD)), alpha=0.2)+
  geom_point(aes(x=sum.ts.evi[[1]]$DOY, y=sum.ts.evi[[1]]$Mean), col='black', size=1)+
  labs(x='DOY', y='EVI2')+
  ylim(0,0.8)+
  scale_x_continuous(breaks = round(seq(50, 365, by = 25),50))+
  annotate("text", x = 300, y = 0.8, label = paste0('nRMSE = ', 
                                                    round((mean(sum.ts.evi[[1]]$RtSqrtError))/(mean(sum.ts.evi[[1]]$Mean)),3)),size=3)
evi.2 <- ggplot() +
  geom_line(aes(x=sum.ts.evi[[2]]$DOY, y=sum.ts.evi[[2]]$Predicted), size=1, col='red')+
  geom_ribbon(aes(x=sum.ts.evi[[2]]$DOY, ymin=(sum.ts.evi[[2]]$Predicted-sum.ts.evi[[2]]$SD), 
                  ymax=(sum.ts.evi[[2]]$Predicted+sum.ts.evi[[2]]$SD)), alpha=0.2)+
  geom_point(aes(x=sum.ts.evi[[2]]$DOY, y=sum.ts.evi[[2]]$Mean), col='black', size=1)+
  labs(x='DOY', y='EVI2', title=tree.name[2])+
  ylim(0,0.8)+
  scale_x_continuous(breaks = round(seq(50, 365, by = 25),50))+
  annotate("text", x = 300, y = 0.8, label = paste0('nRMSE = ', 
                                                    round((mean(sum.ts.evi[[2]]$RtSqrtError))/(mean(sum.ts.evi[[2]]$Mean)),3)),size=3)
evi.3 <- ggplot() +
  geom_line(aes(x=sum.ts.evi[[3]]$DOY, y=sum.ts.evi[[3]]$Predicted), size=1, col='red')+
  geom_ribbon(aes(x=sum.ts.evi[[3]]$DOY, ymin=(sum.ts.evi[[3]]$Predicted-sum.ts.evi[[3]]$SD), 
                  ymax=(sum.ts.evi[[3]]$Predicted+sum.ts.evi[[3]]$SD)), alpha=0.2)+
  geom_point(aes(x=sum.ts.evi[[3]]$DOY, y=sum.ts.evi[[3]]$Mean), col='black', size=1)+
  labs(x='DOY', y='EVI2', title=tree.name[3])+
  ylim(0,0.8)+
  scale_x_continuous(breaks = round(seq(50, 365, by = 25),50))+
  annotate("text", x = 300, y = 0.8, label = paste0('nRMSE = ', 
                                                    round((mean(sum.ts.evi[[3]]$RtSqrtError))/(mean(sum.ts.evi[[3]]$Mean)),3)),size=3)
evi.4 <- ggplot() +
  geom_line(aes(x=sum.ts.evi[[4]]$DOY, y=sum.ts.evi[[4]]$Predicted), size=1, col='red')+
  geom_ribbon(aes(x=sum.ts.evi[[4]]$DOY, ymin=(sum.ts.evi[[4]]$Predicted-sum.ts.evi[[4]]$SD), 
                  ymax=(sum.ts.evi[[4]]$Predicted+sum.ts.evi[[4]]$SD)), alpha=0.2)+
  geom_point(aes(x=sum.ts.evi[[4]]$DOY, y=sum.ts.evi[[4]]$Mean), col='black', size=1)+
  labs(x='DOY', y='EVI2', title='EVI2')+
  ylim(0,0.8)+
  scale_x_continuous(breaks = round(seq(50, 365, by = 25),50))+
  annotate("text", x = 300, y = 0.8, label = paste0('nRMSE = ', 
                                                    round((mean(sum.ts.evi[[4]]$RtSqrtError))/(mean(sum.ts.evi[[4]]$Mean)),3)),size=3)
evi.5 <- ggplot() +
  geom_line(aes(x=sum.ts.evi[[5]]$DOY, y=sum.ts.evi[[5]]$Predicted), size=1, col='red')+
  geom_ribbon(aes(x=sum.ts.evi[[5]]$DOY, ymin=(sum.ts.evi[[5]]$Predicted-sum.ts.evi[[5]]$SD), 
                  ymax=(sum.ts.evi[[5]]$Predicted+sum.ts.evi[[5]]$SD)), alpha=0.2)+
  geom_point(aes(x=sum.ts.evi[[5]]$DOY, y=sum.ts.evi[[5]]$Mean), col='black', size=1)+
  labs(x='DOY', y='EVI2', title=tree.name[5])+
  ylim(0,0.8)+
  scale_x_continuous(breaks = round(seq(50, 365, by = 25),50))+
  annotate("text", x = 300, y = 0.8, label = paste0('nRMSE = ', 
                                                    round((mean(sum.ts.evi[[5]]$RtSqrtError))/(mean(sum.ts.evi[[5]]$Mean)),3)),size=3)
evi.6 <- ggplot() +
  geom_line(aes(x=sum.ts.evi[[6]]$DOY, y=sum.ts.evi[[6]]$Predicted), size=1, col='red')+
  geom_ribbon(aes(x=sum.ts.evi[[6]]$DOY, ymin=(sum.ts.evi[[6]]$Predicted-sum.ts.evi[[6]]$SD), 
                  ymax=(sum.ts.evi[[6]]$Predicted+sum.ts.evi[[6]]$SD)), alpha=0.2)+
  geom_point(aes(x=sum.ts.evi[[6]]$DOY, y=sum.ts.evi[[6]]$Mean), col='black', size=1)+
  labs(x='DOY', y='EVI2', title=tree.name[6])+
  ylim(0,0.8)+
  scale_x_continuous(breaks = round(seq(50, 365, by = 25),50))+
  annotate("text", x = 300, y = 0.8, label = paste0('nRMSE = ', 
                                                    round((mean(sum.ts.evi[[6]]$RtSqrtError))/(mean(sum.ts.evi[[6]]$Mean)),3)),size=3)
evi.9 <- ggplot() +
  geom_line(aes(x=sum.ts.evi[[7]]$DOY, y=sum.ts.evi[[7]]$Predicted), size=1, col='red')+
  geom_ribbon(aes(x=sum.ts.evi[[7]]$DOY, ymin=(sum.ts.evi[[7]]$Predicted-sum.ts.evi[[7]]$SD), 
                  ymax=(sum.ts.evi[[7]]$Predicted+sum.ts.evi[[7]]$SD)), alpha=0.2)+
  geom_point(aes(x=sum.ts.evi[[7]]$DOY, y=sum.ts.evi[[7]]$Mean), col='black', size=1)+
  labs(x='DOY', y='EVI2', title=tree.name[7])+
  ylim(0,0.8)+
  scale_x_continuous(breaks = round(seq(50, 365, by = 25),50))+
  annotate("text", x = 300, y = 0.75, label = paste0('nRMSE = ', 
                                                     round((mean(sum.ts.evi[[7]]$RtSqrtError))/(mean(sum.ts.evi[[7]]$Mean)),3)),size=3)
# MCARI/OSAVI
osavi.1 <- ggplot() +
  geom_line(aes(x=sum.ts.osavi[[1]]$DOY, y=sum.ts.osavi[[1]]$Predicted), size=1, col='red')+
  geom_ribbon(aes(x=sum.ts.osavi[[1]]$DOY, ymin=(sum.ts.osavi[[1]]$Predicted-sum.ts.osavi[[1]]$SD), 
                  ymax=(sum.ts.osavi[[1]]$Predicted+sum.ts.osavi[[1]]$SD)), alpha=0.2)+
  geom_point(aes(x=sum.ts.osavi[[1]]$DOY, y=sum.ts.osavi[[1]]$Mean), col='black', size=1)+
  labs(x='DOY', y='OSAVI')+
  ylim(0,1)+
  scale_x_continuous(breaks = round(seq(50, 365, by = 25),50))+
  annotate("text", x = 300, y = 1, label = paste0('nRMSE = ', 
                                                  round((mean(sum.ts.osavi[[1]]$RtSqrtError))/(mean(sum.ts.osavi[[1]]$Mean)),3)),size=3)
osavi.2 <- ggplot() +
  geom_line(aes(x=sum.ts.osavi[[2]]$DOY, y=sum.ts.osavi[[2]]$Predicted), size=1, col='red')+
  geom_ribbon(aes(x=sum.ts.osavi[[2]]$DOY, ymin=(sum.ts.osavi[[2]]$Predicted-sum.ts.osavi[[2]]$SD), 
                  ymax=(sum.ts.osavi[[2]]$Predicted+sum.ts.osavi[[2]]$SD)), alpha=0.2)+
  geom_point(aes(x=sum.ts.osavi[[2]]$DOY, y=sum.ts.osavi[[2]]$Mean), col='black', size=1)+
  labs(x='DOY', y='OSAVI', title=tree.name[2])+
  ylim(0,1)+
  scale_x_continuous(breaks = round(seq(50, 365, by = 25),50))+
  annotate("text", x = 300, y = 1, label = paste0('nRMSE = ', 
                                                  round((mean(sum.ts.osavi[[2]]$RtSqrtError))/(mean(sum.ts.osavi[[2]]$Mean)),3)),size=3)
osavi.3 <- ggplot() +
  geom_line(aes(x=sum.ts.osavi[[3]]$DOY, y=sum.ts.osavi[[3]]$Predicted), size=1, col='red')+
  geom_ribbon(aes(x=sum.ts.osavi[[3]]$DOY, ymin=(sum.ts.osavi[[3]]$Predicted-sum.ts.osavi[[3]]$SD), 
                  ymax=(sum.ts.osavi[[3]]$Predicted+sum.ts.osavi[[3]]$SD)), alpha=0.2)+
  geom_point(aes(x=sum.ts.osavi[[3]]$DOY, y=sum.ts.osavi[[3]]$Mean), col='black', size=1)+
  labs(x='DOY', y='OSAVI', title=tree.name[3])+
  ylim(0,1)+
  scale_x_continuous(breaks = round(seq(50, 365, by = 25),50))+
  annotate("text", x = 300, y = 1, label = paste0('nRMSE = ', 
                                                  round((mean(sum.ts.osavi[[3]]$RtSqrtError))/(mean(sum.ts.osavi[[3]]$Mean)),3)),size=3)
osavi.4 <- ggplot() +
  geom_line(aes(x=sum.ts.osavi[[4]]$DOY, y=sum.ts.osavi[[4]]$Predicted), size=1, col='red')+
  geom_ribbon(aes(x=sum.ts.osavi[[4]]$DOY, ymin=(sum.ts.osavi[[4]]$Predicted-sum.ts.osavi[[4]]$SD), 
                  ymax=(sum.ts.osavi[[4]]$Predicted+sum.ts.osavi[[4]]$SD)), alpha=0.2)+
  geom_point(aes(x=sum.ts.osavi[[4]]$DOY, y=sum.ts.osavi[[4]]$Mean), col='black', size=1)+
  labs(x='DOY', y='OSAVI', title='OSAVI')+
  ylim(0,1)+
  scale_x_continuous(breaks = round(seq(50, 365, by = 25),50))+
  annotate("text", x = 300, y = 1, label = paste0('nRMSE = ', 
                                                  round((mean(sum.ts.osavi[[4]]$RtSqrtError))/(mean(sum.ts.osavi[[4]]$Mean)),3)),size=3)
osavi.5 <- ggplot() +
  geom_line(aes(x=sum.ts.osavi[[5]]$DOY, y=sum.ts.osavi[[5]]$Predicted), size=1, col='red')+
  geom_ribbon(aes(x=sum.ts.osavi[[5]]$DOY, ymin=(sum.ts.osavi[[5]]$Predicted-sum.ts.osavi[[5]]$SD), 
                  ymax=(sum.ts.osavi[[5]]$Predicted+sum.ts.osavi[[5]]$SD)), alpha=0.2)+
  geom_point(aes(x=sum.ts.osavi[[5]]$DOY, y=sum.ts.osavi[[5]]$Mean), col='black', size=1)+
  labs(x='DOY', y='OSAVI', title=tree.name[5])+
  ylim(0,1)+
  scale_x_continuous(breaks = round(seq(50, 365, by = 25),50))+
  annotate("text", x = 300, y = 1, label = paste0('nRMSE = ', 
                                                  round((mean(sum.ts.osavi[[5]]$RtSqrtError))/(mean(sum.ts.osavi[[5]]$Mean)),3)),size=3)
osavi.6 <- ggplot() +
  geom_line(aes(x=sum.ts.osavi[[6]]$DOY, y=sum.ts.osavi[[6]]$Predicted), size=1, col='red')+
  geom_ribbon(aes(x=sum.ts.osavi[[6]]$DOY, ymin=(sum.ts.osavi[[6]]$Predicted-sum.ts.osavi[[6]]$SD), 
                  ymax=(sum.ts.osavi[[6]]$Predicted+sum.ts.osavi[[6]]$SD)), alpha=0.2)+
  geom_point(aes(x=sum.ts.osavi[[6]]$DOY, y=sum.ts.osavi[[6]]$Mean), col='black', size=1)+
  labs(x='DOY', y='OSAVI', title=tree.name[6])+
  ylim(0,1)+
  scale_x_continuous(breaks = round(seq(50, 365, by = 25),50))+
  annotate("text", x = 300, y = 1, label = paste0('nRMSE = ', 
                                                  round((mean(sum.ts.osavi[[6]]$RtSqrtError))/(mean(sum.ts.osavi[[6]]$Mean)),3)),size=3)
osavi.9 <- ggplot() +
  geom_line(aes(x=sum.ts.osavi[[7]]$DOY, y=sum.ts.osavi[[7]]$Predicted), size=1, col='red')+
  geom_ribbon(aes(x=sum.ts.osavi[[7]]$DOY, ymin=(sum.ts.osavi[[7]]$Predicted-sum.ts.osavi[[7]]$SD), 
                  ymax=(sum.ts.osavi[[7]]$Predicted+sum.ts.osavi[[7]]$SD)), alpha=0.2)+
  geom_point(aes(x=sum.ts.osavi[[7]]$DOY, y=sum.ts.osavi[[7]]$Mean), col='black', size=1)+
  labs(x='DOY', y='OSAVI', title=tree.name[7])+
  ylim(0,1)+
  scale_x_continuous(breaks = round(seq(50, 365, by = 25),50))+
  annotate("text", x = 300, y = 0.95, label = paste0('nRMSE = ', 
                                                     round((mean(sum.ts.osavi[[7]]$RtSqrtError))/(mean(sum.ts.osavi[[7]]$Mean)),3)),size=3)

# Chlorphyl Index Red Edge
cire.1 <- ggplot() +
  geom_line(aes(x=sum.ts.cire[[1]]$DOY, y=sum.ts.cire[[1]]$Predicted), size=1, col='red')+
  geom_ribbon(aes(x=sum.ts.cire[[1]]$DOY, ymin=(sum.ts.cire[[1]]$Predicted-sum.ts.cire[[1]]$SD), 
                  ymax=(sum.ts.cire[[1]]$Predicted+sum.ts.cire[[1]]$SD)), alpha=0.2)+
  geom_point(aes(x=sum.ts.cire[[1]]$DOY, y=sum.ts.cire[[1]]$Mean), col='black', size=1)+
  labs(x='DOY', y='CIRE')+
  ylim(0,2.4)+
  scale_x_continuous(breaks = round(seq(50, 365, by = 25),50))+
  annotate("text", x = 300, y = 2.4, label = paste0('nRMSE = ', 
                                                    round((mean(sum.ts.cire[[1]]$RtSqrtError))/(mean(sum.ts.cire[[1]]$Mean)),3)),size=3)
cire.2 <- ggplot() +
  geom_line(aes(x=sum.ts.cire[[2]]$DOY, y=sum.ts.cire[[2]]$Predicted), size=1, col='red')+
  geom_ribbon(aes(x=sum.ts.cire[[2]]$DOY, ymin=(sum.ts.cire[[2]]$Predicted-sum.ts.cire[[2]]$SD), 
                  ymax=(sum.ts.cire[[2]]$Predicted+sum.ts.cire[[2]]$SD)), alpha=0.2)+
  geom_point(aes(x=sum.ts.cire[[2]]$DOY, y=sum.ts.cire[[2]]$Mean), col='black', size=1)+
  labs(x='DOY', y='CIRE', title=tree.name[2])+
  ylim(0,2.4)+
  scale_x_continuous(breaks = round(seq(50, 365, by = 25),50))+
  annotate("text", x = 300, y = 2.4, label = paste0('nRMSE = ', 
                                                    round((mean(sum.ts.cire[[2]]$RtSqrtError))/(mean(sum.ts.cire[[2]]$Mean)),3)),size=3)
cire.3 <- ggplot() +
  geom_line(aes(x=sum.ts.cire[[3]]$DOY, y=sum.ts.cire[[3]]$Predicted), size=1, col='red')+
  geom_ribbon(aes(x=sum.ts.cire[[3]]$DOY, ymin=(sum.ts.cire[[3]]$Predicted-sum.ts.cire[[3]]$SD), 
                  ymax=(sum.ts.cire[[3]]$Predicted+sum.ts.cire[[3]]$SD)), alpha=0.2)+
  geom_point(aes(x=sum.ts.cire[[3]]$DOY, y=sum.ts.cire[[3]]$Mean), col='black', size=1)+
  labs(x='DOY', y='CIRE', title=tree.name[3])+
  ylim(0,2.4)+
  scale_x_continuous(breaks = round(seq(50, 365, by = 25),50))+
  annotate("text", x = 300, y = 2.4, label = paste0('nRMSE = ', 
                                                    round((mean(sum.ts.cire[[3]]$RtSqrtError))/(mean(sum.ts.cire[[3]]$Mean)),3)),size=3)
cire.4 <- ggplot() +
  geom_line(aes(x=sum.ts.cire[[4]]$DOY, y=sum.ts.cire[[4]]$Predicted), size=1, col='red')+
  geom_ribbon(aes(x=sum.ts.cire[[4]]$DOY, ymin=(sum.ts.cire[[4]]$Predicted-sum.ts.cire[[4]]$SD), 
                  ymax=(sum.ts.cire[[4]]$Predicted+sum.ts.cire[[4]]$SD)), alpha=0.2)+
  geom_point(aes(x=sum.ts.cire[[4]]$DOY, y=sum.ts.cire[[4]]$Mean), col='black', size=1)+
  labs(x='DOY', y='CIRE', title='CIRE')+
  ylim(0,2.4)+
  scale_x_continuous(breaks = round(seq(50, 365, by = 25),50))+
  annotate("text", x = 300, y = 2.4, label = paste0('nRMSE = ', 
                                                    round((mean(sum.ts.cire[[4]]$RtSqrtError))/(mean(sum.ts.cire[[4]]$Mean)),3)),size=3)
cire.5 <- ggplot() +
  geom_line(aes(x=sum.ts.cire[[5]]$DOY, y=sum.ts.cire[[5]]$Predicted), size=1, col='red')+
  geom_ribbon(aes(x=sum.ts.cire[[5]]$DOY, ymin=(sum.ts.cire[[5]]$Predicted-sum.ts.cire[[5]]$SD), 
                  ymax=(sum.ts.cire[[5]]$Predicted+sum.ts.cire[[5]]$SD)), alpha=0.2)+
  geom_point(aes(x=sum.ts.cire[[5]]$DOY, y=sum.ts.cire[[5]]$Mean), col='black', size=1)+
  labs(x='DOY', y='CIRE', title=tree.name[5])+
  ylim(0,2.4)+
  scale_x_continuous(breaks = round(seq(50, 365, by = 25),50))+
  annotate("text", x = 300, y = 2.4, label = paste0('nRMSE = ', 
                                                    round((mean(sum.ts.cire[[5]]$RtSqrtError))/(mean(sum.ts.cire[[5]]$Mean)),3)),size=3)
cire.6 <- ggplot() +
  geom_line(aes(x=sum.ts.cire[[6]]$DOY, y=sum.ts.cire[[6]]$Predicted), size=1, col='red')+
  geom_ribbon(aes(x=sum.ts.cire[[6]]$DOY, ymin=(sum.ts.cire[[6]]$Predicted-sum.ts.cire[[6]]$SD), 
                  ymax=(sum.ts.cire[[6]]$Predicted+sum.ts.cire[[6]]$SD)), alpha=0.2)+
  geom_point(aes(x=sum.ts.cire[[6]]$DOY, y=sum.ts.cire[[6]]$Mean), col='black', size=1)+
  labs(x='DOY', y='CIRE', title=tree.name[6])+
  ylim(0,2.4)+
  scale_x_continuous(breaks = round(seq(50, 365, by = 25),50))+
  annotate("text", x = 300, y = 2.4, label = paste0('nRMSE = ', 
                                                    round((mean(sum.ts.cire[[6]]$RtSqrtError))/(mean(sum.ts.cire[[6]]$Mean)),3)),size=3)
cire.9 <- ggplot() +
  geom_line(aes(x=sum.ts.cire[[7]]$DOY, y=sum.ts.cire[[7]]$Predicted), size=1, col='red')+
  geom_ribbon(aes(x=sum.ts.cire[[7]]$DOY, ymin=(sum.ts.cire[[7]]$Predicted-sum.ts.cire[[7]]$SD), 
                  ymax=(sum.ts.cire[[7]]$Predicted+sum.ts.cire[[7]]$SD)), alpha=0.2)+
  geom_point(aes(x=sum.ts.cire[[7]]$DOY, y=sum.ts.cire[[7]]$Mean), col='black', size=1)+
  labs(x='DOY', y='CIRE', title=tree.name[7])+
  ylim(0,2.4)+
  scale_x_continuous(breaks = round(seq(50, 365, by = 25),50))+
  annotate("text", x = 300, y = 2.2, label = paste0('nRMSE = ', 
                                                    round((mean(sum.ts.cire[[7]]$RtSqrtError))/(mean(sum.ts.cire[[7]]$Mean)),3)),size=3)


per_species_vi_plot <- cowplot::plot_grid(ndvi.4 + theme(axis.text.x = element_blank(),
                                  axis.ticks.x = element_blank(),
                                  axis.title.x = element_blank(),
                                  axis.title.y = element_blank(),
                                  plot.title = element_text(color = "black", size = 12, face = "bold"),
                                  plot.subtitle = element_text(color = "black", size=10, face='italic')),
                   evi.4 + theme(plot.title=element_text(color = "black", size = 12, face = "bold"),
                                 axis.text.x = element_blank(),
                                 axis.ticks.x = element_blank(),
                                 axis.title.x = element_blank(),
                                 axis.title.y = element_blank()),
                   osavi.4 + theme(plot.title=element_text(color = "black", size = 12, face = "bold"),
                                     axis.text.x = element_blank(),
                                     axis.ticks.x = element_blank(),
                                     axis.title.y = element_blank(),
                                     axis.title.x = element_blank()),
                   cire.4 + theme(plot.title=element_text(color = "black", size = 12, face = "bold"),
                                  axis.text.x = element_blank(),
                                  axis.ticks.x = element_blank(),
                                  axis.title.x = element_blank(),
                                  axis.title.y = element_blank()),
                   ndvi.9 + theme(axis.text.x = element_blank(),
                                  axis.ticks.x = element_blank(),
                                  axis.title.x = element_blank(),
                                  axis.title.y = element_blank(),
                                  plot.title = element_text(color = "black", size = 12, face = "bold"),
                                  plot.subtitle = element_text(color = "black", size=10, face='italic')),
                   evi.9 + theme(plot.title=element_blank(),
                                 axis.text.x = element_blank(),
                                 axis.ticks.x = element_blank(),
                                 axis.title.x = element_blank(),
                                 axis.title.y = element_blank()),
                   osavi.9 + theme(plot.title=element_blank(),
                                     axis.text.x = element_blank(),
                                     axis.ticks.x = element_blank(),
                                     axis.title.x = element_blank(),
                                     axis.title.y = element_blank()),
                   cire.9 + theme(plot.title=element_blank(),
                                  axis.text.x = element_blank(),
                                  axis.ticks.x = element_blank(),
                                  axis.title.x = element_blank(),
                                  axis.title.y = element_blank()),
                   ndvi.5 + theme(axis.text.x = element_blank(),
                                  axis.ticks.x = element_blank(),
                                  axis.title.x = element_blank(),
                                  axis.title.y = element_blank(),
                                  plot.title = element_text(color = "black", size = 12, face = "bold"),
                                  plot.subtitle = element_text(color = "black", size=10, face='italic')),
                   evi.5 + theme(plot.title=element_blank(),
                                 axis.text.x = element_blank(),
                                 axis.ticks.x = element_blank(),
                                 axis.title.x = element_blank(),
                                 axis.title.y = element_blank()),
                   osavi.5 + theme(plot.title=element_blank(),
                                     axis.text.x = element_blank(),
                                     axis.ticks.x = element_blank(),
                                     axis.title.x = element_blank(),
                                     axis.title.y = element_blank()),
                   cire.5 + theme(plot.title=element_blank(),
                                  axis.text.x = element_blank(),
                                  axis.ticks.x = element_blank(),
                                  axis.title.x = element_blank(),
                                  axis.title.y = element_blank()),
                   ndvi.3 + theme(axis.text.x = element_blank(),
                                  axis.ticks.x = element_blank(),
                                  axis.title.x = element_blank(),
                                  axis.title.y = element_blank(),
                                  plot.title = element_text(color = "black", size = 12, face = "bold"),
                                  plot.subtitle = element_text(color = "black", size=10, face='italic')),
                   evi.3 + theme(plot.title=element_blank(),
                                 axis.text.x = element_blank(),
                                 axis.ticks.x = element_blank(),
                                 axis.title.x = element_blank(),
                                 axis.title.y = element_blank()),
                   osavi.3 + theme(plot.title=element_blank(),
                                     axis.text.x = element_blank(),
                                     axis.ticks.x = element_blank(),
                                     axis.title.x = element_blank(),
                                     axis.title.y = element_blank()),
                   cire.3 + theme(plot.title=element_blank(),
                                  axis.text.x = element_blank(),
                                  axis.ticks.x = element_blank(),
                                  axis.title.x = element_blank(),
                                  axis.title.y = element_blank()),
                   ndvi.2 + theme(axis.text.x = element_blank(),
                                  axis.ticks.x = element_blank(),
                                  axis.title.x = element_blank(),
                                  axis.title.y = element_blank(),
                                  plot.title = element_text(color = "black", size = 12, face = "bold"),
                                  plot.subtitle = element_text(color = "black", size=10, face='italic')),
                   evi.2 + theme(plot.title=element_blank(),
                                 axis.text.x = element_blank(),
                                 axis.ticks.x = element_blank(),
                                 axis.title.x = element_blank(),
                                 axis.title.y = element_blank()),
                   osavi.2 + theme(plot.title=element_blank(),
                                     axis.text.x = element_blank(),
                                     axis.ticks.x = element_blank(),
                                     axis.title.x = element_blank(),
                                     axis.title.y = element_blank()),
                   cire.2 + theme(plot.title=element_blank(),
                                  axis.text.x = element_blank(),
                                  axis.ticks.x = element_blank(),
                                  axis.title.x = element_blank(),
                                  axis.title.y = element_blank()),
                   ndvi.6 + theme(axis.text.x = element_blank(),
                                  axis.ticks.x = element_blank(),
                                  axis.title.x = element_blank(),
                                  axis.title.y = element_blank(),
                                  plot.title = element_text(color = "black", size = 12, face = "bold"),
                                  plot.subtitle = element_text(color = "black", size=10, face='italic')),
                   evi.6 + theme(plot.title=element_blank(),
                                 axis.text.x = element_blank(),
                                 axis.ticks.x = element_blank(),
                                 axis.title.x = element_blank(),
                                 axis.title.y = element_blank()),
                   osavi.6 + theme(plot.title=element_blank(),
                                     axis.text.x = element_blank(),
                                     axis.ticks.x = element_blank(),
                                     axis.title.x = element_blank(),
                                     axis.title.y = element_blank()),
                   cire.6 + theme(plot.title=element_blank(),
                                  axis.text.x = element_blank(),
                                  axis.ticks.x = element_blank(),
                                  axis.title.x = element_blank(),
                                  axis.title.y = element_blank()),
                   ndvi.1 + theme(axis.title.y = element_blank(),
                                  plot.title = element_text(color = "black", size = 12, face = "bold"),
                                  plot.subtitle = element_text(color = "black", size=10, face='italic')),
                   evi.1 + theme(plot.title=element_blank(),
                                 axis.title.y = element_blank()),
                   osavi.1 + theme(plot.title=element_blank(),
                                     axis.title.y = element_blank()),
                   cire.1 + theme(plot.title=element_blank(),
                                  axis.title.y = element_blank()),
                   nrow = 7,
                   ncol=4,
                   allign='v')

rm(ndvi.1,ndvi.2,ndvi.3,ndvi.4,ndvi.5,ndvi.6,ndvi.9,
   evi.1,evi.2,evi.3,evi.4,evi.5,evi.6,evi.9,
   osavi.1,osavi.2,osavi.3,osavi.4,osavi.5,osavi.6,osavi.9,
   cire.1,cire.2,cire.3,cire.4,cire.5,cire.6,cire.9)


########################## 3) RMSE under various iterations 
dl_iteration_plot <- ggplot(dlog.rmse.nit, aes(x = N_iterations, y = MeanRMSE)) + 
  geom_point()+
  geom_smooth(method = "loess", se=F)+
  labs(x='Iterations', y='Average RMSE (NDVI)', 
       title='Average RMSE DL Fit on Ten Random Trees using NDVI')


########################## 4) Plot actual vs average temperature and rainfall in study area
temp <- ggplot(data=w.data) +
  geom_point(aes(x=DOY, y=T21), alpha=0.1, col='red', fill=NA, size=1)+
  geom_smooth(aes(x=DOY, y=T21), col='red', se=F)+
  geom_line(aes(x=DOY, y=Temp), col='black')+
  labs(y = 'Temperature (?C)')+
  theme(legend.position="bottom")


rain <- ggplot(data=avg.m) +
  geom_smooth(aes(x=DOY, y=R21), col='red', se=F)+
  geom_smooth(aes(x=DOY, y=Rain), col='black', se=F)+
  geom_point(aes(x=DOY, y=R21), col='red', alpha=0.3, fill=NA, size=2)+
  labs(y = 'Precipitation (mm)')+
  ylim(0,50)

temp_rain_plot <- plot_grid(temp, rain)

rm(temp, rain)

