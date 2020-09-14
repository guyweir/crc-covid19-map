

t1 <- selected_data %>% group_by(`IMD MSOA Quintiles`) %>% 
  summarise(`sum covid deaths` = sum(`COVID-19 deaths unadjusted`),
            `sum pop` = sum(population))

t1$`COVID deaths per 100,000` <- (t1$`sum covid deaths`/t1$`sum pop`)*100000
t1$`COVID deaths per 100,000` <- round(t1$`COVID deaths per 100,000`,1)

write.csv(t1, file = "COVID deaths per IMD quintile.csv",row.names = F)
