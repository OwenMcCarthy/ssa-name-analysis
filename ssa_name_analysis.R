
#Load National & State data
data <- bind_rows(lapply(timespan,file_reader))
state.data <- bind_rows(lapply(states,state_file_reader))

#Add a ranking variable
data %<>% group_by(Year,Sex) %>%  
  mutate(counter = 1:n())


data %>% ungroup() %>% 
  mutate(ending = name_ends_with(Name,2),
         prefix = name_starts_with(Name,1)) %>%  
  filter(Year>=1960 & Year <=1969
         & Sex=="M" 
         & counter < 40 & counter>20 & 
           !(ending %in% bad) &
           (prefix %in% LETTERS)) %>% 
  distinct(Name)
#write.table(paste(names[,'Name'],collapse=", "),"C:\\Users\\owenm\\Downloads\\names\\namesf0rcaroline.txt",row.names = FALSE,col.names=FALSE)

#send.mail(from = sender,
#          to = recipients,
#          subject = "Unique names starting with 'T' or 'H' that have a minimum frequency since 1990",
#          body = "C:\\Users\\owenm\\Downloads\\names\\namesf0rcaroline.txt",
#          smtp = list(host.name = "smtp.gmail.com", port = 465, 
#                      user.name = "owenmccart@gmail.com",            
#                      passwd = "uqrffryhgdqhxnmy", ssl = TRUE),
#          authenticate = TRUE,
#          send = TRUE)