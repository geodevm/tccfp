#===============================================================================
### Data cleaning protocol for biological data
# Collect total runtime --------------------------------------------------------
begin <- proc.time() 
dbWriteTable(con, name = "table_name", value = df,
             field.types = c(dte = "date", val = "double(20,10)"), 
             row.names = FALSE)