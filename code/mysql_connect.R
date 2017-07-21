# Connect to MySQL

# Download and import RMySQL

# install.packages("RMySQL")
library(RMySQL)

# Connecting to MySQL:
# Once the RMySQL library is installed create a database connection object.

conn <- dbConnect(MySQL(), user='yen', password='1234', 
                 dbname='stdid', host='10.120.37.114')

# SHOW TABLES
table_list <- dbListTables(conn)

# DESC table
table_name <- "categories"
table_fields <- dbListFields(conn,table_name)

# Encoding
# enc_query <- "SET NAMES utf8;"
enc_query <- "SET NAMES big5;"
dbSendQuery(conn,enc_query)

# Table queries
query <- "SELECT * FROM categories;"
query_result <- dbSendQuery(conn,query)
fetch(query_result, n=-1)
dbClearResult(query_result)

# Fetch the results
# n: number of records to retrieve
# n = -1 => all pending records

# Write tables
dbWriteTable(conn, name='table_name', value=data.frame.name)




# Disconnect
dbDisconnect(conn)
