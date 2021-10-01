library(tidyverse)

library(DBI)

formatNumber <-
    function(value, accuracy = NULL) {
        
        if (is.na(value) || (value == -Inf) || (value == Inf))
            return(value)

        if (value < 1e6)
            return(scales::comma(value))
        
        # else  ...
        if (is.null(accuracy)) {
            accuracy <- 0.001
            
        } else if (accuracy == -1)
            accuracy <- NULL

        value <- scales::label_number_si(accuracy = accuracy)(value)

        # know there's a more efficient way to do this, need to get the regex right for the final lookahead
        if (str_detect(value, "\\.[0]+\\D"))
            value <- str_remove(value, "\\.[0]+")
        

        all(value)
    }
formatNumber <- Vectorize(formatNumber)


# updated with newer function calls
writeToDataStore <-
    function(dataLoaded, dbConnection, dbTable, overwriteDataStore = FALSE) {
        
        if (is_null(dataLoaded) || (nrow(dataLoaded) == 0))
            stop("You must read in a dataframe containing at least one row!")
        
        #table_exists <- as.logical(dbGetQuery(dbConnection, paste0("SELECT COUNT(*) FROM sqlite_master WHERE name = '", 
        #                                                           dbTable, "' and type = 'table'")))
        
        table_exists <- dbExistsTable(dbConnection, dbTable)
        if (!table_exists | overwriteDataStore)
            currentRowCount <- 0
        else
            currentRowCount <- dbGetQuery(dbConnection, paste("SELECT COUNT(*) FROM", dbTable)) %>%
                                    as.integer
        
        
        if (!table_exists) 
            dbCreateTable(dbConnection, dbTable, dataLoaded)
        else 
            dbWriteTable(dbConnection, dbTable, dataLoaded, append = !overwriteDataStore, overwrite = overwriteDataStore)
        
        
        invisible(as.integer(dbGetQuery(dbConnection, paste("SELECT COUNT(*) FROM", dbTable))) - currentRowCount)
    }



removeDuplicatesFromDataStore <- 
    function(dbConnection = NULL, dbTable = NULL) {
        
        if (is_null(dbConnection) | is_null(dbTable))
            stop("You must specify database connection and table to deduplicate!")

        
        #if (!as.logical(dbGetQuery(dbConnection, 
        #                           paste0("SELECT COUNT(*) FROM sqlite_master WHERE name = '", dbTable, "' and type = 'table'"))))
        if (!dbExistsTable(dbConnection, dbTable))
            stop("No changes made; source table does not exist!")
        
        
        row_count <- as.integer(dbGetQuery(dbConnection, paste("SELECT COUNT(*) FROM", dbTable)))
        if (row_count == 0) {
            
            message(paste0("Table'", dbTable, "' empty; no changes made."))
            invisible(row_count) # nothing to do - allows simple conversion to FALSE
        }
        
        
        tmp_table <- paste0(dbTable, "_tmp")
        #if (as.integer(dbGetQuery(dbConnection, 
        #                          paste0("SELECT COUNT(*) FROM sqlite_master WHERE name = '", tmp_table, "' and type = 'table'"))) > 0) 
        if (dbExistsTable(dbConnection, tmp_table))
            dbRemoveTable(dbConnection, tmp_table)

        dbExecute(dbConnection, paste("CREATE TABLE ", tmp_table, " AS SELECT * FROM", dbTable, "WHERE 0"))
        dbExecute(dbConnection, paste("INSERT INTO", tmp_table,
                                      "SELECT DISTINCT * FROM", dbTable)
                 )
        
        row_count <- row_count - as.integer(dbGetQuery(dbConnection, paste("SELECT COUNT(*) FROM", tmp_table)))
        if (row_count > 0) {
            dbRemoveTable(dbConnection, dbTable)
            dbExecute(dbConnection, paste("ALTER TABLE", tmp_table, "RENAME TO", dbTable))
        } else
            dbRemoveTable(dbConnection, tmp_table)


        invisible(row_count) # no. of rows deleted
    }




