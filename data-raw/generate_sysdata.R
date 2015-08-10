nbn.dsn <- "Driver=SQL Server;Server=INBOSQL03\\PRD;Database=NBNData;Trusted_Connection=True;"
devtools::use_data(nbn.dsn, internal = TRUE, overwrite = TRUE)
