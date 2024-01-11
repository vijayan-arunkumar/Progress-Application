DEFINE TEMP-TABLE ttTables NO-UNDO
    FIELD tableNum    AS INTEGER                   COLUMN-LABEL "Table Number"
    FIELD tableName   AS CHARACTER FORMAT "X(32)"  COLUMN-LABEL "Table Name"
    FIELD tableDBName AS CHARACTER FORMAT "X(20)"  COLUMN-LABEL "Database"
    FIELD tableDump   AS CHARACTER FORMAT "X(10)"  COLUMN-LABEL "Dump Name"
    FIELD tableLabel  AS CHARACTER FORMAT "X(75)"  COLUMN-LABEL "Label"
    FIELD tableDesc   AS CHARACTER FORMAT "X(255)" COLUMN-LABEL "Description"

    INDEX PRIMARY tableNum
    INDEX NAME    tableName
    INDEX DUMP    tableDump.
