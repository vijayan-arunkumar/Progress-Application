DEFINE TEMP-TABLE ttFields NO-UNDO
    FIELD fieldNum       AS INTEGER                   COLUMN-LABEL "Field Number"
    FIELD tableNum       AS INTEGER                   COLUMN-LABEL "Table Number"
    FIELD fieldOrder     as integer   format ">>9"    COLUMN-LABEL "Order"
    FIELD fieldName      AS CHARACTER FORMAT "X(35)"  COLUMN-LABEL "Field Name"
    FIELD fieldMandatory AS LOGICAL   FORMAT "YES/NO" COLUMN-LABEL "Mandatory"
    FIELD fieldDataType  AS CHARACTER FORMAT "X(15)"  COLUMN-LABEL "Datatype"
    FIELD fieldFormat    AS CHARACTER FORMAT "X(35)"  COLUMN-LABEL "Format"
    FIELD fieldLabel     AS CHARACTER FORMAT "X(35)"  COLUMN-LABEL "Label"
    FIELD fieldDesc      AS CHARACTER FORMAT "X(255)" COLUMN-LABEL "Description"
    

    INDEX PRIMARY tableNum   fieldNum
    INDEX NAME               fieldName
    index Order              fieldOrder
.
