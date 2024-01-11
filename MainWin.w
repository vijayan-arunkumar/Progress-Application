&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{t_table.i}
{t_field.i}

DEFINE TEMP-TABLE ttselected LIKE ttTables.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BROWSE-7

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttTables ttFields

/* Definitions for BROWSE BROWSE-7                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-7 ttTables.tableName ttTables.tableLabe ttTables.tableDump   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-7   
&Scoped-define SELF-NAME BROWSE-7
&Scoped-define QUERY-STRING-BROWSE-7 FOR EACH ttTables NO-LOCK
&Scoped-define OPEN-QUERY-BROWSE-7 OPEN QUERY {&SELF-NAME} FOR EACH ttTables NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BROWSE-7 ttTables
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-7 ttTables


/* Definitions for BROWSE BROWSE-9                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-9 ttFields.fieldOrder ttFields.fieldName ttFields.fieldLabel ttFields.fieldDataType ttFields.fieldFormat ttFields.fieldMandatory   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-9   
&Scoped-define SELF-NAME BROWSE-9
&Scoped-define QUERY-STRING-BROWSE-9 FOR EACH ttFields NO-LOCK
&Scoped-define OPEN-QUERY-BROWSE-9 OPEN QUERY {&SELF-NAME} FOR EACH ttFields NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BROWSE-9 ttFields
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-9 ttFields


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-BROWSE-7}~
    ~{&OPEN-QUERY-BROWSE-9}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BUTTON-1 BUTTON-2 BUTTON-4 BROWSE-7 BROWSE-9 ~
Db_name 
&Scoped-Define DISPLAYED-OBJECTS Db_name 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-1 
     LABEL "Exit" 
     SIZE 10 BY 1.14
     FONT 3.

DEFINE BUTTON BUTTON-2 
     LABEL "Saved Query" 
     SIZE 18.4 BY 1.14
     FONT 3.

DEFINE BUTTON BUTTON-4 DEFAULT 
     LABEL "Move To Query" 
     SIZE 22 BY 1.14
     FGCOLOR 13 FONT 3.

DEFINE VARIABLE Db_name AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 23 BY .71
     FGCOLOR 13 FONT 3 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-7 FOR 
      ttTables SCROLLING.

DEFINE QUERY BROWSE-9 FOR 
      ttFields SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-7
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-7 C-Win _FREEFORM
  QUERY BROWSE-7 DISPLAY
      ttTables.tableName 
      ttTables.tableLabe
      ttTables.tableDump
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 68 BY 15.24
         BGCOLOR 8 FONT 3 ROW-HEIGHT-CHARS .86 FIT-LAST-COLUMN.

DEFINE BROWSE BROWSE-9
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-9 C-Win _FREEFORM
  QUERY BROWSE-9 DISPLAY
      ttFields.fieldOrder
ttFields.fieldName
ttFields.fieldLabel
ttFields.fieldDataType
ttFields.fieldFormat
ttFields.fieldMandatory
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 68 BY 15.24
         BGCOLOR 8 FONT 3 ROW-HEIGHT-CHARS .86 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     BUTTON-1 AT ROW 3.67 COL 100.6 WIDGET-ID 8
     BUTTON-2 AT ROW 3.67 COL 111.2 WIDGET-ID 12
     BUTTON-4 AT ROW 3.67 COL 130 WIDGET-ID 16
     BROWSE-7 AT ROW 7.67 COL 10.8 WIDGET-ID 200
     BROWSE-9 AT ROW 7.67 COL 83.8 WIDGET-ID 300
     Db_name AT ROW 4.33 COL 9 COLON-ALIGNED NO-LABEL WIDGET-ID 20
     "Data base connected" VIEW-AS TEXT
          SIZE 28 BY .62 AT ROW 3.62 COL 11 WIDGET-ID 18
          FONT 3
     "Fields" VIEW-AS TEXT
          SIZE 20 BY .95 AT ROW 6.71 COL 84 WIDGET-ID 6
          FONT 2
     "Tables" VIEW-AS TEXT
          SIZE 20 BY .95 AT ROW 6.71 COL 11 WIDGET-ID 4
          FONT 2
     "Progress App" VIEW-AS TEXT
          SIZE 20 BY 1.19 AT ROW 1.76 COL 11.2 WIDGET-ID 2
          FONT 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 160.6 BY 23.95 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Tables"
         COLUMN             = 25.6
         ROW                = 9.43
         HEIGHT             = 23.95
         WIDTH              = 160.6
         MAX-HEIGHT         = 24
         MAX-WIDTH          = 160.6
         VIRTUAL-HEIGHT     = 24
         VIRTUAL-WIDTH      = 160.6
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* BROWSE-TAB BROWSE-7 BUTTON-4 DEFAULT-FRAME */
/* BROWSE-TAB BROWSE-9 BROWSE-7 DEFAULT-FRAME */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-7
/* Query rebuild information for BROWSE BROWSE-7
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttTables NO-LOCK.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BROWSE-7 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-9
/* Query rebuild information for BROWSE BROWSE-9
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttFields NO-LOCK.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BROWSE-9 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Tables */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Tables */
DO:
  /* This event will close the window and terminate the procedure.  */
   MESSAGE "Do you want to close this application ? " 
   VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lChoice AS LOGICAL.
   IF lChoice THEN
   DO:
   APPLY "CLOSE":U TO THIS-PROCEDURE.
   RETURN NO-APPLY.
   END.
   ELSE RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-7
&Scoped-define SELF-NAME BROWSE-7
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-7 C-Win
ON VALUE-CHANGED OF BROWSE-7 IN FRAME DEFAULT-FRAME
DO:
EMPTY TEMP-TABLE ttFields .
  DO WITH FRAME {&FRAME-NAME}:
        RUN rfield(INPUT ttTables.tableDBName,
                                 INPUT ttTables.tableNum).
     END.
   
     {&OPEN-QUERY-BROWSE-9}    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 C-Win
ON CHOOSE OF BUTTON-1 IN FRAME DEFAULT-FRAME /* Exit */
DO:
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-2 C-Win
ON CHOOSE OF BUTTON-2 IN FRAME DEFAULT-FRAME /* Saved Query */
DO:
RUN ViewQuery.W.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-4 C-Win
ON CHOOSE OF BUTTON-4 IN FRAME DEFAULT-FRAME /* Move To Query */
DO:
  EMPTY TEMP-TABLE ttselected. 
  CREATE ttselected.
  BUFFER-COPY ttTables TO ttselected.
  RELEASE ttselected.
 
  RUN selectfields.w(INPUT TABLE ttselected ).
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */
/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */  
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  IF LDBNAME(1) = ? OR LDBNAME(1) = "" THEN
  DO:
    MESSAGE "No database connection was established."
      VIEW-AS ALERT-BOX ERROR BUTTONS OK.
     RETURN.
  END.
  ELSE DO:
     ASSIGN Db_name = LDBNAME(1).   
  
     DISPLAY Db_name WITH FRAME default-frame IN WINDOW c-win. 
     RUN retrivetables(INPUT Db_name).
  
     APPLY 'VALUE-CHANGED':U TO BROWSE-7.
  END.
  
  
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY Db_name 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE BUTTON-1 BUTTON-2 BUTTON-4 BROWSE-7 BROWSE-9 Db_name 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE retrivetables C-Win 
PROCEDURE retrivetables :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER ipDB AS CHARACTER  NO-UNDO.

DEFINE VARIABLE hTables  AS HANDLE     NO-UNDO.
DEFINE VARIABLE qTables  AS HANDLE     NO-UNDO.

CREATE BUFFER hTables  FOR TABLE STRING(ipDB) + "._FILE"  BUFFER-NAME "b_FILE".

CREATE QUERY qTables.
qTables:ADD-BUFFER(hTables).
qTables:QUERY-PREPARE("FOR EACH " + hTables:NAME + " WHERE " +  hTables:NAME + "._Tbl-Type = " + '"' + "T" + '"' + " NO-LOCK").
qTables:QUERY-OPEN().

qTables:GET-FIRST().
DO WHILE hTables:AVAILABLE:

   CREATE ttTables.
   ASSIGN ttTables.tableNum    = hTables:RECID
          ttTables.tableName   = hTables:BUFFER-FIELD("_file-name"):BUFFER-VALUE
          ttTables.tableDump   = hTables:BUFFER-FIELD("_dump-name"):BUFFER-VALUE
          ttTables.tableLabel  = hTables:BUFFER-FIELD("_file-label"):BUFFER-VALUE
          ttTables.tableDesc   = hTables:BUFFER-FIELD("_desc"):BUFFER-VALUE
          ttTables.tableDBName = ipDB.
   
   qTables:GET-NEXT().

END.

qTables:QUERY-CLOSE().
{&OPEN-QUERY-BROWSE-7} 
DELETE OBJECT qTables.
DELETE OBJECT hTables.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE rfield C-Win 
PROCEDURE rfield :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER ipDB    AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER ipRecId AS INTEGER    NO-UNDO.

DEFINE VARIABLE hFields AS HANDLE     NO-UNDO.
DEFINE VARIABLE qFields AS HANDLE     NO-UNDO.

CREATE BUFFER hFields  FOR TABLE ipDB + "._FIELD" BUFFER-NAME "b_FIELD".

CREATE QUERY qFields.
qFields:ADD-BUFFER(hFields).
qFields:QUERY-PREPARE("FOR EACH " + hFields:NAME + " NO-LOCK WHERE " + hFields:NAME + "._file-recid = INT('" + STRING(ipRecId) + "')").
qFields:QUERY-OPEN().

qFields:GET-FIRST().
DO WHILE hFields:AVAILABLE:

   CREATE ttFields.
   ASSIGN ttFields.fieldNum       = hFields:RECID
          ttFields.tableNum       = ipRecId
          ttFields.fieldOrder     = hFields:BUFFER-FIELD("_order"):BUFFER-VALUE
          ttFields.fieldName      = hFields:BUFFER-FIELD("_field-name"):BUFFER-VALUE
          ttFields.fieldMandatory = hFields:BUFFER-FIELD("_mandatory"):BUFFER-VALUE
          ttFields.fieldDataType  = hFields:BUFFER-FIELD("_Data-type"):BUFFER-VALUE
          ttFields.fieldFormat    = hFields:BUFFER-FIELD("_format"):BUFFER-VALUE
          ttFields.fieldLabel     = hFields:BUFFER-FIELD("_label"):BUFFER-VALUE
          ttFields.fieldDesc      = hFields:BUFFER-FIELD("_desc"):BUFFER-VALUE.

   qFields:GET-NEXT().

END.
qFields:QUERY-CLOSE().

DELETE OBJECT qFields.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

