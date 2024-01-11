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
{t_table.i}
{t_field.i}
/* Parameters Definitions ---                                           */
DEFINE INPUT PARAMETER TABLE FOR ttTables.

/* Local Variable Definitions ---                                       */

DEFINE TEMP-TABLE ttselectedFld LIKE ttFields. 
DEFINE VARIABLE temp-string AS CHARACTER NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BROWSE-19

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttfields ttselectedFld

/* Definitions for BROWSE BROWSE-19                                     */
&Scoped-define FIELDS-IN-QUERY-BROWSE-19 ttFields.fieldOrder ttFields.fieldName ttFields.fieldLabel ttFields.fieldDataType ttFields.fieldFormat ttFields.fieldMandatory   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-19   
&Scoped-define SELF-NAME BROWSE-19
&Scoped-define QUERY-STRING-BROWSE-19 FOR EACH ttfields
&Scoped-define OPEN-QUERY-BROWSE-19 OPEN QUERY {&SELF-NAME} FOR EACH ttfields.
&Scoped-define TABLES-IN-QUERY-BROWSE-19 ttfields
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-19 ttfields


/* Definitions for BROWSE BROWSE-20                                     */
&Scoped-define FIELDS-IN-QUERY-BROWSE-20 ttselectedFld.fieldOrder ttselectedFld.fieldName ttselectedFld.fieldLabel ttselectedFld.fieldDataType ttselectedFld.fieldFormat ttselectedFld.fieldMandatory   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-20   
&Scoped-define SELF-NAME BROWSE-20
&Scoped-define QUERY-STRING-BROWSE-20 FOR EACH ttselectedFld
&Scoped-define OPEN-QUERY-BROWSE-20 OPEN QUERY {&SELF-NAME} FOR EACH ttselectedFld.
&Scoped-define TABLES-IN-QUERY-BROWSE-20 ttselectedFld
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-20 ttselectedFld


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-BROWSE-19}~
    ~{&OPEN-QUERY-BROWSE-20}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btn_mq Btn_QBack Cmb_Tables BROWSE-19 ~
BROWSE-20 BUTTON-2 BUTTON-10 
&Scoped-Define DISPLAYED-OBJECTS Cmb_Tables 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn_mq 
     LABEL "Move to query builder" 
     SIZE 33 BY 1.14
     FONT 3.

DEFINE BUTTON Btn_QBack AUTO-END-KEY  NO-FOCUS FLAT-BUTTON
     LABEL "<- Query" 
     SIZE 15 BY 1.19.

DEFINE BUTTON BUTTON-10 
     LABEL "< Remove" 
     SIZE 15 BY 1.14
     FONT 3.

DEFINE BUTTON BUTTON-2 
     LABEL "Add >" 
     SIZE 15 BY 1.14
     FONT 3.

DEFINE VARIABLE Cmb_Tables AS CHARACTER FORMAT "X(256)":U 
     LABEL "Table" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 22 BY 1
     FONT 3 DROP-TARGET NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-19 FOR 
      ttfields SCROLLING.

DEFINE QUERY BROWSE-20 FOR 
      ttselectedFld SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-19
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-19 C-Win _FREEFORM
  QUERY BROWSE-19 DISPLAY
      ttFields.fieldOrder
ttFields.fieldName
ttFields.fieldLabel
ttFields.fieldDataType
ttFields.fieldFormat
ttFields.fieldMandatory
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 66 BY 15
         BGCOLOR 8 FONT 3 ROW-HEIGHT-CHARS .81 FIT-LAST-COLUMN.

DEFINE BROWSE BROWSE-20
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-20 C-Win _FREEFORM
  QUERY BROWSE-20 DISPLAY
      ttselectedFld.fieldOrder
ttselectedFld.fieldName
ttselectedFld.fieldLabel
ttselectedFld.fieldDataType
ttselectedFld.fieldFormat
ttselectedFld.fieldMandatory
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 66 BY 15
         BGCOLOR 8 FONT 3 ROW-HEIGHT-CHARS .81 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     btn_mq AT ROW 1.95 COL 118 WIDGET-ID 4
     Btn_QBack AT ROW 1.95 COL 11 WIDGET-ID 14
     Cmb_Tables AT ROW 4.33 COL 19 COLON-ALIGNED WIDGET-ID 10
     BROWSE-19 AT ROW 6.71 COL 5 WIDGET-ID 200
     BROWSE-20 AT ROW 6.71 COL 91 WIDGET-ID 300
     BUTTON-2 AT ROW 12 COL 73.2 WIDGET-ID 6
     BUTTON-10 AT ROW 14 COL 73.2 WIDGET-ID 8
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 160.6 BY 24
         FONT 3
         CANCEL-BUTTON Btn_QBack WIDGET-ID 100.


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
/* SUPPRESS Window definition (used by the UIB) 
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Fields"
         HEIGHT             = 24
         WIDTH              = 160.4
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
                                                                        */
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME
ASSIGN C-Win = CURRENT-WINDOW.




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* BROWSE-TAB BROWSE-19 Cmb_Tables DEFAULT-FRAME */
/* BROWSE-TAB BROWSE-20 BROWSE-19 DEFAULT-FRAME */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-19
/* Query rebuild information for BROWSE BROWSE-19
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttfields.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BROWSE-19 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-20
/* Query rebuild information for BROWSE BROWSE-20
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttselectedFld.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BROWSE-20 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Fields */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Fields */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_mq
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_mq C-Win
ON CHOOSE OF btn_mq IN FRAME DEFAULT-FRAME /* Move to query builder */
DO:
RUN querybuilder.w (INPUT TABLE ttTables,INPUT TABLE ttselectedFld).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_QBack
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_QBack C-Win
ON CHOOSE OF Btn_QBack IN FRAME DEFAULT-FRAME /* <- Query */
DO:
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-10
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-10 C-Win
ON CHOOSE OF BUTTON-10 IN FRAME DEFAULT-FRAME /* < Remove */
DO:
  IF NOT AVAIL ttselectedfld THEN
    RETURN.
  
  CREATE ttfields.
  BUFFER-COPY ttselectedfld TO ttfields.
  RELEASE ttfields.
  
  DELETE ttselectedfld.
  
  {&OPEN-QUERY-BROWSE-19}
  {&OPEN-QUERY-BROWSE-20}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-2 C-Win
ON CHOOSE OF BUTTON-2 IN FRAME DEFAULT-FRAME /* Add > */
DO:
  IF NOT AVAIL ttfields THEN
    RETURN.
  FOR EACH ttselectedfld WHERE ttselectedfld.tableNum =  ttfields.tableNum AND ttselectedfld.FieldNum =  ttfields.FieldNum NO-LOCK:
    IF AVAIL ttselectedfld THEN
      RETURN.
  END.
  CREATE ttselectedfld.
  BUFFER-COPY ttfields TO ttselectedfld.
  RELEASE ttselectedfld.
  
  DELETE ttfields.
  
  {&OPEN-QUERY-BROWSE-19}
  {&OPEN-QUERY-BROWSE-20}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Cmb_Tables
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_Tables C-Win
ON VALUE-CHANGED OF Cmb_Tables IN FRAME DEFAULT-FRAME /* Table */
DO:
  FIND FIRST ttTables WHERE ttTables.tableName = Cmb_Tables:SCREEN-VALUE NO-ERROR.
  IF AVAIL ttTables THEN
     RUN retrievefields(INPUT ttTables.tableDBName,INPUT ttTables.tableNum).
                                 
   {&OPEN-QUERY-BROWSE-19} 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-19
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
  FIND FIRST ttTables  NO-ERROR.
   IF AVAIL ttTables THEN
     RUN retrievefields(INPUT ttTables.tableDBName,
                                 INPUT ttTables.tableNum).
  FOR EACH ttTables NO-LOCK:
    ASSIGN temp-string = IF temp-string = "" THEN ttTables.tableName 
                         ELSE temp-string + "," + ttTables.tableName.
  END.
  
  ASSIGN Cmb_Tables:LIST-ITEMS IN FRAME DEFAULT-FRAME = temp-string.
  
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
  /* Hide all frames. */
  HIDE FRAME DEFAULT-FRAME.
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
  DISPLAY Cmb_Tables 
      WITH FRAME DEFAULT-FRAME.
  ENABLE btn_mq Btn_QBack Cmb_Tables BROWSE-19 BROWSE-20 BUTTON-2 BUTTON-10 
      WITH FRAME DEFAULT-FRAME.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE retrievefields C-Win 
PROCEDURE retrievefields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER ipDB    AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER ipRecId AS INTEGER    NO-UNDO.

DEFINE VARIABLE hFields AS HANDLE     NO-UNDO.
DEFINE VARIABLE qFields AS HANDLE     NO-UNDO.
EMPTY TEMP-TABLE  ttFields.
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
FOR EACH ttselectedfld NO-LOCK ,
    EACH ttFields WHERE ttfields.tableNum =  ttselectedfld.tableNum AND ttfields.FieldNum =  ttselectedfld.FieldNum :
    
   IF AVAIL ttFields THEN
      DELETE ttFields.
END.
DELETE OBJECT qFields.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

