&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
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
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE INPUT PARAMETER ictype   AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER icfrmt   AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ictext   AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER ocvalue AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER olsw    AS LOGICAL   NO-UNDO.
/* Local Variable Definitions ---                                       */
DEFINE VARIABLE h-wdgt     AS HANDLE  NO-UNDO.
DEFINE VARIABLE irow       AS INTEGER NO-UNDO.
DEFINE VARIABLE dcol       AS DECIMAL NO-UNDO.
DEFINE VARIABLE ifrmwidth  AS INTEGER NO-UNDO.
DEFINE VARIABLE ifrmheight AS INTEGER NO-UNDO.
DEFINE VARIABLE iwdgtwidth AS INTEGER NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnOK btncancel 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD check_date Dialog-Frame 
FUNCTION check_date RETURNS LOGICAL
  ( INPUT-OUTPUT indata AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD check_decimal Dialog-Frame 
FUNCTION check_decimal RETURNS LOGICAL
  ( INPUT indata AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD check_integer Dialog-Frame 
FUNCTION check_integer RETURNS LOGICAL
  ( INPUT indata AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btncancel 
     LABEL "&Cancel" 
     SIZE 10 BY .95.

DEFINE BUTTON btnOK 
     LABEL "&OK" 
     SIZE 10 BY .95.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     btnOK AT ROW 3.38 COL 1
     btncancel AT ROW 3.38 COL 11
     SPACE(1.00) SKIP(0.47)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "<insert dialog title>".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   FRAME-NAME                                                           */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* <insert dialog title> */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btncancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btncancel Dialog-Frame
ON CHOOSE OF btncancel IN FRAME Dialog-Frame /* Cancel */
DO:
ASSIGN olsw = FALSE.
APPLY "go" TO FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnOK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnOK Dialog-Frame
ON CHOOSE OF btnOK IN FRAME Dialog-Frame /* OK */
DO:
CASE ictype:
    WHEN "INTEGER" THEN DO:
        olsw = check_integer(INPUT ocvalue).
        IF NOT olsw THEN DO:
            MESSAGE "Value can't be 0" VIEW-AS ALERT-BOX INFO BUTTONS OK.
            RETURN.
        END.
        ELSE 
         ocvalue  = REPLACE (ocvalue,",","").
    END.
    WHEN "DATE" THEN DO:
        olsw = check_date(INPUT-OUTPUT ocvalue).
        IF NOT olsw THEN DO:
            /*MESSAGE "Enter date" VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
            RETURN.
        END.
    END.
    WHEN "DECIMAL" THEN DO:
        olsw = check_decimal(INPUT ocvalue).
        IF NOT olsw THEN DO:
            MESSAGE "Value can't be 0" VIEW-AS ALERT-BOX INFO BUTTONS OK.
            RETURN.
        END.
        ELSE 
         ocvalue =  REPLACE (ocvalue,",","").
    END.
    WHEN "CHARACTER" THEN DO:
        IF ocvalue = "" THEN DO:
            MESSAGE "Enter value" VIEW-AS ALERT-BOX INFO BUTTONS OK.
            RETURN.
        END.
        ELSE 
            ASSIGN ocvalue = '"' + ocvalue + '"'.
    END.
    WHEN "LOGICAL"  THEN DO:
        ocvalue = h-wdgt:SCREEN-VALUE.
        IF ocvalue = "" THEN DO:
            MESSAGE "Select value" VIEW-AS ALERT-BOX INFO BUTTONS OK.
            RETURN.
        END.
    END.
END CASE.
ASSIGN olsw = TRUE.
APPLY "go" TO FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN head_UI.
  RUN enable_UI.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE build_screen Dialog-Frame 
PROCEDURE build_screen :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
ASSIGN ifrmheight = 5.
CASE ictype:
    WHEN "CHARACTER" THEN ASSIGN iwdgtwidth = integer(substring(icfrmt,index(icfrmt,"(") + 1,R-INDEX(icfrmt,")") - INDEX(icfrmt,"(") - 1)) * 2 - 2.
    WHEN "DECIMAL" THEN ASSIGN iwdgtwidth = LENGTH(icfrmt) * 2.
    WHEN "INTEGER" THEN ASSIGN iwdgtwidth = LENGTH(icfrmt) * 2.
    WHEN "DATE" THEN DO: 
      ASSIGN iwdgtwidth = LENGTH(icfrmt) * 2 - 3
             ictext = ictext + " FORMATE MM/DD/YYYY" .
    END.
    WHEN "LOGICAL" THEN ASSIGN iwdgtwidth = 50.
   
    OTHERWISE DO:
        MESSAGE "no valid variable type" VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN.
    END.
END CASE.
IF iwdgtwidth = 0 THEN iwdgtwidth = 5.

ASSIGN irow = 2
       FRAME {&FRAME-NAME}:TITLE = ictext
       ifrmwidth = FRAME {&FRAME-NAME}:WIDTH-CHARS.


/*IF NOT (iwdgtwidth + 20 < ifrmwidth) THEN ifrmwidth = iwdgtwidth + 20. /*widget doesn't fit*/*/
ASSIGN ifrmwidth = iwdgtwidth + 20.
IF ifrmwidth < 49 THEN ASSIGN ifrmwidth = 50.
ASSIGN FRAME {&FRAME-NAME}:WIDTH-CHARS = ifrmwidth
       FRAME {&FRAME-NAME}:HEIGHT-CHARS = ifrmheight
       dcol = ifrmwidth / 2 - (iwdgtwidth / 2) 
       btnOk:COLUMN = 10
       btnCancel:COLUMN = ifrmwidth - 10 - btnCancel:WIDTH-CHARS
       btnOk:ROW = ifrmheight - 1.8
       btnCancel:ROW = ifrmheight - 1.8.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dialog-Frame  _DEFAULT-DISABLE
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
  HIDE FRAME Dialog-Frame.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dialog-Frame  _DEFAULT-ENABLE
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
  ENABLE btnOK btncancel 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE field_return Dialog-Frame 
PROCEDURE field_return :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE h-dummy AS HANDLE     NO-UNDO.

APPLY "entry" TO btnOK IN FRAME {&FRAME-NAME}.
ASSIGN h-dummy = SELF:HANDLE.
IF valid-handle(h-dummy) THEN DO:
    ASSIGN ocvalue = SELF:SCREEN-VALUE.
END.
ELSE RETURN.
APPLY "choose" TO btnOK IN FRAME {&FRAME-NAME}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE field_value-changed Dialog-Frame 
PROCEDURE field_value-changed :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE h-dummy AS HANDLE     NO-UNDO.

ASSIGN h-dummy = SELF:HANDLE.
IF valid-handle(h-dummy) THEN DO:
    ASSIGN ocvalue = SELF:SCREEN-VALUE.
END.
ELSE RETURN.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE head_UI Dialog-Frame 
PROCEDURE head_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*create widget*/
RUN build_screen.

CASE ictype:
    WHEN "LOGICAL" THEN DO:
        icfrmt = REPLACE(icfrmt, "/", ",").
        CREATE COMBO-BOX h-wdgt
        ASSIGN ROW = irow
        COLUMN = dcol
        WIDTH-CHARS = iwdgtwidth
        FORMAT = "X(30)"
        LIST-ITEMS = icfrmt
        INNER-LINES = 5
        FRAME = FRAME {&FRAME-NAME}:HANDLE
        /*HEIGHT = 0.75*/
        SENSITIVE = TRUE
        VISIBLE = TRUE
        SCREEN-VALUE = ENTRY(1,icfrmt)
        NAME = "cb"
        TRIGGERS:
          ON "VALUE-CHANGED" PERSISTENT RUN field_value-changed IN THIS-PROCEDURE.
          ON 'RETURN' PERSISTENT RUN field_return IN THIS-PROCEDURE.
        END TRIGGERS.
    END.
    OTHERWISE DO:
        icfrmt = REPLACE(icfrmt, "$", "").
        icfrmt = REPLACE(icfrmt, ",", "").
        CREATE FILL-IN h-wdgt
        ASSIGN ROW = irow
        COLUMN = dcol
        WIDTH-CHARS = iwdgtwidth
        DATA-TYPE = ictype
        FORMAT = icfrmt
        FRAME = FRAME {&FRAME-NAME}:HANDLE
        HEIGHT = 0.9
        SENSITIVE = YES
        VISIBLE = TRUE
        NAME = "f"
        TRIGGERS:
          ON "VALUE-CHANGED" /*"LEAVE"*/ PERSISTENT RUN field_value-changed IN THIS-PROCEDURE.
          ON 'RETURN' PERSISTENT RUN field_return IN THIS-PROCEDURE.
        END TRIGGERS.    
    END.
END.

APPLY "entry":u TO h-wdgt.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION check_date Dialog-Frame 
FUNCTION check_date RETURNS LOGICAL
  ( INPUT-OUTPUT indata AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE dtdate AS DATE NO-UNDO.

  ASSIGN dtdate = DATE(indata) NO-ERROR.
  IF ERROR-STATUS:ERROR OR dtdate = ? THEN RETURN FALSE.
  ELSE 
  DO:
    ASSIGN indata = string(dtdate,icfrmt).
    RETURN TRUE. /* Function return value. */
  END.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION check_decimal Dialog-Frame 
FUNCTION check_decimal RETURNS LOGICAL
  ( INPUT indata AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE ddata AS DECIMAL    NO-UNDO.
  ASSIGN ddata = DECIMAL(indata) NO-ERROR.
  IF ERROR-STATUS:ERROR OR ddata = 0 THEN RETURN FALSE.
  ELSE RETURN TRUE. /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION check_integer Dialog-Frame 
FUNCTION check_integer RETURNS LOGICAL
  ( INPUT indata AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE idata AS INTEGER    NO-UNDO.
  ASSIGN idata = INTEGER(indata) NO-ERROR.
  IF ERROR-STATUS:ERROR OR idata = 0 THEN RETURN FALSE.
  ELSE RETURN TRUE. /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

