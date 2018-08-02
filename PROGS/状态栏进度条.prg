SET STATUS BAR ON
PUBLIC obar
**Open table
cTable = GETFILE("dbf")
IF EMPTY(cTable)
   RETURN .T.
ENDIF   

**Create the Progress bar object
obar = CREATEOBJECT("POnStatus")
obar.pIndicatorStyle = "||"
SELECT * FROM (cTable) WHERE obar.DrawStatus(RECNO(), RECCOUNT())
SET MESSAGE TO
CLOSE ALL
RELEASE ALL

** Class definition for the Progress bar object
DEFINE CLASS POnStatus AS Custom
    pIndicatorStyle = ""
     
    PROCEDURE DrawStatus
    LPARAMETER nRecno, nReccount
    LOCAL nPtr, cIndicator
    
        nPtr = INT(nRecno*100/nReccount)
        cIndicator = REPLICATE(THIS.pIndicatorStyle, nPtr) + ;
           SPACE(2) + STR(nPtr)+"%"
        SET MESSAGE TO LEFT(cIndicator, LEN(cIndicator))
        RETURN .T.
    ENDPROC

ENDDEFINE  