#DEFINE THERMCOMPLETE_LOC "完成。"
#DEFINE WIN32FONT  '宋体'
#DEFINE WIN95FONT  '宋体'
#DEFINE DBCS_LOC   "81 82 86 88"

para cMsgInfo,nMaxvalue
oProcessBar= CREATEOBJECT("thermometer",cMsgInfo,nMaxvalue)
oProcessBar.show

*-- 定义进度条表单属性
DEFINE CLASS thermometer AS form

 Top = 196
 Left = 142
 Height = 100
 Width = 356
 AutoCenter = .T.
 BackColor = RGB(192,192,192)
 BorderStyle = 0
 Caption = ""
 Closable = .F.
 ControlBox = .F.
 MaxButton = .F.
 MinButton = .F.
 Movable = .F.
 AlwaysOnTop = .F.
 iPercentage = 0
 cCurrentTask = ''
 shpThermBarMaxWidth = 322
 cThermRef = ""
 Maxvalue = 0
 Name = "thermometer"

 *-- 外边框左亮线
 ADD OBJECT shape3 AS shape WITH ;
  BorderColor = RGB(255,255,255), ;
  Height = 97, ;
  Left = 1, ;
  Top = 1, ;
  Width = 1, ;
  Name = "Shape3"
  
 *-- 外边框左暗线
 ADD OBJECT shape10 AS shape WITH ;
  BorderColor = RGB(128,128,128), ;
  Height = 93, ;
  Left = 3, ;
  Top = 3, ;
  Width = 1, ;
  Name = "Shape10"

 *-- 外边框上亮线
 ADD OBJECT shape2 AS shape WITH ;
  BorderColor = RGB(255,255,255), ;
  Height = 1, ;
  Left = 1, ;
  Top = 1, ;
  Width = 353, ;
  Name = "Shape2"

 *-- 外边框上暗线
 ADD OBJECT shape9 AS shape WITH ;
  BorderColor = RGB(128,128,128), ;
  Height = 1, ;
  Left = 3, ;
  Top = 3, ;
  Width = 349, ;
  Name = "Shape9"

 *-- 外边框右亮线
 ADD OBJECT shape8 AS shape WITH ;
  BorderColor = RGB(255,255,255), ;
  Height = 94, ;
  Left = 352, ;
  Top = 3, ;
  Width = 1, ;
  Name = "Shape8"

 *-- 外边框右暗线
 ADD OBJECT shape6 AS shape WITH ;
  BorderColor = RGB(128,128,128), ;
  Height = 98, ;
  Left = 354, ;
  Top = 1, ;
  Width = 1, ;
  Name = "Shape6"

 *-- 外边框下亮线
 ADD OBJECT shape7 AS shape WITH ;
  BorderColor = RGB(255,255,255), ;
  Height = 1, ;
  Left = 3, ;
  Top = 96, ;
  Width = 350, ;
  Name = "Shape7"

 *-- 外边框下暗线
 ADD OBJECT shape4 AS shape WITH ;
  BorderColor = RGB(128,128,128), ;
  Height = 1, ;
  Left = 1, ;
  Top = 98, ;
  Width = 354, ;
  Name = "Shape4"


 *-- 覆盖整个表单
 ADD OBJECT shape1 AS shape WITH ;
  BackStyle = 0, ;
  Height = 100, ;
  Left = 0, ;
  Top = 0, ;
  Width = 356, ;
  Name = "Shape1"


 *-- 进度条上边
 ADD OBJECT shape11 AS shape WITH ;
  BorderColor = RGB(128,128,128), ;
  Height = 1, ;
  Left = 16, ;
  Top = 61, ;
  Width = 322, ;
  Name = "Shape11"

 *-- 进度条下边
 ADD OBJECT shape12 AS shape WITH ;
  BorderColor = RGB(255,255,255), ;
  Height = 1, ;
  Left = 16, ;
  Top = 77, ;
  Width = 323, ;
  Name = "Shape12"

 *-- 进度条左边
 ADD OBJECT shape13 AS shape WITH ;
  BorderColor = RGB(128,128,128), ;
  Height = 16, ;
  Left = 16, ;
  Top = 61, ;
  Width = 1, ;
  Name = "Shape13"

 *-- 进度条右边
 ADD OBJECT shape14 AS shape WITH ;
  BorderColor = RGB(255,255,255), ;
  Height = 17, ;
  Left = 338, ;
  Top = 61, ;
  Width = 1, ;
  Name = "Shape14"

 *-- 进度条
 ADD OBJECT shape5 AS shape WITH ;
  BorderStyle = 0, ;
  FillColor = RGB(192,192,192), ;
  FillStyle = 0, ;
  Height = 15, ;
  Left = 17, ;
  Top = 63, ;
  Width = 322, ;
  Name = "Shape5"

 *-- 进度条标题底色
 ADD OBJECT lbltitle AS label WITH ;
  FontName = WIN32FONT, ;
  FontSize = 9, ;
  BackStyle = 0, ;
  BackColor = RGB(192,192,192), ;
  Caption = "", ;
  Height = 16, ;
  Left = 18, ;
  Top = 14, ;
  Width = 319, ;
  WordWrap = .F., ;
  Name = "lblTitle"

 *-- 当前任务名称
 ADD OBJECT lbltask AS label WITH ;
  FontName = WIN32FONT, ;
  FontSize = 9, ;
  BackStyle = 0, ;
  BackColor = RGB(192,192,192), ;
  Caption = "", ;
  Height = 16, ;
  Left = 18, ;
  Top = 35, ;
  Width = 319, ;
  WordWrap = .F., ;
  Name = "lblTask"


 *-- 进度指示条
 ADD OBJECT shpthermbar AS shape WITH ;
  BorderStyle = 0, ;
  FillColor = RGB(128,128,128), ;
  FillStyle = 0, ;
  Height = 16, ;
  Left = 17, ;
  Top = 62, ;
  Width = 0, ;
  Name = "shpThermBar"


 *-- 百分比指示（黑色）
 ADD OBJECT lblpercentage AS label WITH ;
  FontName = WIN32FONT, ;
  FontSize = 9, ;
  BackStyle = 0, ;
  Caption = "0%", ;
  Height = 13, ;
  Left = 170, ;
  Top = 63, ;
  Width = 16, ;
  Name = "lblPercentage"

 *-- 百分比指示（白色）
 ADD OBJECT lblpercentage2 AS label WITH ;
  FontName = WIN32FONT, ;
  FontSize = 9, ;
  BackColor = RGB(0,0,128), ;
  BackStyle = 0, ;
  Caption = "Label1", ;
  ForeColor = RGB(255,255,255), ;
  Height = 13, ;
  Left = 170, ;
  Top = 63, ;
  Width = 0, ;
  Name = "lblPercentage2"

 *-- 退出信息
 ADD OBJECT lblescapemessage AS label WITH ;
  FontBold = .F., ;
  FontName = WIN32FONT, ;
  FontSize = 9, ;
  Alignment = 2, ;
  BackStyle = 0, ;
  BackColor = RGB(192,192,192), ;
  Caption = "", ;
  Height = 14, ;
  Left = 17, ;
  Top = 80, ;
  Width = 322, ;
  WordWrap = .F., ;
  Name = "lblEscapeMessage"

*!*********************************************************************
*!
*!      Procedure: complete
*!
*!*********************************************************************

PROCEDURE complete
  * This is the default complete message
  parameters m.cTask
  private iSeconds
  if parameters() = 0
   m.cTask = THERMCOMPLETE_LOC
  endif
  this.ShowBar(100,m.cTask)
ENDPROC


*!*********************************************************************
*!
*!      Procedure: ShowBar
*!
*!*********************************************************************

PROCEDURE ShowBar
  *-- 显示进度指示以及当前任务名称

  parameters iProgress,cTask

  if parameters() >= 2 .and. type('m.cTask') = 'C'
   this.cCurrentTask = m.cTask
  endif
  
  if ! this.lblTask.Caption == this.cCurrentTask
   this.lblTask.Caption = this.cCurrentTask
  endif

  m.iPercentage = m.iProgress
  
  m.iPercentage = int(m.iPercentage*100/this.Maxvalue)
  

  *-- 计算进度条的宽度
  if len(alltrim(str(m.iPercentage,3)))<>len(alltrim(str(this.iPercentage,3)))
   iAvgCharWidth=fontmetric(6,this.lblPercentage.FontName, ;
    this.lblPercentage.FontSize, ;
    iif(this.lblPercentage.FontBold,'B','')+ ;
    iif(this.lblPercentage.FontItalic,'I',''))
   this.lblPercentage.Width=txtwidth(alltrim(str(m.iPercentage,3)) + '%', ;
    this.lblPercentage.FontName,this.lblPercentage.FontSize, ;
    iif(this.lblPercentage.FontBold,'B','')+ ;
    iif(this.lblPercentage.FontItalic,'I','')) * iAvgCharWidth
   this.lblPercentage.Left=int((this.shpThermBarMaxWidth- ;
    this.lblPercentage.Width) / 2)+this.shpThermBar.Left-1
   this.lblPercentage2.Left=this.lblPercentage.Left
  endif
  this.shpThermBar.Width = int((this.shpThermBarMaxWidth)*m.iPercentage/100)
  this.lblPercentage.Caption = alltrim(str(m.iPercentage,3)) + '%'
  this.lblPercentage2.Caption = this.lblPercentage.Caption

  *-- 调整进度指示的黑白交替
  if this.shpThermBar.Left + this.shpThermBar.Width -1 >= ;
   this.lblPercentage2.Left
   if this.shpThermBar.Left + this.shpThermBar.Width - 1 >= ;
    this.lblPercentage2.Left + this.lblPercentage.Width - 1
    this.lblPercentage2.Width = this.lblPercentage.Width
   else
    this.lblPercentage2.Width = ;
     this.shpThermBar.Left + this.shpThermBar.Width - ;
     this.lblPercentage2.Left - 1
   endif
  else
   this.lblPercentage2.Width = 0
  endif
  this.iPercentage = m.iPercentage

  *-- 当进度到100%时，释放进度表单
  if m.iPercentage >= 100
   wait clear
   ??chr(7)
   oProcessbar.release
  endif

ENDPROC


*!*********************************************************************
*!
*!      Procedure: Init
*!
*!*********************************************************************

PROCEDURE Init

  parameters cTitle, nMaxvalue
  
  wait clear
  
  if type("nMaxvalue") <> "N"
   nMaxvalue = 100
  endif
  this.Maxvalue = nMaxvalue
  
  this.lblTitle.Caption = iif(empty(m.cTitle),'',m.cTitle)
  this.shpThermBar.FillColor = rgb(0,0,128)
  local cColor

  *-- 根据外观设置调整表单上的颜色和字体等。
  if fontmetric(1, WIN32FONT, 9, '') <> 13 .or. ;
   fontmetric(4, WIN32FONT, 9, '') <> 2 .or. ;
   fontmetric(6, WIN32FONT, 9, '') <> 5 .or. ;
   fontmetric(7, WIN32FONT, 9, '') <> 11
   this.SetAll('FontName', WIN95FONT)
  endif

  m.cColor = rgbscheme(1, 2)
  m.cColor = 'rgb(' + substr(m.cColor, at(',', m.cColor, 3) + 1)
  this.BackColor = &cColor
  this.Shape5.FillColor = &cColor
ENDPROC

ENDDEFINE
