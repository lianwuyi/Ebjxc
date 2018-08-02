*----------------------------------
*	防止程序被多次开启
*----------------------------------
Public handle
Declare Integer CreateFileMapping In kernel32.Dll Integer hFile, ;
	INTEGER lpFileMappingAttributes,Integer flProtect, ;
	INTEGER dwMaximumSizeHigh, Integer dwMaximumSizeLow, ;
	STRING lpName
Declare Integer GetLastError In kernel32.Dll
Declare Integer CloseHandle In kernel32.Dll Integer hObject

szname="ebjxc-sos"   
handle = CreateFileMapping(0xFFFFFFFF,0,4,0,128,szname)

If handle = 0
	Wait Window "CreateFileMapping 失败 - LastError: " + Ltrim(Str(GetLastError()))
	Return
Endif

If handle=0
	Messagebox("内存映射文件创建失败！",16,"错误")
	Return .F.
Else
	If GetLastError()=183
		=Messagebox("该程序程序已经运行！",16,"提示")
		Close All
		Clear Dlls
		Clear Events
		Quit
	Endif
ENDIF

**** 系统开始 ----
lcLastSetPath=SET("PATH")
CD "\ebjxc\"
SET PATH TO;DATA;FORMS;LIBS;MENUS;PROGS;BMP;EXCEL
***
SET SYSMENU TO    && 不带其他参数的 SET SYSMENU TO 命令废止 Visual FoxPro 主菜单栏
MODIFY WINDOW SCREEN FROM 0.00, 0.00 TO 48.00,168.50 &&更改桌面 , 1024X768
_SCREEN.Caption = "登录系统"
_SCREEN.Icon = "..\bmp\NET01.ICO"
_SCREEN.Picture = "..\bmp\main_bg.gif"
_SCREEN.CLOSABLE = .F.  && 取消窗口关闭表单按钮

**********************************
DO ..\PROGS\系统参数.prg

SET STATUS BAR ON   && 显示或移去图形状态栏 ,on 默认状态栏
SET SYSMENU TO      && 不带其他参数的 SET SYSMENU TO 命令废止 Visual FoxPro 主菜单栏
SET ECHO OFF        && (默认值)关闭 FoxPro 2.0 以前版本中的跟踪窗口
SET ESCAPE OFF      && 禁止运行的程序和命令在按 Esc 键后被中断。
SET SAFETY OFF      && 决定改写已有文件之前是否显示对话框
SET TALK OFF        && 决定 Visual FoxPro 是否显示命令结果
SET CENT ON         && 显示年份为4位
SET DATE ANSI       && yy.mm.dd (年月日格式)
SET DELETE ON       && ON 为使用范围子句处理记录(包括在相关表中的记录)的命令忽略标有删除标记的记录。
SET EXCL OFF        && (私有数据工作期的默认方式)允许网络上的任何用户共享和修改网络上打开的表。
SET HELP ON         && 允许打开帮助

* 更改主窗口属性
_SCREEN.Caption = "登录系统"
_SCREEN.Icon = "..\bmp\NET01.ICO" 
_SCREEN.Picture = "..\bmp\main_bg.gif"
MODIFY WINDOW SCREEN FROM 0.00, 0.00 TO 48.00,168.50 &&更改桌面 , 1024X768

* 系统登录 
@44,84 SAY c版本号 ;
 	 FONT "Arial" ,15 ;
     STYLE "BIUT";
     COLOR RGB(128,128,128,,,)   
     
DO FORM '..\forms\login.scx'

IF mem_on="OFF"
* 删除临时表 
CLOSE DATABASES ALL
CLOSE TABLES ALL 
SELECT 0
USE ..\data\mmk1.dbf EXCLUSIVE 
ZAP
USE
DO ..\PROGS\quit.prg
ENDIF 

IF  mem_on="ON"
  CLOSE DATABASES ALL
  CLOSE TABLES ALL 
  SELECT 0
  USE ..\data\mmk1.dbf EXCLUSIVE 
  ZAP
  USE

  * 恢复桌面窗口
  MODIFY WINDOW SCREEN FROM 0.00, 0.00 TO 48.00,168.50 &&更改桌面 , 1024X768
  * 恢复系统菜单
  * SET SYSMENU TO DEFAULT
  DO '..\progs\ebjxc.PRG'

ELSE
  * 恢复系统菜单
  SET SYSMENU TO DEFAULT
  * 恢复桌面窗口
  MODIFY WINDOW SCREEN FROM 0.00, 0.00 TO 48.00,168.50 &&更改桌面 , 1024X768
  SET STATUS BAR ON
  CLEAR
  RETURN
 
ENDIF