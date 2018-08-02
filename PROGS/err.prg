PARAMETERS nError,cMessage,cMessage1,cProgram,nLineno  &&接收参数
SET TEXTMERGE DELIMITERS TO  &&指定文本合并分隔符为"<<"和">>"
SET TEXTMERGE ON  &&对文本合并分隔符括起的字段、变量、函数等进行计算
SET TEXTMERGE TO ErrorLog.txt ADDITIVE NOSHOW  &&设置为追加到文件尾
\---------------------------------------------------------------------
\<<DATE( )>> <<TIME( )>> 错误记录
\程序标题: <<_Screen.Caption>>
\程序开发版本: <<VERSION(1)>>
DO CASE 
    CASE _Screen.WindowState = 0
        \窗口状态: 普通
    CASE _Screen.WindowState = 1
        \窗口状态: 最小化
    CASE _Screen.WindowState = 2
        \窗口状态: 最大化
ENDCASE 
\窗口可视: <<IIF(_Screen.Visible= .T.  , "可见" , "不可见")>>
\窗口集合数: <<_Screen.FormCount>>
\网络机器信息: <<SYS(0)>>
\
\执行程序: <<JUSTFNAME(SYS(16,1))>>
\执行程序所在目录: <<JUSTPATH(SYS(16,1))>>
\执行程序所在目录磁盘空间: <<DISKSPACE(JUSTDRIVE(SYS(16,1)))>>
\
\默认目录: <<SYS(5)>><<SYS(2003)>>
\默认目录磁盘空间: <<DISKSPACE(SYS(5))>>
\文件搜寻路径: <<SET("PATH")>>
\
\系统临时目录: <<SYS(2023)>>
\虚拟内存池大小: <<SYS(1001)>>
\
\正在使用的工作区: <<Alias()>>
\活动字段: <<VARREAD()>>
\
IF TYPE("_Screen.ActiveForm.Name")="C"
    \活动表单: <<_Screen.ActiveForm.Name>>
    \表单标题: <<_Screen.ActiveForm.Caption>>
    \表单基类: <<_Screen.ActiveForm.BaseClass>>
    \表单派生: <<_Screen.ActiveForm.Class>>
    \表单派生库: <<_Screen.ActiveForm.ClassLibrary>>
    \表单位置: <<SYS(1271, _Screen.ActiveForm)>>
ELSE 
    \无活动表单   
ENDIF 
IF TYPE("_Screen.ActiveForm.ActiveControl")="O"
    \活动控制: <<_Screen.ActiveForm.ActiveControl.Name>>
    IF TYPE("_Screen.ActiveForm.ActiveControl.Caption") = "C"
        \控制标题: <<_Screen.ActiveForm.ActiveControl.Caption>>
    ENDIF 
    \控件基类: <<_Screen.ActiveForm.ActiveControl.BaseClass>>
    \控件派生: <<_Screen.ActiveForm.ActiveControl.Class>>
    \控件派生库: <<_Screen.ActiveForm.ActiveControl.ClassLibrary>>
    \控件位置: <<SYS(1271, _Screen.ActiveForm.ActiveControl)>>
ELSE 
    \无活动控制   
ENDIF 
\
\错误代号: <<nError>>
\错误信息: <<cMessage>>
\产生错误的位置: <<cProgram>>
\所在行号: <<nLineno>>
\产生错误的代码: <<cMessage1>>
\
\输出内存使用情况 -> MemoryLog.txt
\输出工作环境到 -> StatusLog.txt
SET SAFETY OFF 
DISPLAY MEMORY NOCONSOLE TO FILE MemoryLog.txt
DISPLAY STATUS NOCONSOLE TO FILE StatusLog.txt
\
\---------------------------------------------------------------------
SET TEXTMERGE TO 
nValue= MESSAGEBOX("程序发生错误！详细信息如下："+CHR(13)+CHR(13)+;
    "错误代号: " + LTRIM( STR(nError) ) + CHR(13) + ;
    "错误行号: " + LTRIM( STR(nLineno) ) + CHR(13) + ;
    "错误信息: " + cMessage + CHR(13) + ;
    "错误代码: " + cMessage1 + CHR(13) + ;
    "错误位置: " + cProgram + CHR(13) + CHR(13) + ;
    "该错误已经记录到文件:Errorlog.txt,Memorylog.txt,Statuslog.txt。";
    ,2+48,"信息")
DO CASE 
    CASE nValue=3  &&选择终止时发生
        QUIT 
    CASE nValue=4  &&选择重试时发生
        RETRY 
    CASE nValue=5  &&选择忽略时发生
        RETURN 
ENDCASE 
