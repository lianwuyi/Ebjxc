* 常用语法：
* DoDefault() && 继续执行默认代码  
* thisform.货品编码1.setfocus  &&光标

* && 取整数   floor(val(str(n_cfid)))       floor() 取整，去掉空格,  int() 取整是不去掉空格； CEILING(3.1) = 4   int(3.1) = 3  

* ThisForm.Refresh()  && 刷新表单
* THISFORM.Grid1.Refresh()  && 刷新表格
* 货品编码: 写入2 = 'Y'  rele 类别s,货品编码s,品名规格s,单位s,单价s,货架号s,备注s,颜色s

*打印机：REPORT FORM c:\jck\forms\计划成品表.frx NOEJECT NOCONSOLE TO PRINTER   
*        REPORT FORM ..\forms\dzbg.frx NOEJECT NOCONSOLE PREV
******************************************************
** 保存按钮
*  =cursorsetprop("Buffering",5)
*   =tableupdate(.t.)             && 保存数据
*  thisform.cmd添加.enabled=.t.
*  thisform.cmd保存.enabled=.f.
*  thisform.cmd放弃.enabled=.f.
*  thisform.cmd修改.enabled=.t.
*  ThisForm.Refresh()

*  =cursorsetprop("Buffering",2)
*  wait window '已保存在临时表，请记帐' nowait noclear
******************************************************

* 1024 * 768  表单：高 630 宽：880

*   TOTAL TO TableName ON FieldName   [FIELDS FieldNameList]   [Scope]
*       [FOR lExpression1]   [WHILE lExpression2]   [NOOPTIMIZE]

****************************************************************
* IF file("..\ck\arjs\&dbfs"+".arj")=.T.

*  wait windows "文件重名,请重新输入！" AT 8,30 TIMEOUT  2
* return
* ENDIF
** -g:10748612 为加密码压缩，-g: 密码开关， "10748612" 为密码
**解压时，要带上密码开关才能解压缩


* run arj a -g:10748612 ..\ck\arjs\&dbfs  ..\ck\data\*.*
* run arj e -g:10748612  "\fapiao\arjs\&dbfs_1

********************************************************
*     INDEX ON eExpression TO IDXFileName | TAG TagName [OF CDXFileName]
*     [FOR lExpression]
 *    [COMPACT]
  *   [ASCENDING | DESCENDING]
   *  [UNIQUE | CANDIDATE]
    * [ADDITIVE]
****

 *************************
*   select *  from cPATHS+'syslydj.dbf' ;
*        where alltrim(联业订单号) == djh1 ;
*        into table ..\test.dbf
*      into cursor tmp

*  UPDATE cPATHS+'syslydj.dbf' SET 完成记号 = "*" , 完成日期 = rq1 WHERE alltrim(联业订单号) == djh1
 **************************        
 
 ***生成 Excel 表：************************************
*  USE c:\bjxt\data\传真表.dbf AGAIN IN 0 ALIAS 传真表
*    SELECT 传真表
*    BROWSE LAST
*    COPY TO "c:\bjxt\e-mail_html\2.xls" TYPE XL5
*
********************************************************
*打印机：REPORT FORM c:\jck\forms\计划成品表.frx NOEJECT NOCONSOLE TO PRINTER  


*新常用命令

***  SQLDISCONNECT(0)  && 断开所有数据库连接

*** SQL 和 VFP 中日期字段处理
* 1） SQL 和 VFP 表中的日期字段都有必须为可以输入空值 .NULL.
* 2)  赋予一个变量空值： STORE .NULL. TO rq1
*    再用替代命令： repl 订货日期 with rq1 for 订货日期 = {    .  .  }
*    这样，VFP 表中日期为空的就可以赋予空值： .NULL.
** 
** 设计表时，不允许空值的数值型字段要给予初值0，否则，在给表添加记录时，如不对空值进行录入数值或0 就会出错，即 INSERT INTO 语句出错 ！
** 设计表时，对关键字段要进行索引，这样才提高查询、修改、插入记录时才快。


***************************************************************
* 在SQL2000企业管理器中怎样输入NULL字符？
* ctrl+0,不会马上以<null>填充，但已经把<null>赋值给了该字段，不必去管他，用鼠标在其它字段上单击，会看到<null>已经写上去了
* SET NULLDISPLAY TO '' && 去除.NULL
* WAIT WINDOW NOWAIT "数据库记录写入表: 记录" + ALLTRIM(STR(kk)) + " of " + ALLTRIM(STR(ss))
* set message to  && 清空状态栏信息