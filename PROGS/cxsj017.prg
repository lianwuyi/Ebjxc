
*CXSJ017.PRG
*人民币表现形式转换
*调用 "..\data\cxsj015.dbf"

SET DEBUG OFF
public xs

SET TALK OFF
DO WHILE .T.
*ACCEPT "请输入人民币总数:" TO zs1
*金额合计=1234567.12
store str(金额合计,10,2) to zs1 &&读入百万位数

*?zs1
*wait ""
assy=len(zs1)
aat=subs(zs1,1,assy-3)
aat1=subs(zs1,assy-1,2)
zs=aat+aat1
zsbl1=LEN(zs)
IF zsbl1>=1.AND.zsbl1<=11
EXIT
ENDIF
ENDDO

select 0
USE "..\data\cxsj015.dbf"
bk1="*"
bk2="*"
REPLACE sz WITH bk1 ALL
REPLACE dx1 WITH bk2 ALL
dx="零壹贰叁肆伍陆柒捌玖"
GOTO BOTTOM
DO WHILE zsbl1<>0
zsbl2=SUBSTR(zs,zsbl1,1)
dxbl1=SUBSTR(dx,(VAL(zsbl2)*2+1),2)
REPLACE sz WITH zsbl2
REPLACE dx1 WITH dxbl1
SKIP-1
zsbl1=zsbl1-1
ENDDO
*
goto top
do while .t.
if  dx1="零"
repl dx1 with "**"
skip
loop
else
exit
endif
enddo

* 
 
xs=""
GOTO TOP
DO WHILE .NOT.EOF()
xs=xs+dx1+" "+hz+" "

SKIP
ENDDO
*xs="大写:"+xs
* @ROW()+1.3,1 SAY "人民币(大写): "+xs+"    ￥"+zs1 ;
*       FONT "宋体",10

RELE A1
RELE A2
RELE A3
RELE A4
RELE A5
RELE A6
RELE A7
RELE A8
RELE A9
 
PUBLIC A1,A2,A3,A4,A5,A6,A7,A8,A9
i=1
scan
if i=1
A1=dx1
endif
if i=2
A2=dx1
endif
if i=3
A3=dx1
endif
if i=4
A4=dx1
endif
if i=5
A5=dx1
endif
if i=6
A6=dx1
endif
if i=7
A7=dx1
endif
if i=8
A8=dx1
endif
if i=9
A9=dx1
endif

i=i+1
endscan
use 