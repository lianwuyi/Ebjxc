*虽然是搞数学的，计算机不是我的专业，但我为VFP疯狂！！！！
*添加自定义纸张的程序，试试吧，也许你会怀疑此代码。代码还可以精简，你会更加叫绝！
*第一个参数为纸张的名称,第二个参数为纸张的宽度(毫米),第三个参数为纸张的高度(毫米)
? setpager("自定义纸张",195,270)
? setpager("我的纸张",210,256)
function setpager
parameters pagername,pagerwith,pagerheight
declare integer OpenPrinter in winspool.drv string,integer @,integer
declare integer ClosePrinter in winspool.drv integer
declare integer AddForm in winspool.drv integer,integer,string @
declare integer HeapCreate in kernel32 integer,integer,integer
declare integer HeapAlloc in kernel32 integer,integer,integer 
declare integer HeapFree in kernel32 integer,integer,integer
declare HeapDestroy in kernel32 integer
declare RtlMoveMemory in kernel32 AS RtlCopy integer,string,integer
local lhPrinter
lhPrinter=0
if OpenPrinter(set("Printer",2),@lhPrinter,0)=0
return .F.
endif
local hHeap,lnFormName,lcForm,lnretval
hHeap=HeapCreate(0,4096,0)
lnFormName=HeapAlloc(hHeap,0,len(pagername)+1) 
=RtlCopy(lnFormName,pagername+chr(0),LEN(pagername)+1) 
lcForm=numtolong(0)+;
numtolong(lnFormName)+;
numtolong(pagerwith*1000) +;
numtolong(pagerheight*1000)+;
numtolong(0)+;
numtolong(0)+;
numtolong(pagerwith*1000)+;
numtolong(pagerheight*1000)
lnretval=AddForm(lhPrinter,1,@lcForm) 
=HeapFree(hHeap,0,lnFormName)
=ClosePrinter(lhPrinter)
if hHeap<>0
HeapDestroy(hHeap)
endif
return !lnretval=0
endfunc
function numtolong(tnNum)
local lcString
lcString=space(4)
declare RtlMoveMemory in kernel32 AS RtlCopyLong strinG @,Long @,Long
=RtlCopyLong(@lcString,bitor(tnNum,0),4)
return lcString
endfunc