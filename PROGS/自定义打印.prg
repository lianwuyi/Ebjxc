*��Ȼ�Ǹ���ѧ�ģ�����������ҵ�רҵ������ΪVFP��񣡣�����
*����Զ���ֽ�ŵĳ������԰ɣ�Ҳ����ỳ�ɴ˴��롣���뻹���Ծ��������ӽо���
*��һ������Ϊֽ�ŵ�����,�ڶ�������Ϊֽ�ŵĿ��(����),����������Ϊֽ�ŵĸ߶�(����)
? setpager("�Զ���ֽ��",195,270)
? setpager("�ҵ�ֽ��",210,256)
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