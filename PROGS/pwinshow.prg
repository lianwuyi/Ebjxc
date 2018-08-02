RELEASE c标题,c提示
PUBLIC  c标题,c提示

*** 【提前三天到期提醒】
IF c到期 - DATE() < 3  && 提前三天提示到期。
   c标题 = "【Wwwjxc 到期提醒】"
   c提示 = "软件将在"+ALLTRIM(STR(c到期 - DATE()))+"天后到期，;
   为了您的正常使用，请及时与供应商联系！"+CHR(13)+c技术支持
   DO ..\progs\popwindows.prg
ENDIF

IF c盘点查录 = "1" && 拥有盘点权限的，才提示
  *** 【提前28号就盘点提醒】
  IF DATE() = ctod(subs(dtoc(date()),1,8)+'30') OR DATE() = ctod(subs(dtoc(date()),1,8)+'28');
     OR DATE() = ctod(subs(dtoc(date()),1,8)+'29') OR DATE() = ctod(subs(dtoc(date()),1,8)+'31')
     c标题 = "【Wwwjxc 盘点提醒】"
     c提示 = "盘点日期已接近，请做好盘点准备！"
     DO ..\progs\popwindows.prg
  ENDIF

  *** 【1号盘点提醒】
  IF DATE() = ctod(subs(dtoc(date()),1,8)+'1') 
     c标题 = "【Wwwjxc 盘点提醒】"
     c提示 = "今天为系统盘点日期 1号，请将本月度数据录完，下班后进行盘点！"
     DO ..\progs\popwindows.prg
  ENDIF 
ENDIF 