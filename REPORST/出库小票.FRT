   ,   @                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              9ORIENTATION=0
PAPERLENGTH=600
PAPERWIDTH=900
COLOR=2
                                                                     Arial                                                         'NO:'+出入库ID1                                               Arial                                                         日期                                                          Arial                                                         alltrim(客户名称s)                                            Arial                                                         "Ebongsoft"                                                   Arial                                                         6"--------------------- 宜邦软件 ---------------------"        Arial                                                         产品名称                                                      alltrim(ckdp.产品名称)                                        Arial                                                         出库单价                                                      ckdp.出库单价                                                 Arial                                                         出库数量                                                      ckdp.出库数量                                                 Arial                                                         出库金额                                                      ckdp.出库金额                                                 Arial                                                         Arial                                                         Arial                                                         Arial                                                         Arial                                                         Arial                                                         dataenvironment                                               aTop = 115
Left = 414
Width = 520
Height = 200
DataSource = .NULL.
Name = "Dataenvironment"
                             cursor                                                        rTop = 20
Left = 10
Height = 90
Width = 115
Alias = "ckdp"
CursorSource = ..\data\ckdp.dbf
Name = "Cursor1"
      