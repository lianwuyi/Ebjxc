******�����������**
Public intnetlj
intnetlj=.F.
Do cslj
******************************
If     intnetlj=.T.
    Do lj
    Release intnetlj
Else
    Return
ENDIF
**************************
Procedure lj
*���Ӻ������
 
 
*************
Procedure cslj
Declare Integer InternetGetConnectedState In wininet.Dll ;
    Integer @lpdwFlags, ;
    Integer dwReservednReserved
If internetgetconnectedstate(7, 0) = 0
*Messagebox("δ���ᵽ������������в������ӻ�������״̬��", 0, "����")
    intnetlj=.F.
Else
    Do Case
    Case internetgetconnectedstate(7, 0) = 1
*Messagebox("�����Ѿ���ͨ!",0,"��ʾ")
        intnetlj=.T.
    Case internetgetconnectedstate(7, 0) = 2
*Messagebox("�����Ѿ���ͨ",0,"��ʾ")
        intnetlj=.T.
    Otherwise
*Messagebox("ͨ��������ͨ��")
        intnetlj=.T.
    Endcase
Endif