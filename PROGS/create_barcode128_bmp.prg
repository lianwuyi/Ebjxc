*!*	示例
*defaultcharset = GETFONT()
*barStringFont = defaultcharset
*create_barcode128_bmp('c:\whg.bmp','123456789012', 12, 2, 80, 0, .T., defaultcharset, 1)

create_barcode128_bmp('..\ebjxc\tm.bmp',tm1, 12, 2, 80, 0, .T., "楷体,8,N" , 1)
	
**************************************************************************************************
*	函数名：create_barcode128_bmp()
*	功　能：生成Code 128条形码位图
*	参　数：filename		条码位图文件名（含路径）,若无路径则在默认当前路径下建立文件
*			barString		条码字符串
*			barStringLen	条码字符串长度
*			barWidth		条码位图基本单元的宽度
*			barHeight		条码高度（不包含打印条码字串)
*			RotateDegress	图像旋转角度
*			barStringFlag	生成的条码位图中是否包含条码字符串的内容,为真时包含
*           barStringFont	写条码字符串所用字体属性(由getfont()函数返回的值)。
*           barStringFormat 写条码字符串格式(0 左对齐，1 水平居中，2 右对齐)
* 	返回值：无
*	运行环境：Windows 2000及其以上版本
*	作者：王华罡    2005-3-18 修改于2005-6-1
****************************************************************************************************
FUNCTION create_barcode128_bmp(filename as String ,barString as String ,barStringLen as integer, ;
							barWidth as integer ,barHeight as Integer , RotateDegress as Integer, ;
							barStringFlag as Boolean ,barStringFont as String,barStringFormat as Integer )
	PRIVATE bmpWidth,bmpHeight, jgString,barlen,checkchar,SingleStr,SingleStrLen
	
	DO decl					&&注册要使用的API函数

	
	SingleStr = ''
	SingleStrLen = 0
	
	*1、*	生成code 128条码符号字符串
	Getbarcode128_SingleValue(barString, barStringLen , @SingleStr , @SingleStrLen)	
	
	*2、*   生成条码位图
	
	CreateBarBMP(filename , SingleStr , SingleStrLen , barWidth , barHeight , ;
				RotateDegress , barString , barStringLen , barStringFlag , ;
				barStringFont ,barStringFormat)

	DO undecl		&&卸载注册的API函数

	
	RETURN (filename)	&&返回位图文件名
	
ENDFUNC  	&&create_barcode128_bmp()函数定义结束

**********************************************************************************
*	函数名：Getbarcode128_SingleValue()
*	功　能：由条码字符串生成对应的条码符号字符串
*	参　数：barString		条码字符串
*			barStringLen	条码字符串长度
*			SingleStr		返回的条码符号字符串
*			SingleStrLen	返回的条码符号字符串长度
***********************************************************************************
FUNCTION Getbarcode128_SingleValue(barString as String , ;
				barStringLen as Integer , ;
				SingleStr as string@, ;
				SingleStrLen as Integer@)

	PRIVATE c ,jgString , barlen , checkchar
	c = 1
	DO while c <= barStringLen
		IF not ISDIGIT(SUBSTR(barString,c,1))
			MESSAGEBOX('本函数仅支持数字字符条码')
			RETURN
		ENDIF
		c = c + 1
	ENDDO
	jgString = CHR(105)
	barlen = 1
	IF MOD(barStringLen,2) > 0
		FOR c = 1 to barStringLen - 1 step 2 
			jgString = jgString + CHR(VAL(SUBSTR(barString,c,2)))
			barlen = barlen + 1
		ENDFOR 
		jgString = jgString + CHR(103)
		barlen = barlen + 1
		jgString = jgString + CHR(VAL(SUBSTR(barString,barStringLen,1)))
		barlen = barlen + 1
	ELSE
		FOR c = 1 to barStringLen  step 2 
			jgString = jgString + CHR(VAL(SUBSTR(barString,c,2)))
			barlen = barlen + 1
		ENDFOR
	ENDIF
	checkchar = 105
	FOR c = 2 to barlen step 1
		checkchar = checkchar + ASC(SUBSTR(jgstring,c,1)) * (c - 1)
	ENDFOR
	jgstring = jgstring + CHR(MOD(checkchar,103))
	barlen = barlen + 1	
	
	SingleStr = jgstring
	SingleStrLen = 	barlen
	
ENDFUNC 	&&Getbarcode128_SingleValue()函数定义结束	

***********************************************************************************
*	函数名：CreateBarBMP()
*	功　能：根据条码符号字符串生成条码位图
*	参　数：filename		位图文件名
*			SingleStr		条码符号字符串
*			SingleStrLen	条码符号字符串长度
*			barWidth		条码基本单元的宽度
*			barHeight		条码的高度（不包含打印条码字串)
*			RotateDegress	条码位图的旋转角度
*			barString		条码字符串
*			barStringLen	条码字符串长度
*			barStringFlag	生成的条码位图中是否包含条码字符串,为真时包含
*           barStringFont	写条码字符串所用字体属性(由getfont()函数返回的值)。
*           barStringFormat 写条码字符串格式(0 左对齐，1 水平居中，2 右对齐)
*	返回值：无
*	运行环境：Windows 2000及其以上版本、Visual FoxPro 8.0
***********************************************************************************
FUNCTION CreateBarBMP(filename AS String , SingleStr AS String , ;
					SingleStrLen AS Integer , barWidth AS INTEGER , ;
					barHeight AS Integer , RotateDegress AS Integer , ;
					barString as string , barStringLen as integer , ;
					barStringFlag AS Boolean ,barStringFont as String, ;
					barStringFormat as Integer  )

	#define WHITENESS			0x00FF0062
	#define MM_TEXT             1
	#define SYSTEM_FONT         13
	#define SRCCOPY             0x00CC0020
	
	#define DT_LEFT                     0x00000000
	#define DT_CENTER                   0x00000001
	#define DT_RIGHT                    0x00000002
	
	#define OUT_DEFAULT_PRECIS          0
	#define OUT_STRING_PRECIS           1
	#define OUT_CHARACTER_PRECIS        2
	#define OUT_STROKE_PRECIS           3
	#define OUT_TT_PRECIS               4
	#define OUT_DEVICE_PRECIS           5
	#define OUT_RASTER_PRECIS           6
	#define OUT_TT_ONLY_PRECIS          7
	#define OUT_OUTLINE_PRECIS          8
	#define OUT_SCREEN_OUTLINE_PRECIS   9
	#define OUT_PS_ONLY_PRECIS          10	
	
	#define CLIP_DEFAULT_PRECIS     0
	#define CLIP_CHARACTER_PRECIS   1
	#define CLIP_STROKE_PRECIS      2
	#define CLIP_MASK               0xf


	#define DEFAULT_QUALITY         0
	#define DRAFT_QUALITY           1
	#define PROOF_QUALITY           2	
	
	PRIVATE signSet				&&定义条码符号数组
	
	DIMENSION signSet(106)
	signSet(1) = '11011001100'
	signSet(2) = '11001101100'
	signSet(3) = '11001100110'
	signSet(4) = '10010011000'
	signSet(5) = '10010001100'
	signSet(6) = '10001001100'
	signSet(7) = '10011001000'
	signSet(8) = '10011000100'
	signSet(9) = '10001100100'
	signSet(10) = '11001001000'
	signSet(11) = '11001000100'
	signSet(12) = '11000100100'
	signSet(13) = '10110011100'
	signSet(14) = '10011011100'
	signSet(15) = '10011001110'
	signSet(16) = '10111001100'
	signSet(17) = '10011101100'
	signSet(18) = '10011100110'
	signSet(19) = '11001110010'
	signSet(20) = '11001011100'
	signSet(21) = '11001001110'
	signSet(22) = '11011100100'
	signSet(23) = '11001110100'
	signSet(24) = '11101101110'
	signSet(25) = '11101001100'
	signSet(26) = '11100101100'
	signSet(27) = '11100100110'
	signSet(28) = '11101100100'
	signSet(29) = '11100110100'
	signSet(30) = '11100110010'
	signSet(31) = '11011011000'
	signSet(32) = '11011000110'
	signSet(33) = '11000110110'
	signSet(34) = '10100011000'
	signSet(35) = '10001011000'
	signSet(36) = '10001000110'
	signSet(37) = '10110001000'
	signSet(38) = '10001101000'
	signSet(39) = '10001100010'
	signSet(40) = '11010001000'
	signSet(41) = '11000101000'
	signSet(42) = '11000100010'
	signSet(43) = '10110111000'
	signSet(44) = '10110001110'
	signSet(45) = '10001101110'
	signSet(46) = '10111011000'
	signSet(47) = '10111000110'
	signSet(48) = '10001110110'
	signSet(49) = '11101110110'
	signSet(50) = '11010001110'
	signSet(51) = '11000101110'
	signSet(52) = '11011101000'
	signSet(53) = '11011100010'
	signSet(54) = '11011101110'
	signSet(55) = '11101011000'
	signSet(56) = '11101000110'
	signSet(57) = '11100010110'
	signSet(58) = '11101101000'
	signSet(59) = '11101100010'
	signSet(60) = '11100011010'
	signSet(61) = '11101111010'
	signSet(62) = '11001000010'
	signSet(63) = '11110001010'
	signSet(64) = '10100110000'
	signSet(65) = '10100001100'
	signSet(66) = '10010110000'
	signSet(67) = '10010000110'
	signSet(68) = '10000101100'
	signSet(69) = '10000100110'
	signSet(70) = '10110010000'
	signSet(71) = '10110000100'
	signSet(72) = '10011010000'
	signSet(73) = '10011000010'
	signSet(74) = '10000110100'
	signSet(75) = '10000110010'
	signSet(76) = '11000010010'
	signSet(77) = '11001010000'
	signSet(78) = '11110111010'
	signSet(79) = '11000010100'
	signSet(80) = '10001111010'
	signSet(81) = '10100111100'
	signSet(82) = '10010111100'
	signSet(83) = '10010011110'
	signSet(84) = '10111100100'
	signSet(85) = '10011110100'
	signSet(86) = '10011110010'
	signSet(87) = '11110100100'
	signSet(88) = '11110010100'
	signSet(89) = '11110010010'
	signSet(90) = '11011011110'	
	signSet(91) = '11011110110'
	signSet(92) = '11110110110'
	signSet(93) = '10101111000'
	signSet(94) = '10100011110'
	signSet(95) = '10001011110'
	signSet(96) = '10111101000'
	signSet(97) = '10111100010'
	signSet(98) = '11110101000'
	signSet(99) = '11110100010'
	signSet(100) = '10111011110'
	signSet(101) = '10111101110'
	signSet(102) = '11101011110'
	signSet(103) = '11110101110'
	signSet(104) = '11010000100'
	signSet(105) = '11010010000'
	signSet(106) = '11010011100'
	
	PRIVATE bmpWidth,bmpHeight	&&定义位图宽度和高度变量

	
	&&计算位图的宽度和高度，条码字符串的高度固定为32
	
	if	barStringFlag = .T.
		PRIVATE zt,ztname,ztsize,ztflag,nHeight,nWidth,fnWeight, ;
				Italic,Underlin,StrikeOut,chrset,PitchAndFamily,hfont
		zt = ',' + barStringFont + ','
		ztname = STREXTRACT(zt,",",",",1)
		
		ztsize = STREXTRACT(zt,",",",",2)
		ztflag = STREXTRACT(zt,",",",",3)
		nHeight = FONTMETRIC( 1,ztname,VAL(ztsize),ztflag)
		nWidth = FONTMETRIC( 7,ztname,VAL(ztsize),ztflag)
		fnWeight = FONTMETRIC(8,ztname,VAL(ztsize),ztflag)
		Italic = FONTMETRIC(9,ztname,VAL(ztsize),ztflag)
		Underlin = FONTMETRIC(10,ztname,VAL(ztsize),ztflag)
		StrikeOut = FONTMETRIC(11,ztname,VAL(ztsize),ztflag)
		chrset =  FONTMETRIC(17,ztname,VAL(ztsize),ztflag)
		PitchAndFamily =  FONTMETRIC(16,ztname,VAL(ztsize),ztflag)
		
		hfont = CreateFont(nHeight,nWidth,0,0,fnWeight,Italic,Underlin,StrikeOut,chrset, ;
					OUT_DEFAULT_PRECIS,CLIP_DEFAULT_PRECIS ,DEFAULT_QUALITY ,PitchAndFamily, ;
					ztname)
					
		PRIVATE fonthdc,hfntdc , hfnt , hfntBitmap ,fntWidth , fntHeight
		fonthdc = GetWindowDC( 0 )			&&得到系统设备描述表
		hfntdc = CreateCompatibleDC (fonthdc)	&&创建与设备兼容的内存设备描述表
		SelectObject(hfntdc,hfont)			&&向设备描述表选入指定的字体
		PRIVATE lpSize
		lpSize = SPACE(8)
		GetTextExtentPoint(hfntdc , barString , barStringLen , @lpSize)
		fntWidth = StrToInteger(LEFT(lpSize,4),.T.)			&&得到字符串的宽度
		fntHeight = StrToInteger(SUBSTR(lpSize,5,4),.T.)		&&得到字符串的高度		
				
		bmpWidth = ((SingleStrLen * 11) + 13) * barWidth
		IF bmpWidth < fntWidth
			bmpWidth = fntWidth
		ENDIF
		
	
		bmpHeight = barHeight + nHeight + 5
		
		DeleteDC( hfntdc )	&&释放对象
		DeleteDC( fonthdc )
	ELSE
		bmpWidth = ((SingleStrLen * 11) + 13) * barWidth
		bmpHeight = barHeight
	ENDIF
	
	PRIVATE hdc , hVdc , hBitmap
	hdc = GetWindowDC( 0 )			&&得到系统设备描述表
	hVdc = CreateCompatibleDC (hdc)	&&创建与设备兼容的内存设备描述表
	
	&&创建与设备兼容的位图
	hBitmap = CreateCompatibleBitmap(hVdc, bmpWidth, bmpHeight)
	SelectObject (hVdc, hBitmap)	&&把创建的位图选入指定的内存设备描述表
	PatBlt(hVdc , 0 , 0 , bmpWidth , bmpHeight , WHITENESS)	&&输出位全部置1
	
	SetBkColor(hVdc,0xFFFFFF)	&&设置背景色为白色
	SetTextColor(hVdc,0)		&&设置文本色为黑色
	SetMapMode(hVdc,MM_TEXT)	&&设置图像模式为文本模式
	
	&&画条形码
	PRIVATE pointstr , tempstr , d , e
	pointstr = SPACE(8)
	PRIVATE lines,c
	lines = 0
	FOR d = 1 to SingleStrLen step 1
		tempstr = signSet(ASC(SUBSTR(SingleStr,d,1)) + 1)
		
		FOR c = 1 to 11 step 1
			IF SUBSTR(tempstr,c,1) == '0'
				FOR e = 1 to barWidth step 1
				 	lines = lines + 1
				ENDFOR
			ELSE
				FOR e = 1 to barWidth step 1
				MoveToEx(hVdc,lines,0,@pointstr)
				LineTo(hVdc,lines,barHeight)
			 	lines = lines + 1
				ENDFOR
			ENDIF
		ENDFOR
	ENDFOR

	tempstr = '1100011101011'

	FOR c = 1 to 13 step 1
		IF SUBSTR(tempstr,c,1) == '0'
			FOR e = 1 to barWidth step 1

			 	lines = lines + 1
			ENDFOR
		ELSE
			FOR e = 1 to barWidth step 1
				MoveToEx(hVdc,lines,0,@pointstr)
				LineTo(hVdc,lines,barHeight)
			 	lines = lines + 1
			ENDFOR
		ENDIF
	ENDFOR	&&画条形码结束
	
	&&判断是否需要写条码字符串
	IF barStringFlag = .T.
*!*			PRIVATE hfntdc , hfnt , hfntBitmap ,fntWidth , fntHeight
*!*			hfntdc = CreateCompatibleDC (hdc)	&&创建与设备兼容的内存设备描述表
*!*			hfnt = GetStockObject(SYSTEM_FONT)	&&得到系统字体
*!*			SelectObject(hfntdc,hfnt)			&&向设备描述表选入指定的字体
*!*			PRIVATE lpSize
*!*			lpSize = SPACE(8)
*!*			GetTextExtentPoint(hfntdc , barString , barStringLen , @lpSize)
*!*			fntWidth = StrToInteger(LEFT(lpSize,4),.T.)			&&得到字符串的宽度
*!*			fntHeight = StrToInteger(SUBSTR(lpSize,5,4),.T.)		&&得到字符串的高度
*!*			
*!*			&&创建与设备兼容的位图
*!*			hfntBitmap = CreateCompatibleBitmap(hfntdc , fntWidth , fntHeight)
*!*			SelectObject (hfntdc, hfntBitmap)	&&把创建的位图选入指定的内存设备描述表
*!*			PatBlt(hfntdc , 0 , 0 , fntWidth , fntHeight , WHITENESS )	&&输出位全部置1
*!*			
*!*			SetBkColor(hfntdc , 0xFFFFFF )	&&设置背景色为白色
*!*			SetTextColor(hfntdc , 0 )		&&设置文本色为黑色
*!*			SetMapMode(hfntdc , MM_TEXT )	&&设置图像模式为文本模式
*!*			TextOut(hfntdc,0,0,barString , barStringLen )

*!*			StretchBlt(hVdc , 5 , barHeight + 1 , bmpWidth - 10 , 31 , hfntdc , 0 , 0 , fntWidth , fntHeight , SRCCOPY )
		SelectObject(hVdc,hfont)
		PRIVATE rect 
		rect = CHR(0)+CHR(0)+CHR(0)+CHR(0)+IntegerTOStr((barHeight + 2),4) +IntegerTOStr(bmpWidth,4)+IntegerTOStr(bmpHeight,4)		
		DrawText(hVdc,barString,barStringLen,@rect,barStringFormat)
				
*!*			DeleteObject (hfntBitmap)	&&释放对象
*!*			DeleteDC (hfntdc)

	ENDIF 
	
	&&判断是否需要旋转位图
	PRIVATE hresults , hnewdc
	hnewdc = 0
	hresults = 0
	IF RotateDegress <> 0
		RotateBitmap( hVdc , hBitmap , RotateDegress , 0xFFFFFF , @hnewdc , @hresults )	&&旋转位图
		DeleteObject (hBitmap)	&&释放对象
		DeleteDC (hVdc)
		hBitmap = hresults
		hVdc = hnewdc
	ENDIF 
	
	PRIVATE pbi
	pbi = CreateBitmapInfoStruct( hBitmap )

	CreateBMPFile( filename , pbi , hBitmap , hVdc )
	
	DeleteObject( hBitmap )	&&释放对象
	DeleteDC( hVdc )
	DeleteDC( hdc )
	#undef WHITENESS			
	#undef MM_TEXT             
	#undef SYSTEM_FONT         
	#undef SRCCOPY   
	
	#undefine DT_LEFT                  
	#undefine DT_CENTER               
	#undefine DT_RIGHT   
	
	#undefine OUT_DEFAULT_PRECIS        
	#undefine OUT_STRING_PRECIS          
	#undefine OUT_CHARACTER_PRECIS       
	#undefine OUT_STROKE_PRECIS          
	#undefine OUT_TT_PRECIS              
	#undefine OUT_DEVICE_PRECIS          
	#undefine OUT_RASTER_PRECIS          
	#undefine OUT_TT_ONLY_PRECIS         
	#undefine OUT_OUTLINE_PRECIS         
	#undefine OUT_SCREEN_OUTLINE_PRECIS  
	#undefine OUT_PS_ONLY_PRECIS         	                         	
	#undefine CLIP_DEFAULT_PRECIS     
	#undefine CLIP_CHARACTER_PRECIS   
	#undefine CLIP_STROKE_PRECIS      
	#undefine CLIP_MASK

	#undefine DEFAULT_QUALITY         0
	#undefine DRAFT_QUALITY           1
	#undefine PROOF_QUALITY           2						
ENDFUNC 		&&CreateBarBMP()函数定义结束

***********************************************************************************
*	函数名：RotateBitmap()
*	功　能：以指定角度旋转位图
*	参　数：Srchdc			源设备描述表句柄
*			hBitmap			位图句柄
*			RotateDegress	旋转角度
*			clrBack			无图像数据区域的背景色
*			hnewdc			返回新的设备描述表句柄
*			hresults		返回新的位图句柄
*	运行环境：Windows 2000及其以上版本、Visual FoxPro 8.0
***********************************************************************************
FUNCTION RotateBitmap(Srchdc as integer , hBitmap as Integer , RotateDegress as Integer , ;
					clrBack as Integer , hnewdc as Integer@ , hresults as integer@ )
	
	#define PI		3.141592653589793
	#define GM_ADVANCED         2
	#define SRCCOPY             0x00CC0020 
	#define PATCOPY             0x00F00021 
	#define MM_ISOTROPIC        7
	#define WHITENESS			0x00FF0062
	#define MM_ANISOTROPIC      8
	PRIVATE Desthdc
	Desthdc = CreateCompatibleDC( Srchdc )

	PRIVATE bmp
	bmp = SPACE(24)
	IF GetObjectAPI(hBitmap,24,@bmp) = 0
		MESSAGEBOX('GetObject调用发生错误！')
		RETURN
	ENDIF
	PRIVATE radians , cosine , sine , bmWidth , bmHeight,w,h
	PRIVATE lpPoint,x,y,SrcX,SrcY,A_x,A_y,B_x,B_y,C_x,C_y
	PRIVATE  minx , miny , maxx , maxy ,xcenter ,ycenter
*!*		4．图像旋转
*!*	　　图像旋转是指把定义的图像绕某一点以逆时针或顺时针方向旋转一定的角度，
*!*		通常是指绕图像的中心以逆时针方向旋转。
*!*	　　假设图像的左上角为（left, top),右下角为（right, bottom)，
*!*		则图像上任意点（x0, y0）绕其中心（xcenter, ycenter)逆时针旋转angle角度后，
*!*		新的坐标位置（x′, y′）的计算公式为：
*!*	xcenter = (right － left ＋ 1) ／ 2 ＋ left;
*!*	ycenter = (bottom － top ＋ 1) ／ 2 ＋ top;
*!*	x′ = (x0 － xcenter) cosθ － (y0 － ycenter) sinθ ＋ xcenter;
*!*	y′ = (x0 － xcenter) sinθ ＋ (y0 － ycenter) cosθ ＋ ycenter
	radians = ( RotateDegress * PI / 180 )

	cosine = cos(radians)
	sine = sin(radians)

	bmWidth = StrToInteger(SUBSTR(bmp,5,4),.T.)
	bmHeight = StrToInteger(SUBSTR(bmp,9,4),.T.)

	xcenter = INT((bmWidth + 1) / 2 )
	ycenter = INT((bmHeight + 1) / 2 )
	
	A_x = INT((0 - xcenter) * cosine - (0 - ycenter) * sine  + xcenter)
	A_y = INT((0 - xcenter) * sine + (0 - ycenter) *  cosine + ycenter)
	B_x = INT((bmWidth - 1 - xcenter) * cosine - ( 0 - 1 - ycenter) * sine + xcenter)
	B_y = INT((bmWidth - 1 - xcenter) * sine + (0 - ycenter) *  cosine + ycenter)
	C_x = INT((0 - xcenter) *  cosine - (bmHeight - 1 - ycenter) * sine + xcenter)
	C_y = INT((0 - xcenter) * sine + (bmHeight - 1 - ycenter) * cosine + ycenter)
	D_x = INT((bmWidth - 1 - xcenter) *  cosine - (bmHeight - 1 - ycenter) * sine + xcenter)
	D_y = INT((bmWidth - xcenter) * sine + (bmHeight -1 - ycenter) * cosine + ycenter)

	
	w = MAX(ABS(D_x-A_x),ABS(C_x-B_x))+5
	h = MAX(ABS(D_Y-A_Y),ABS(C_Y-B_Y))+5
	xcenter = INT((w - bmWidth + 1) / 2 )
	ycenter = INT((h - bmHeight + 1) / 2 )
	A_x = A_x + xcenter
	A_y = A_y + ycenter
	B_x = B_x + xcenter
	B_y = B_y + ycenter
	C_x = C_x + xcenter
	C_y = C_y + ycenter
	D_x = D_x + xcenter
	D_y = D_y + ycenter	
	
	IF MOD(RotateDegress,360) <> 0 
		IF MOD(RotateDegress,270) = 0 

			A_x = 0
			A_y = bmWidth 
			B_x = 0
			B_y = 0
			C_x = bmHeight 
			C_y = bmWidth 
			D_x = 0
			D_y = bmHeight 
			w = bmHeight
			h = bmWidth
			
		ELSE 
			IF MOD(RotateDegress,180) = 0 
				w = bmWidth 
				h = bmHeight
				A_x = bmWidth 
				A_y = bmHeight 
				B_x = 0
				B_y = bmHeight 
				C_x = bmWidth 
				C_y = 0
				D_x = 0
				D_y = 0				
			ELSE 
				IF MOD(RotateDegress,90) = 0 
					
					A_x = bmHeight 
					A_y = 0
					B_x = bmHeight 
					B_y = bmWidth 
					C_x = 0
					C_y = 0
					D_x = 0
					D_y = bmWidth  
					w = bmHeight
					h = bmWidth
				ENDIF 
			ENDIF  
		ENDIF 
	ELSE 
	 	w = bmWidth 
		h = bmHeight
	 	hbmResult = CreateCompatibleBitmap ( Srchdc , w , h )	
		SelectObject( Desthdc , hbmResult )
		BitBlt(Desthdc , 0 , 0 , w , h ,Srchdc , 0 , 0 , SRCCOPY)
		
		hnewdc = Desthdc
		hresults = hbmResult

		RETURN 
	ENDIF 

	PRIVATE hbmResult , hbrBack
	hbmResult = CreateCompatibleBitmap ( Srchdc , w , h )	
	SelectObject( Desthdc , hbmResult )
	hbrBack = CreateSolidBrush(clrBack)
	SelectObject(Desthdc,hbrBack)
	PatBlt(Desthdc , 0 , 0 , w , h , PATCOPY )	&&把刷子图案拷贝到Desthdc
	DeleteObject( hbrBack )	&&释放对象
	SetGraphicsMode( Desthdc , GM_ADVANCED )
	
	lpPoint = IntegerToStr(A_x ,4) + IntegerToStr(A_y,4) + IntegerToStr(B_x ,4) + ;
			IntegerToStr(B_y  ,4) + IntegerToStr(C_x,4) + IntegerToStr(C_y,4)

	PlgBlt( Desthdc , @lpPoint , Srchdc , 0 , 0 , bmWidth , bmHeight ,  0 , 0 , 0 )

	hnewdc = Desthdc
	hresults = hbmResult
	#undef 	PI
	#undef  GM_ADVANCED
	#undef  SRCCOPY
	#undef  PATCOPY

ENDFUNC 		&&RotateBitmap()函数定义结束


************************************************************************************
*	函数名：CreateBitmapInfoStruct()
*	功　能：依据位图句柄创建BITMAPINFO结构
*	参　数：hBmp	位图句柄
*	返回值：返回BITMAPINFO结构指针
*	运行环境：Windows 2000及其以上版本、Visual FoxPro 8.0
************************************************************************************
FUNCTION CreateBitmapInfoStruct(hBmp as Long)
	PRIVATE bmp,pbmi,cClrBits,cColors
	
	#define BI_RGB        0
	bmp = SPACE(24)
	pbmi = 0
	IF GetObjectAPI(hBmp,24,@bmp) = 0
		MESSAGEBOX('GetObject调用发生错误！')
		RETURN
	ENDIF
	cClrBits = StrToInteger(SUBSTR(bmp,17,2),.T.) * StrToInteger(SUBSTR(bmp,19,2),.T.)
	
	DO case
		CASE cClrBits = 1
			cClrBits = 1
			cColors = 2
		CASE cClrBits <= 4
			cClrBits = 4
			cColors = 16
		CASE cClrBits <= 8
			cClrBits = 8
			cColors = 256
		CASE cClrBits <= 16
			cClrBits = 16
			cColors = 0
		CASE cClrBits <= 24
			cClrBits = 24
			cColors = 0			
		OTHERWISE
			cClrBits = 24
			cColors = 0	
	ENDCASE
	
	IF (cClrBits < 16)
		GetMemPoint(64,(40 + 4 * cColors),@pbmi)
	ELSE
		GetMemPoint(64,40,@pbmi) 
	ENDIF
	
	SYS(2600 , pbmi , 4 , IntegerToStr( 40 , 4 ))		&&写BITMAPINFOHEADER结构长度
	SYS(2600 , pbmi + 4 , 4 , SUBSTR( bmp , 5 , 4 ))	&&写biWidth
	SYS(2600 , pbmi + 8 , 4 , SUBSTR( bmp , 9 , 4 ))	&&写biHeight
	SYS(2600 , pbmi + 12 , 2 , SUBSTR( bmp , 17 , 2 ))	&&写颜色位面的计数值
	SYS(2600 , pbmi + 14 , 2 , IntegerToStr(cClrBits , 2 ))	&&指定每个象素的颜色的位数
	SYS(2600 , pbmi + 16 , 4 , IntegerToStr( BI_RGB , 4 ))	&&指定压缩类型,BI_RGB表示不压缩
	SYS(2600 , pbmi + 20 , 4 , ;
			IntegerToStr( INT((StrToInteger(SUBSTR( bmp , 5 , 4 ) , .T. ) * cClrBits + 31 ) / 32 ) * ;
							4  * StrToInteger(SUBSTR(bmp,9,4),.T.) ,4))	&&图像尺寸
							
	SYS(2600,pbmi + 36,4,CHR(0)+CHR(0)+CHR(0)+CHR(0)) &&设置所有颜色都是重要的。
	RETURN pbmi
ENDFUNC			&&CreateBitmapInfoStruct()函数定义结束



****************************************************************************************
*	函数名：CreateBMPFile()
*	功　能：创建位图文件
*	参　数：BmpFileName		位图文件名
*			pbi				指向BITMAPINFO结构的指针
*			hBMP			位图句柄
*			hDC				设备描述表句柄
*	返回值：返回位图文件名字符串
****************************************************************************************
FUNCTION  CreateBMPFile(BmpFileName as string ,pbi as Long ,hBMP as Long , hDC as Long)

	PRIVATE hf ,hdr,pbih,ImageSize,lpBits,hp,bmpinfoSize,bmp,cColors,cClrBits
	#define DIB_RGB_COLORS      0
	pbih = pbi
	lpBits = 0
	hdr = ''
	GetMemPoint(64,StrToInteger(sys(2600,pbih + 20,4),.T.),@lpBits)
	if lpBits = 0 
		messagebox('内存分配错误!')
		RETURN ''
		
	endif
	cClrBits = StrToInteger(sys(2600,pbih + 14,2),.T.)
	IF (cClrBits < 16)
		cColors = 2^cClrBits
	ELSE
		cColors = 0
	endif
	GetDIBits(hdc,hBMP,0,StrToInteger(sys(2600,pbih + 8,4),.T.) ,0,pbi,DIB_RGB_COLORS)	
	GetDIBits(hdc,hBMP,0,StrToInteger(sys(2600,pbih + 8,4),.T.) ,lpBits,pbi,DIB_RGB_COLORS)		&&DIB_RGB_COLORS
	hdr = 'BM' + IntegerToStr(14 + 40 + cColors * 4 + StrToInteger(sys(2600,pbih + 20,4)),4) ;
		 + chr(0) + chr(0) + chr(0) + chr(0) + IntegerToStr(14 + 40 + cColors * 4,4)
		 
	hf = fCreate(BmpFileName,0)
	fwrite(hf,hdr,14)
	bmpinfoSize = 40 + cColors * 4
	fwrite(hf,sys(2600,pbih,bmpinfoSize),bmpinfoSize)
	ImageSize = StrToInteger(sys(2600,pbih + 20,4),.T.)
	

	hp = lpBits
	do while ImageSize > 250
		fwrite(hf,sys(2600,hp,250),250)
		ImageSize = ImageSize - 250
		hp = hp + 250
	enddo
	fwrite(hf,sys(2600,hp,ImageSize),ImageSize)
	fclose(hf)
	
	DeleteMemPoint(lpBits)
	DeleteMemPoint(pbih)
	
ENDFUNC 


***********************************************************************************
*	过程名：decl
*	功　能：注册相关API函数
*	参　数：无
*	返回值：无
*	运行环境：Windows 2000及其以上版本、Visual FoxPro 8.0
*	备　注：运行该过程后，在不使用时应及时运行undecl过程，卸载注册的API函数
************************************************************************************
PROCEDURE decl
	DECLARE  INTEGER GetWindowDC IN user32 INTEGER hwnd
	DECLARE  INTEGER GetLastError IN kernel32.dll
	DECLARE  INTEGER CreateCompatibleDC IN gdi32.dll INTEGER hdc
	DECLARE  INTEGER DeleteDC IN gdi32.dll INTEGER hdc
	DECLARE  INTEGER ReleaseDC IN user32.dll INTEGER hwnd, INTEGER hdc
	DECLARE  INTEGER CreateCompatibleBitmap IN gdi32.dll ;
		INTEGER hdc,;
		INTEGER nWidth,;
		INTEGER nHeight
	DECLARE  INTEGER SelectObject IN gdi32.dll INTEGER hdc, INTEGER hObject
	DECLARE  INTEGER DeleteObject IN gdi32.dll INTEGER hObject
	DECLARE  INTEGER BitBlt IN gdi32.dll ;
			INTEGER hDestDC, ;
			INTEGER x, INTEGER y, ;
			INTEGER nWidth, INTEGER nHeight, ;
			INTEGER hSrcDC, ;
			INTEGER xSrc, INTEGER ySrc, ;
			INTEGER dwRop

	DECLARE long TextOut in GDI32.dll long hdc , long x, long y , ;
			 string lpString, long nCount
	DECLARE long SetBkMode in GDI32.dll long hdc, long nBkMode
	DECLARE long SetBkColor  in gdi32.dll long hdc, long crColor
	DECLARE SHORT GetDIBits in gdi32.dll long hdc,long hbmp,integer startscan, ;
			integer scanLines,long lpBits,long lpbi,integer wUsage
	DECLARE long CreateDIBitmap in gdi32.dll long hdc,string@ lpbmih,long fdwInit, ;
			string@ lpbInit,string@ lpbmi,integer fuUsage
	DECLARE integer PatBlt in gdi32.dll long hdc,integer x,integer y, ;
			integer nwidth,integer nHeight,long dwrop
	DECLARE INTEGER MoveToEx in gdi32.dll long hdc,integer x,integer y,string@ lpPoint
	DECLARE integer LineTo in gdi32.dll long hdc,integer x,integer y
	DECLARE integer SetTextColor in gdi32.dll long hdc,long crcolor
	DECLARE integer GetObject in gdi32.dll as GetObjectAPI integer hbmp, ;
			integer BITBMPSize,string@ bmp
	DECLARE long GetStockObject in gdi32.dll long nIndex
	DECLARE integer GetTextExtentPoint in gdi32.dll long hdc,string szstring , ;
			LONG stringLen,string@ lpSize
	DECLARE LONG GetCharacterPlacement in gdi32.dll long  hdc,string@ lpString, ;
			short stringLen,short MaxExtent,string@ results,long dwFlags
	DECLARE long GetTextMetrics in gdi32.dll long hdc,string@ lpMetrics
	DECLARE integer StretchBlt in gdi32.dll long Desthdc,long nDestx,long nDesty, ;
			long nDestWidth,long nDestHeight,long Srchdc,long Srcx,long Srcy, ;
			long SrcWidth,long SrcHeight,long rop
	DECLARE INTEGER SetMapMode IN gdi32.dll long hdc ,long MapMode
	DECLARE integer CreateSolidBrush in gdi32.dll long clrBack
	DECLARE integer SetGraphicsMode in gdi32.dll long hdc,long iMode
	DECLARE integer SetWorldTransform in gdi32.dll long hdc,string@ xform
	DECLARE integer PlgBlt in gdi32.dll long hdc,string@ lppoint,long hdcSrc, ;
						long nxSrc,long nYSrc,long nWidth,long nHeight, ;
						long hbmMask,long xMask,long yMask
	DECLARE long SetPixel in gdi32.dll long hdc,long x,long y,long crcolor
	DECLARE long GetPixel in gdi32.dll long hdc,long x,long y
	DECLARE INTEGER DrawText IN user32.DLL INTEGER HDC,STRING,INTEGER STRLEN,STRING@ RECT,INTEGER wFormat
	DECLARE integer CreateFont in GDI32.dll integer,integer,integer,integer,integer, ;
			Long,long,long,long,long,long,long,long,string
ENDPROC			&&decl过程定义结束

*******************************************************************************
*	过程名：undecl
*	功　能：卸载注册的API函数
*	参　数：无
*	返回值：无
*	运行环境：Windows 2000及其以上版本、Visual FoxPro 8.0
*	备　注：与decl过程配套使用
************************************************************************************
PROCEDURE   undecl
	CLEAR DLLS 'GetWindowDC'
	CLEAR DLLS 'GetLastError'
	CLEAR DLLS 'CreateCompatibleDC'
	CLEAR DLLS 'DeleteDC'
	CLEAR DLLS 'ReleaseDC'
	CLEAR DLLS 'CreateCompatibleBitmap'
	CLEAR DLLS 'SelectObject'
	CLEAR DLLS 'DeleteObject'
	CLEAR DLLS 'BitBlt'
	CLEAR DLLS 'TextOut'
	CLEAR DLLS 'SetBkMode'
	CLEAR DLLS 'GetDIBits'
	CLEAR DLLS 'CreateDIBitmap'
	CLEAR DLLS 'PatBlt'
	CLEAR DLLS 'MoveToEx'
	CLEAR DLLS 'LineTo'
	CLEAR DLLS 'SetTextColor'
	CLEAR DLLS 'GetStockObject'
	CLEAR DLLS 'GetTextExtentPoint'	
	CLEAR DLLS 'GetCharacterPlacement'
	CLEAR DLLS 'GetTextMetrics'
	CLEAR DLLS 'SetMapMode'
	CLEAR DLLS 'CreateSolidBrush'
	CLEAR DLLS 'SetGraphicsMode'
	CLEAR DLLS 'SetWorldTransform'
	CLEAR DLLS 'PlgBlt'
	CLEAR DLLS 'SetPixel'
	CLEAR DLLS 'GetPixel'
	CLEAR DLLS 'DrawText'
	CLEAR DLLS 'CreateFont'
ENDPROC 	&&undecl过程定义结束	

*******************************************************************************
*	过程名：ConstantDefine
*	功　能：定义API函数中使用的常量
*	参　数：无
*	备　注：使用本过程后应使用unconstantdefined过程清除已定义的常量
*******************************************************************************
PROCEDURE ConstantDefine

	#define PI		3.1415926
	
	#define MM_TEXT             1
	#define MM_LOMETRIC         2
	#define MM_HIMETRIC         3
	#define MM_LOENGLISH        4
	#define MM_HIENGLISH        5
	#define MM_TWIPS            6
	#define MM_ISOTROPIC        7
	#define MM_ANISOTROPIC      8
	
	#define SRCCOPY             0x00CC0020 
	#define SRCPAINT            0x00EE0086 
	#define SRCAND              0x008800C6 
	#define SRCINVERT           0x00660046 
	#define SRCERASE            0x00440328 
	#define NOTSRCCOPY          0x00330008 
	#define NOTSRCERASE         0x001100A6 
	#define MERGECOPY           0x00C000CA 
	#define MERGEPAINT          0x00BB0226 
	#define PATCOPY             0x00F00021 
	#define PATPAINT            0x00FB0A09 
	#define PATINVERT           0x005A0049 
	#define DSTINVERT           0x00550009 
	#define BLACKNESS           0x00000042
	#define WHITENESS			0x00FF0062
	
	#define WHITE_BRUSH         0
	#define LTGRAY_BRUSH        1
	#define GRAY_BRUSH          2
	#define DKGRAY_BRUSH        3
	#define BLACK_BRUSH         4
	#define NULL_BRUSH          5
	#define HOLLOW_BRUSH        5
	#define WHITE_PEN           6
	#define BLACK_PEN           7
	#define NULL_PEN            8
	#define OEM_FIXED_FONT      10
	#define ANSI_FIXED_FONT     11
	#define ANSI_VAR_FONT       12
	#define SYSTEM_FONT         13
	#define DEVICE_DEFAULT_FONT 14
	#define DEFAULT_PALETTE     15
	#define SYSTEM_FIXED_FONT   16
	
	&&背景模式定义
	#define TRANSPARENT         1
	#define OPAQUE              2
	#define BKMODE_LAST         2

	&&图像模式定义
	#define GM_COMPATIBLE       1
	#define GM_ADVANCED         2
	#define GM_LAST             2	
	
	#define GCP_MAXEXTENT      0x00100000
&&此处添加代码	

ENDPROC 	&&ConstantDefine过程定义结束


*******************************************************************************
*	过程名：unConstantDefine
*	功　能：定义API函数中使用的常量
*	参　数：无
*	备　注：使用本过程后应使用unConstantDefined过程清除已定义的常量
*******************************************************************************
PROCEDURE unConstantDefine

	#undef MM_TEXT            
	#undef MM_LOMETRIC         
	#undef MM_HIMETRIC         
	#undef MM_LOENGLISH        
	#undef MM_HIENGLISH        
	#undef MM_TWIPS            
	#undef MM_ISOTROPIC        
	#undef MM_ANISOTROPIC      
	
	#undef SRCCOPY              
	#undef SRCPAINT             
	#undef SRCAND              
	#undef SRCINVERT           
	#undef SRCERASE            
	#undef NOTSRCCOPY          
	#undef NOTSRCERASE         
	#undef MERGECOPY          
	#undef MERGEPAINT          
	#undef PATCOPY             
	#undef PATPAINT            
	#undef PATINVERT           
	#undef DSTINVERT           
	#undef BLACKNESS           
	#undef WHITENESS           
	
	#undef WHITE_BRUSH         
	#undef LTGRAY_BRUSH        
	#undef GRAY_BRUSH          
	#undef DKGRAY_BRUSH        
	#undef BLACK_BRUSH         
	#undef NULL_BRUSH          
	#undef HOLLOW_BRUSH        
	#undef WHITE_PEN           
	#undef BLACK_PEN           
	#undef NULL_PEN            
	#undef OEM_FIXED_FONT      
	#undef ANSI_FIXED_FONT     
	#undef ANSI_VAR_FONT       
	#undef SYSTEM_FONT         
	#undef DEVICE_DEFAULT_FONT 
	#undef DEFAULT_PALETTE     
	#undef SYSTEM_FIXED_FONT 
	
	&&背景模式定义
	#undef TRANSPARENT         
	#undef OPAQUE              
	#undef BKMODE_LAST         

	&&图像模式定义
	#undef GM_COMPATIBLE       
	#undef GM_ADVANCED         
	#undef GM_LAST  
	
	#undef GCP_MAXEXTENT          	        
&&此处添加代码	

ENDPROC 	&&unConstantDefine过程定义结束


*==============================================================================
*　　　　　Windows API 函数中使用的结构或结构指针转换函数库
*					
*						程序设计：王华罡
*					
*	测试时运行环境：Windows 2000,Visual FoxPro 8.0
*	完成日期：2004年10月26日
*==============================================================================
*!*	*!*	示例：
*!*	*!*	１、整数转换为字符串
*!*	StrValue = IntegerToStr(-700,4) 	&&可以指定长度转换
*!*	?StrValue
*!*	*!*	２、字符串转换为整数
*!*	IntegerValue = StrToInteger(StrValue,.T.)	&&可以指定该字符串是有符号数(.T.),还是无符号数(.F.)
*!*	?IntegerValue
*!*	*!*	３、双精度浮点数转换为字符串
*!*	StrValue = SPACE(8)
*!*	StrValue = DoubleValueToStr(-456.123456789)
*!*	?StrValue
*!*	*!*	４、8字节字符串转换为双精度浮点数
*!*	DoubleValue = StrToDoubleValue(StrValue)
*!*	?DoubleValue
*!*	*!*	５、单精度浮点数转换为字符串
*!*	StrValue = SPACE(4)
*!*	StrValue = SingleValueToStr(-125.54321)
*!*	?StrValue
*!*	*!*	６、４字节字符串转换为单精度浮点数
*!*	SingleValue = StrToFloatValue(StrValue)
*!*	?SingleValue
*!*	*!*	７、获取指定长度内存，返回内存指针,并赋值
*!*	LOCAL PointAddress AS Integer 
*!*	GetMemPoint(64,125,@PointAddress)		&&分配１２５个字节的内存，并初始化为0
*!*	?SYS(2600,PointAddress,12,'Hello World!') &&向该内存赋值
*!*	?SYS(2600,PointAddress+12,28,' my first memory point test.')		&&向该内存指定地址赋值
*!*	?SYS(2600,PointAddress,40)		&&读取内存内容
*!*	*!*	８、释放指定内存指针
*!*	DeleteMemPoint(PointAddress)


*================================================================
* 函数: IntegerToStr
* 状态: Public
* 用途: 转换整型值到字符串
* 参数: Integervalue   十进制值
*       StrLen  转换后的字节数 ,StrLen应不大于8
* 返回值: 十六进制值
* 运行环境：
*================================================================
FUNCTION IntegerToStr(Integervalue AS Integer ,StrLen AS Integer )
	LOCAL lnplaces, ;
	lchex, ;
	lcout, ;
	lni, ;
	lnCurrDecimal
	
	lcHex=''
	lnDecimal = Integervalue
	lnplaces = StrLen	&&检测实际传递参数个数，确定返回值字节数,缺省为4个字节
	IF lnPlaces > 8
		MESSAGEBOX('要求的字节数大太，不能超过8个字节，即64位值。',0,'错误信息')
		RETURN ''
	ENDIF
	
	IF ABS(lnDecimal) >= 2^(lnplaces*8)			&&如果实际值的位数大于给定的位数，则返回空串
	   RETURN lcHex
	ENDIF
	
	lnCurrDecimal=SET("Decimals" )
	SET DECIMALS TO 17
	IF lnDecimal < 0
		IF lnplaces > 4
			lnDecimal = 2^((lnplaces-4)*8) + (2^32 + lnDecimal)
		ELSE
			lnDecimal = 2^(lnplaces*8) + lnDecimal	&& 得到 lnDecimal 的补数
		ENDIF
	ENDIF
			
	FOR lni = lnplaces TO 1 STEP -1
		lnexponent = 256 ^ (lni - 1)
		lntemp = INT(lndecimal/lnexponent)
		lchex = lchex + CHR(lntemp)
		lndecimal = lndecimal - lntemp * lnexponent
	NEXT lni
	
		
	lcout = ''
	FOR lni = 1 TO lnplaces
		lcout = lcout + SUBSTR(lchex, lnplaces - lni + 1, 1)
	NEXT lni
	SET DECIMALS TO lnCurrDecimal
	RETURN lcout
ENDFUNC &&IntegerToStr函数结束

*==============================================================================================
* 函数: StrToInteger
* 状态: Public
* 用途: 转换字符串到整型值
* 参数: tcValue - 要转换的值
*       tlSigned - .T. 若是有符号的值
* 返回值: 整型的值
*运行环境：适应于INTEL CPU　的　低字在前高字在后　的操作系统
*==============================================================================================
FUNCTION  StrToInteger(StrValue AS String,SignedFlag AS Boolean )

	local lnDecimal,lnLen,lnI,lnMax
	
	lnDecimal = 0
	lnLen=0
	lnLen = len(StrValue)
	IF lnLen > 8
		MESSAGEBOX('要求的字节数大太，不能超过8个字节，即64位值。',0,'错误信息')
	RETURN ''
	ENDIF
	for lnI = 1 to lnLen
		lnDecimal = lnDecimal + asc(substr(StrValue, lnI, 1)) * 256^ (lnI-1)		&&以　256 进制方式计算无符号值
	next lnI
	
	if SignedFlag and BITTEST(lnDecimal,((lnLen * 8) - 1))		&&判断是否是有符号值,并且是负值
		IF lnLen > 4
			lnDecimal = (lnDecimal - 2^32) - 2^((lnLen-4)*8)	&& 得到 lnDecimal 的补数
		ELSE
			lnDecimal = lnDecimal - 2^(lnLen*8)
		ENDIF
	ENDIF
		
	return INT(lnDecimal)
endfunc		&& StrToInteger 函数结束 

*==============================================================================
* 函数： DoubleValueToStr
* 状态： Public
* 用途： 双精度浮点数 转换为 字符串表示
* 参数： DoubleValue  双精度浮点数
*				
* 返回值： 字符型　8个字节的字符串
* 运行环境：：适应于INTEL CPU　的　低字在前高字在后　的操作系统,并且符合IEEE754标准的浮点数格式
*==============================================================================

FUNCTION DoubleValueToStr(DoubleValue AS double)
		LOCAL BinStr,tempMBinStr,StrLen,tempValue, ;
		EValue, ;		&&阶码值
		Ms, ;			&&符号位二进制串
		EBinStr, ;		&&阶码二进制串
		MBinStr, ;		&&尾数二进制串
		ResultStr		&&结果字符串
		
		IF DoubleValue = 0
			RETURN (CHR(0)+CHR(0)+CHR(0)+CHR(0)+CHR(0)+CHR(0)+CHR(0)+CHR(0))
		ENDIF
		
		Ms = SPACE(1)
		EBinStr = SPACE(11)
		MBinStr = SPACE(52)
		
		IF DoubleValue < 0 
			Ms = '1'
			tempValue = ABS(DoubleValue)
		ELSE
			Ms = '0'
			tempValue = DoubleValue
		ENDIF
				
		BinStr = ''
		tempMBitStr = ''
		EValue = 0
		StrLen = 64
		BinStr = IntegerToBinStr(INT(tempValue),@StrLen,.T.)
		EValue = StrLen - 1
		IF EValue < 0
			StrLen = 53
			tempMBinStr = DecToBinStr((tempValue - INT(tempValue)),@StrLen,.T.)
			MBinStr = SUBSTR(tempMBinStr,2,52)
			EValue = EValue - StrLen 
		ELSE
			StrLen = 52 - EValue
			tempMBinStr = DecToBinStr((tempValue - INT(tempValue)),@StrLen,.F.)
			MBinStr = SUBSTR(BinStr,2,EValue) + tempMBinStr
		ENDIF				
		StrLen = 11
		EBinStr = IntegerToBinStr((EValue + 2^10-1),@StrLen,.F.)
		
		ResultStr = Ms + EBinStr + MBinStr
		ResultStr = BinStrToHexStr(ResultStr)
		ResultStr = SUBSTR(ResultStr,8,1) ;
					+ SUBSTR(ResultStr,7,1) ;
					+ SUBSTR(ResultStr,6,1) ;
					+ SUBSTR(ResultStr,5,1) ;
					+ SUBSTR(ResultStr,4,1) ;
					+ SUBSTR(ResultStr,3,1) ;
					+ SUBSTR(ResultStr,2,1) ;
					+ LEFT(ResultStr,1) 
			
		RETURN (ResultStr)
		
		
ENDFUNC		&&DoubleValueToStr函数结束

*==============================================================================
* 函数： StrToDoubleValue
* 状态： Public
* 用途： 字符串表示 转换为 双精度浮点数
* 参数： HexStr  8个字节的字符串
*				
* 返回值： 双精度浮点数
* 运行环境：：适应于INTEL CPU　的　低字在前高字在后　的操作系统,并且符合IEEE754标准的浮点数格式
*==============================================================================
FUNCTION StrToDoubleValue(HexStr AS String )
	LOCAL tempStr,tempValue,MsBitValue,EBitValue,MBitValue,MsValue,C,MValue,ResultValue
	IF LEN(HexStr) <> 8
		MESSAGEBOX('字符串长度错误，应为８个字节',0,'错误信息')
		RETURN 0
	ENDIF
	tempStr = SUBSTR(HexStr,8,1) ;
				+ SUBSTR(HexStr,7,1) ;
				+ SUBSTR(HexStr,6,1) ;
				+ SUBSTR(HexStr,5,1) ;
				+ SUBSTR(HexStr,4,1) ;
				+ SUBSTR(HexStr,3,1) ;
				+ SUBSTR(HexStr,2,1) ;
				+ LEFT(HexStr,1) 
			
	tempStr = StrToBinStr(tempStr)
	MsBitValue = LEFT(tempStr,1)
	EBitValue = SUBSTR(tempStr,2,11)
	MBitValue = SUBSTR(tempStr,13,52)
	MsValue = IIF(MsBitValue=='1', -1, 1)
	EValue = BinStrToInteger(EBitValue)-1023
	MValue = 1 + BinStrToDec(MBitValue)
	ResultValue = (MsValue) * MValue * 2^EValue
	RETURN (ResultValue)
ENDFUNC		&&StrToDoubleValue函数结束


*==============================================================================
* 函数： SingleValueToStr
* 状态： Public
* 用途： 双精度浮点数 转换为 字符串表示
* 参数： DoubleValue  双精度浮点数
*				
* 返回值： 字符型　8个字节的字符串
* 运行环境：：适应于INTEL CPU　的　低字在前高字在后　的操作系统,并且符合IEEE754标准的浮点数格式
*==============================================================================

FUNCTION SingleValueToStr(DoubleValue AS single )
		LOCAL BinStr,tempMBinStr,StrLen,tempValue, ;
		EValue, ;		&&阶码值
		Ms, ;			&&符号位二进制串
		EBinStr, ;		&&阶码二进制串
		MBinStr, ;		&&尾数二进制串
		ResultStr		&&结果字符串
		
		IF DoubleValue = 0
			RETURN (CHR(0)+CHR(0)+CHR(0)+CHR(0)+CHR(0)+CHR(0)+CHR(0)+CHR(0))
		ENDIF
		
		Ms = SPACE(1)
		EBinStr = SPACE(8)
		MBinStr = SPACE(23)
		
		IF DoubleValue < 0
			Ms = '1'
			tempValue = ABS(DoubleValue)
		ELSE
			Ms = '0'
			tempValue = DoubleValue
		ENDIF
				
		BinStr = ''
		StrLen = 32
		BinStr = IntegerToBinStr(INT(tempValue),@StrLen,.T.)
		EValue = StrLen - 1
		
		IF EValue < 0
			StrLen = 24
			tempMBinStr = DecToBinStr((tempValue - INT(tempValue)),@StrLen,.T.)
			MBinStr = SUBSTR(tempMBinStr,2,23)
			EValue = EValue - StrLen 
		ELSE
			StrLen = 23 - EValue
			tempMBinStr = DecToBinStr((tempValue - INT(tempValue)),@StrLen,.F.)
			MBinStr = SUBSTR(BinStr,2,EValue) + tempMBinStr
		ENDIF				
		
		StrLen = 8
		EBinStr = IntegerToBinStr((EValue + 2^7-1),@StrLen,.F.)

		ResultStr = BinStrToHexStr(Ms + EBinStr + MBinStr)
		
		RETURN SUBSTR(ResultStr,4,1) ;
			+ SUBSTR(ResultStr,3,1) ;
			+ SUBSTR(ResultStr,2,1) ;
			+ left(ResultStr,1) 
		
		
ENDFUNC		&&SingleValueToStr函数结束

*==============================================================================
* 函数： StrToFloatValue
* 状态： Public
* 用途： 字符串表示 转换为 单精度浮点数
* 参数： HexStr  ４个字节的字符串
*				
* 返回值： 双精度浮点数
* 运行环境：：适应于INTEL CPU　的　低字在前高字在后　的操作系统,并且符合IEEE754标准的浮点数格式
*==============================================================================
FUNCTION StrToFloatValue(HexStr AS String )
	LOCAL tempStr,tempValue,MsBitValue,EBitValue,MBitValue,MsValue,C,MValue,ResultValue
	IF LEN(HexStr) <> 4
		MESSAGEBOX('字符串长度错误，应为４个字节',0,'错误信息')
		RETURN 0
	ENDIF
	tempStr = SUBSTR(HexStr,4,1) ;
				+ SUBSTR(HexStr,3,1) ;
				+ SUBSTR(HexStr,2,1) ;
				+ LEFT(HexStr,1) 
			
	tempStr = StrToBinStr(tempStr)
	MsBitValue = LEFT(tempStr,1)
	EBitValue = SUBSTR(tempStr,2,8)
	MBitValue = SUBSTR(tempStr,10,23)
	MsValue = IIF(MsBitValue=='1', -1, 1)
	EValue = BinStrToInteger(EBitValue)-127
	MValue = 1 + BinStrToDec(MBitValue)
	ResultValue = (MsValue) * MValue * 2^EValue
	RETURN (ResultValue)
ENDFUNC		&&StrToFloatValue函数结束



*================================================================
* 函数: GetMemPoint
* 状态: Public
* 用途: 将字符串转换为字符串指针，字符串指针所指内存位置保存有该字符串的一份拷贝
* 参数: uFlags   定义如何分配内存，可使用以下标志
*				#define GMEM_FIXED          0x0000			&&分配固定内存，返回一个指针
*				#define GMEM_MOVEABLE       0x0002			&&分配可移动内存
*				#define GMEM_ZEROINIT       0x0040			&&初始化内存的内容为0。不能单独使用，需与其它标志组合使用
*				#define GHND                BITOR(GMEM_MOVEABLE , GMEM_ZEROINIT)		&&分配固定内存，并初始化为0，返回一个指针
*				#define GPTR                BITOR(GMEM_FIXED , GMEM_ZEROINIT)		&&分配可移动内存，并初始化为0
*       MemLen  要求分配内存的长度（以字节计）
*		PointAddress	返回的内存指针
* 返回值: 分配内存长度。
* 运行环境：WIN32
*================================================================
   	
FUNCTION GetMemPoint(uFlags AS Integer ,MemLen AS Integer ,PointAddress AS Integer@)

		*!*	GlobalAlloc()函数从堆中分配指定字节大小空间。
		DECLARE INTEGER GlobalAlloc IN KERNEL32.DLL ;
							INTEGER		uFlags, ;		&&定义如何分配内存
							INTEGER		dwBytes			&&定义分配内存的字节数
						
		DECLARE INTEGER GlobalLock IN KERNEL32.DLL ;
							INTEGER		hMem			&&内存句柄
		
		DECLARE INTEGER GlobalSize IN KERNEL32.DLL ;
							INTEGER		hMem			&&内存句柄					
							
		DECLARE INTEGER GetLastError IN KERNEL32.DLL ;
												  
		
		LOCAL MemHandle,PAddress,MemSize					
		MemHandle = GlobalAlloc(uFlags,Memlen)
		IF MemHandle = 0
			GetErrorInformation(GetLastError())
			RETURN 0
		ENDIF
		
		PAddress = GlobalLock(MemHandle)
		IF PAddress = 0
			GetErrorInformation(GetLastError())
			RETURN 0
		ENDIF
		PointAddress = PAddress	
		MemSize = GlobalSize(MemHandle)
		CLEAR DLLS 'GlobalAlloc'
		CLEAR DLLS 'GlobalLock'
		CLEAR DLLS 'GlobalSize'
		CLEAR DLLS 'GetLastError'
		
		RETURN	(MemSize)
ENDFUNC 	&&	StrToPoint函数结束

*================================================================
* 函数: DeleteMemPoint
* 状态: Public
* 用途: 释放内存指针，
* 参数: tPointAddress	内存指针，它必须是用GetMemPoint函数获得的
*       
* 返回值: 无
* 运行环境：WIN32
*================================================================
FUNCTION DeleteMemPoint(PointAddress AS Integer)

		DECLARE INTEGER GlobalUnlock IN KERNEL32.DLL ;
							INTEGER		hMem			&&内存句柄
														
		DECLARE INTEGER GlobalFree IN KERNEL32.DLL ;
							INTEGER		hMem			&&内存句柄
							
		GlobalUnlock(PointAddress)
		GlobalFree(	PointAddress)
		CLEAR DLLS 'GlobalUnLock'
		CLEAR DLLS 'GlobalFree'
		
ENDFUNC 	&&DeleteMemPoint函数结束		
						

*==============================================================================
* 函数： BinStrToInteger
* 状态： Public
* 用途： 二进制字符串 转换为 十进制数 函数
* 参数： BinStr  二进制字符串 如：'11010100'
*       
* 返回值： 数值型　十进制数
* 运行环境：
*==============================================================================
FUNCTION BinStrToInteger(BinStr AS String )
   LOCAL StrLen,jnSum,C
   
   StrLen = LEN(ALLTRIM(BinStr))
   DecValue = 0
   FOR C = 0 TO StrLen
      IF SUBSTR(BinStr,StrLen-C,1) = '1'
          DecValue=DecValue + (2^C)
      ENDIF
   ENDFOR
RETURN int(DecValue)		&&  BinStrToDec 函数结束

*==============================================================================
* 函数： IntegerToBinStr
* 状态： Public
* 用途： 十进制数 转换为 二进制字符串 函数
* 参数： DecValue  整型值
*        StrLen		该变量按址传送，标识生成的二进制字符串长度，返回实际输出的字符串长度。
*		 bFlag		此值为 .T. 时，忽略StrLen参数，从不为‘0’的字符处输出字符串，
*					并把实际输出的字符串长度赋值给StrLen.,
*					 。为 .F.时使用指定的字符串长度。
* 返回值： 字符型　二进制字符串,为
* 运行环境：
*==============================================================================
FUNCTION IntegerToBinStr(DecValue AS Integer,StrLen AS Integer@,bFlag AS Boolean )

	LOCAL c,tempValue,BinStr
	tempValue = DecValue
	BinStr = ''
	
			
	IF tempValue < 0
		tempValue = 2^c+DecValue
	ENDIF

	IF bFlag = .T.
		IF tempValue  = 0
			StrLen = 0
			RETURN ''
		ENDIF
		
		c = 0
		DO while (ABS(tempValue) >= 2^c)
			c = c + 1
		ENDDO
		StrLen = c
					
		IF tempValue < 0
			tempValue = 2^c+DecValue
		ENDIF
	ENDIF
			
	IF tempValue < 0
		tempValue = 2^StrLen+DecValue
	ENDIF	
	
	IF tempValue > 2^StrLen
		MESSAGEBOX('定义的字符串长度太短，无法完成转换。',0,'错误信息')
		RETURN ''
	ENDIF
					
	FOR c=(StrLen-1) to 0 step -1
		BinStr = BinStr + CHR(48 + INT(tempValue/2^c))
		tempValue = mod(tempValue,2^c)
	ENDFOR
	
	RETURN (BinStr)

ENDFUNC			&&DecToBinStr()函数结束


*==============================================================================
* 函数： DecToBinStr(DecValue AS double,BinStrLen AS Integer )
* 状态： Public
* 用途： 定点纯小数 转换为 二进制字符串表示
* 参数： DecValue  正十进制纯小数
*		 BinStrLen  转换后的二进制串的长度,当uFlags为.T.时，返回非零数字前的零的个数。
*		 uFlags    为真值时，从第一个不为‘0’的位置返回字符串，并将非零数字前的零的个数的值写入BinStrLen中
* 返回值： 字符型　二进制字符串
* 运行环境：
*==============================================================================
FUNCTION DecToBinStr(DecValue AS double,BinStrLen AS Integer@,uFlags AS Boolean )

		lnCurrDecimal=SET("Decimals" )
		SET DECIMALS TO 17
		IF ABS(DecValue) >= 1
			MESSAGEBOX('非纯小数，无法转换',0,'错误信息')
		ENDIF
		LOCAL BinStr,c , tempValue,ReturnValue
		tempValue = DecValue
		
		ReturnValue = BinStrLen
		BinStr = ''
		IF uFlags = .T.
			c = 0
			DO while INT(tempValue * 2) < 1
				c = c +1
				tempValue = tempValue * 2
			ENDDO
			ReturnValue = c
		ENDIF
		c = 0
		
			
		DO while (c < BinStrLen)
			BinStr = BinStr + CHR(48+INT(tempValue * 2))
			IF (tempValue * 2) >= 1
				tempValue = tempValue * 2 - 1
			ELSE
				tempValue = tempValue * 2
			ENDIF
			c = c + 1
		ENDDO
		BinStrLen = ReturnValue
		SET DECIMALS TO lnCurrDecimal
		RETURN (BinStr)
ENDFUNC			&&DecToBinStr函数结束

*==============================================================================
* 函数： BinStrToDec(BinStr AS String)
* 状态： Public
* 用途： 二进制字符串 转换为 定点纯小数
* 参数： BinStr  正十进制纯小数的二进制字符串
*		
* 返回值： 纯小数
* 运行环境：
*==============================================================================
FUNCTION BinStrToDec(BinStr AS String)
	LOCAL StrLen,c,ResultValue
	lnCurrDecimal=SET("Decimals" )
	SET DECIMALS TO 17
	StrLen = LEN(BinStr)
	ResultValue = 0.00000000000000000
	FOR c= 1 to StrLen step 1
		ResultValue = ResultValue + IIF(SUBSTR(BinStr,c,1)=='1',2^(-1*c),0)
	ENDFOR
	
	SET DECIMALS TO lnCurrDecimal
	RETURN (ResultValue)
	
ENDFUNC &&BinStrToDec函数结束
*===============================================================================
* 函数： BinStrToStr
* 状态： Public
* 用途： 二进制串转换为数值字符串
* 参数： BinStr		二进制字符串
*       
* 返回值： 返回内存表示的数值字符串
* 运行环境：WIN32
*=============================================================================== 

FUNCTION BinStrToHexStr(BinStr AS String )
		LOCAL StrLen,c,b,tempStr,ResultStr
		StrLen = LEN(BinStr)
		c = MOD(StrLen,8)
		
		ResultStr = ''
		IF c > 0
			ResultStr = ResultStr + CHR(BinStrToInteger(LEFT(BinStr,c)))
		ENDIF
		b = StrLen - c
		FOR c = c+1 to b step 8
			ResultStr = ResultStr + CHR(BinStrToInteger(substr(BinStr,c,8)))
		ENDFOR
		RETURN 	(ResultStr)
ENDFUNC &&BinStrToStr函数结束

*===============================================================================
* 函数： StrToBinStr()
* 状态： Public
* 用途： 字符串转换为二进制字符串
* 参数： HexStr		字符串
*       
* 返回值： 返回二进制字符串
* 运行环境：WIN32
*===============================================================================  	
FUNCTION StrToBinStr(HexStr AS String )
	LOCAL StrLen,ReSultStr
	LOCAL BinStr(16)
	BinStr(1) = '0000'
	BinStr(2) = '0001'
	BinStr(3) = '0010'
	BinStr(4) = '0011'
	BinStr(5) = '0100'
	BinStr(6) = '0101'
	BinStr(7) = '0110'
	BinStr(8) = '0111'	 
	BinStr(9) = '1000'
	BinStr(10) = '1001'
	BinStr(11) = '1010'
	BinStr(12) = '1011'
	BinStr(13) = '1100'
	BinStr(14) = '1101'
	BinStr(15) = '1110'
	BinStr(16) = '1111'
	StrLen = LEN(HexStr)
	ReSultStr = ''
	FOR c = 1 to StrLen step 1
		ReSultStr = ReSultStr + BinStr(INT(ASC(SUBSTR(HexStr,c,1))/16)+1) + BinStr(MOD(ASC(SUBSTR(HexStr,c,1)),16)+1)
	ENDFOR
	RETURN (ResultStr)
	
ENDFUNC 	&&StrToBinStr函数结束
	

*===============================================================================
* 函数： GetErrorInformation()
* 状态： Public
* 用途： 调用API函数时获取错误信息
* 参数： lnError		错误代码可由GetLastError()函数获得,或有关API函数返回的错误代码
*       
* 返回值： 无
* 运行环境：WIN32
*===============================================================================  		

FUNCTION GetErrorInformation
	LPARAMETERS  lnError
	#DEFINE FORMAT_MESSAGE_FROM_SYSTEM     0x00001000
	#DEFINE MB_ICONINFORMATION      64      
	#DEFINE MB_OK                   0       
	#DEFINE CR						CHR(13) 
	
	DECLARE INTEGER FormatMessage IN kernel32.DLL ;
				INTEGER dwFlags, ;
				STRING @lpSource, ;
				INTEGER dwMessageId, ;
				INTEGER dwLanguageId, ;
				STRING @lpBuffer, ;
				INTEGER nSize, ;
				INTEGER Arguments
	
	PRIVATE lpBuffer
	lpBuffer  = SPACE(128)
	=FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM,'', lnError, 0, @lpBuffer, 128 , 0)
	=MESSAGEBOX("系统发生错误." + CR + ;
		"错误代码: " + ALLTRIM(STR(lnError)) + CR + ;
		"错误信息: "+ALLT(lpBuffer),MB_ICONINFORMATION+MB_OK,"错误信息")
	CLEAR DLLS 'FormatMessage'
	#undef FORMAT_MESSAGE_FROM_SYSTEM
	#undef MB_ICONINFORMATION
	#undef MB_OK
	#undef CR
ENDFUNC 

FUNCTION BMPAddIcon(lcDestFileName as string , ;
					lcBMPFileName as string , ;
					lcIconFileName as String , ;
					nx as Integer , ;
					ny as Integer)
					
#define IMAGE_BITMAP  0
#define DST_BITMAP      0x0004
#define LR_DEFAULTCOLOR     0x0000
#define LR_MONOCHROME       0x0001
#define LR_COLOR            0x0002
#define LR_COPYRETURNORG    0x0004
#define LR_COPYDELETEORG    0x0008
#define LR_LOADFROMFILE     0x0010
#define LR_LOADTRANSPARENT  0x0020
#define LR_DEFAULTSIZE      0x0040
#define LR_VGACOLOR         0x0080
#define LR_LOADMAP3DCOLORS  0x1000
#define LR_CREATEDIBSECTION 0x2000
#define LR_COPYFROMRESOURCE 0x4000
#define LR_SHARED           0x8000

#define SRCCOPY         0x00CC0020

#define SRCPAINT            0x00EE0086 
#define SRCAND              0x008800C6 
#define SRCINVERT           0x00660046 
#define SRCERASE            0x00440328 
#define NOTSRCCOPY          0x00330008 
#define NOTSRCERASE         0x001100A6 
#define MERGECOPY           0x00C000CA 
#define MERGEPAINT          0x00BB0226 
#define PATCOPY             0x00F00021 
#define PATPAINT            0x00FB0A09 
#define PATINVERT           0x005A0049 
#define DSTINVERT           0x00550009 
#define BLACKNESS           0x00000042 
#define WHITENESS           0x00FF0062 
DECLARE  INTEGER GetDC IN user32.dll INTEGER hwnd
DECLARE  INTEGER GetWindowDC IN user32.dll INTEGER hwnd
DECLARE integer LoadImage in user32.dll integer hinst, ;
								string lpszName, ;
								integer uType, ;
								integer cxDesired, ;
								integer cyDesired, ;
								integer fuLoad
								
DECLARE integer DrawState in user32.dll integer hDC, ;
								integer hbr, ;
								integer lpOutputFunc, ;
								integer lData, ;
								integer wData, ;
								integer x, ;
								integer y, ;
								integer cx, ;
								integer cy, ;
								integer fuFlags
								
DECLARE  INTEGER DeleteDC IN gdi32.dll INTEGER hdc	
DECLARE  INTEGER DeleteObject IN gdi32.dll INTEGER hObject
DECLARE SHORT GetDIBits in gdi32.dll long hdc,long hbmp,integer startscan, ;
			integer scanLines,long lpBits,long lpbi,integer wUsage
DECLARE  INTEGER SelectObject IN gdi32.dll INTEGER hdc, INTEGER hObject
DECLARE  INTEGER CreateCompatibleDC IN gdi32.dll INTEGER hdc
DECLARE integer StretchBlt in gdi32.dll long Desthdc,long nDestx,long nDesty, ;
                 long nDestWidth,long nDestHeight,long Srchdc,long Srcx,long Srcy, ;
                 long SrcWidth,long SrcHeight,long rop
DECLARE long CreateDIBitmap in gdi32.dll long hdc,string@ lpbmih,long fdwInit, ;
                 string@ lpbInit,string@ lpbmi,integer fuUsage
                 
DECLARE integer CreateCompatibleBitmap in gdi32.dll integer hDC,integer nWidth,integer nHeight
DECLARE integer CreateBitmap in gdi32.dll integer nWidth,integer nHeight,integer cPlanes,integer nBitsPerPel,integer lpvBits										
DECLARE integer MaskBlt in gdi32.dll integer hDCDest,integer nx,integer ny,integer nw,integer nH,integer hDCSrc,integer nxS,integer nyS,integer hbmmsk,integer xM,integer yM,integer rop
PRIVATE hSrcBitmap,hDestBitmap,hDC,IconDC,hVDC

hDC = GetDC(0)
hIconBitmap = LoadImage( 0,lcIconFileName,IMAGE_BITMAP,0,0,BITOR(LR_DEFAULTCOLOR,LR_LOADFROMFILE))
hDestBitmap = LoadImage( 0,lcBMPFileName,IMAGE_BITMAP,0,0,BITOR(LR_DEFAULTCOLOR,LR_LOADFROMFILE))

IconDC = CreateCompatibleDC(hDC)
SelectObject(IconDC,hIconBitmap)
hVDC = CreateCompatibleDC(hDC)
SelectObject(hVDC,hDestBitmap)

lnWidth=0
lnHeight = 0
GetBitmapSize(hIconBitmap,@lnWidth,@lnHeight)
StretchBlt(hVDC , nx , ny , lnWidth , lnHeight , IconDC , 0 , 0 , lnWidth , lnHeight , SRCCOPY )

PRIVATE pbi
pbi = CreateBitmapInfoStruct( hDestBitmap )

CreateBMPFile( lcDestFileName , pbi , hDestBitmap , hVDC )

DeleteObject( hIconBitmap )	&&释放对象
DeleteObject( hDestBitmap)
DeleteDC( hVdc )
DeleteDC( hdc )
DeleteDC( IconDC )

ENDFUNC



************************************************************************************
* GetBitmapSize(hbmp as long ,lnWidth as integer ,lnHeight as integer)
* 功　能：由位图句柄得到位图宽度和高度
* 参　数：hbmp   	位图句柄
*         lnWidth  	返回位图宽度
*         lnHeight	返回位图高度
*************************************************************************************
FUNCTION GetBitmapSize(hbmp as long ,lnWidth as integer@ ,lnHeight as integer@)
	DECLARE integer GetObject in gdi32.dll as GetObjectAPI integer hbmp, ;
			integer BITBMPSize,string@ bmp				
	#define BI_RGB        0
	bmp = SPACE(24)
	pbmi = 0
	IF GetObjectAPI(hBmp,24,@bmp) = 0
		MESSAGEBOX('GetObject调用发生错误！')
		RETURN
	ENDIF
	lnWidth = StrToInteger(SUBSTR( bmp , 5 , 4 ) , .T. )
	lnHeight = StrToInteger(SUBSTR(bmp , 9 , 4 ) , .T. )
ENDFUNC


****************************************************************************************
*  函数名：GetBMPSize
*  功　能：得到位图的宽度和高度
*  参　数：lcFileName		指定的位图文件
*          lnWidth			返回位图的宽度
*          lnHeight			返回位图的高度
*  返回值：成功则返回.T.，否则返回.F.
*运行环境：Visual Foxpro 8.0
****************************************************************************************
FUNCTION GetBMPSize( lcFileName as String , lnWidth as Integer@, lnHeight as Integer@)
	PRIVATE lFileHandle,BMPFlag
	IF not FILE(lcFileName)
		MESSAGEBOX(lcFileName + '没有找到指定文件！',0)
		RETURN .F.
	ENDIF
	
	lFileHandle = FOPEN(lcFileName)
	IF lFileHandle = -1
		MESSAGEBOX('无法打开文件：' + lcFileName,0)
		RETURN .F.
	ENDIF
	BMPFlag = FREAD(lFileHandle,2)
	IF BMPFlag <> 'BM'
		MESSAGEBOX(lcFileName + '不是一个合法的BMP文件')
		RETURN .F.
	ENDIF
		
	FSEEK(lFileHandle,18,0)
	lnWidth = 0
	lnHeight = 0
	&&得到ＢＭＰ位图的宽度（象素）
	lnWidth = ASC(FREAD(lFileHandle,1))
	lnWidth = lnWidth + ASC(FREAD(lFileHandle,1)) * 0x100
	lnWidth = lnWidth + ASC(FREAD(lFileHandle,1)) * 0x10000
	lnWidth = lnWidth + ASC(FREAD(lFileHandle,1)) * 0x1000000
	
	&&得到ＢＭＰ位图的高度度（象素）
	lnHeight = ASC(FREAD(lFileHandle,1))
	lnHeight = lnHeight + ASC(FREAD(lFileHandle,1)) * 0x100
	lnHeight = lnHeight + ASC(FREAD(lFileHandle,1)) * 0x10000
	lnHeight = lnHeight + ASC(FREAD(lFileHandle,1)) * 0x1000000	
	
	FCLOSE(lFileHandle)
	RETURN .T.
	
ENDFUNC

****************************************************************************************
*  函数名：GetJPGSize
*  功　能：得到位图的宽度和高度
*  参　数：lcFileName		指定的位图文件
*          lnWidth			返回位图的宽度
*          lnHeight			返回位图的高度
*  返回值：成功则返回.T.，否则返回.F.
*运行环境：Visual Foxpro 8.0
****************************************************************************************
FUNCTION GetJPGSize( lcFileName as String , lnWidth as Integer@, lnHeight as Integer@)	
	PRIVATE lFileHandle,JPGFlag,MarkerFlag,lnMarkerSize
	IF not FILE(lcFileName)
		MESSAGEBOX(lcFileName + '没有找到指定文件！',0)
		RETURN .F.
	ENDIF
	
	lFileHandle = FOPEN(lcFileName)
	IF lFileHandle = -1
		MESSAGEBOX('无法打开文件：' + lcFileName,0)
		RETURN .F.
	ENDIF
	JPGFlag = FREAD(lFileHandle,2)
	IF JPGFlag <> CHR(0xFF) + CHR(0xD8)
		MESSAGEBOX(lcFileName + '不是一个合法的JPG文件')
		RETURN .F.
	ENDIF
	
	lnHeight = 0
	lnWidth = 0
	tmpHeight = 0
	
	DO while not EOF()
		DO while not EOF()
			MarkerFlag = FREAD(lFileHandle,1)
			IF MarkerFlag == CHR(0xFF)
				EXIT
			ENDIF
		ENDDO
		
		IF EOF()
			FCLOSE(lFileHandle)
			RETURN .F.
		ENDIF
		
		IF MarkerFlag == CHR(0xFF)
			DO while not EOF()
				MarkerFlag = FREAD(lFileHandle,1)
				IF MarkerFlag <> CHR(0xFF)
					EXIT
				ENDIF
			ENDDO
		ENDIF
		
		IF EOF()
			FCLOSE(lFileHandle)
			RETURN .F.
		ENDIF
		
		lnMarkerSize = ASC(FREAD(lFileHandle,1)) * 0x100
		lnMarkerSize = lnMarkerSize + ASC(FREAD(lFileHandle,1))

		
		
		IF MarkerFlag == CHR(0xC0)
			FSEEK(lFileHandle,1,1)
			
			lnHeight = ASC(FREAD(lFileHandle,1)) * 0x100
			lnHeight = lnHeight + ASC(FREAD(lFileHandle,1))
			
			
			lnWidth = ASC(FREAD(lFileHandle,1)) * 0x100
			lnWidth = lnWidth + ASC(FREAD(lFileHandle,1))
			IF lnHeight <> 0
				FCLOSE(lFileHandle)
				RETURN .T.
			ENDIF
			
		ENDIF
		
		IF lnHeight = 0
			IF MarkerFlag == CHR(0xDC)
				lnHeight = FREAD(lFileHandle,1) * 0x100
				lnHeight = lnHeight + FREAD(lFileHandle,1)
			ENDIF
		ENDIF
		
		IF (lnWidth> 0 and lnHeight > 0 )
			FCLOSE(lFileHandle)
			EXIT
		ENDIF
		FSEEK(lFileHandle,lnMarkerSize - 2 ,1)
				
	ENDDO
	
ENDFUNC