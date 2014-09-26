;;
;;  ISO 4217
;;  Codes for the representation of currencies and funds.
;;
;;  Copyright 2014 Thomas de Grivel <thomas@lowh.net>
;;
;;  Permission to use, copy, modify, and distribute this software for any
;;  purpose with or without fee is hereby granted, provided that the above
;;  copyright notice and this permission notice appear in all copies.
;;
;;  THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;;  WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;;  MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;;  ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;;  WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;;  ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;;  OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
;;

(in-package :cl-user)

(defpackage :iso4217
  (:use :cl)
  (:export
   #:*currencies* #:country-currencies #:currency #:currency-alpha3
   #:currency-name #:currency-numeric #:currency-exp #:currency-countries
   #:AED #:AFN #:ALL #:AMD #:ANG #:AOA #:ARS #:AUD #:AWG #:AZN #:BAM #:BBD
   #:BDT #:BGN #:BHD #:BIF #:BMD #:BND #:BOB #:BRL #:BSD #:BTN #:BWP #:BYR
   #:BZD #:CAD #:CDF #:CHF #:CLP #:CNY #:COP #:CRC #:CUC #:CUP #:CVE #:CZK
   #:DJF #:DKK #:DOP #:DZD #:EEK #:EGP #:ERN #:ETB #:EUR #:FJD #:FKP #:GBP
   #:GEL #:GHS #:GIP #:GMD #:GNF #:GTQ #:GWP #:GYD #:HKD #:HNL #:HRK #:HTG
   #:HUF #:IDR #:ILS #:INR #:IQD #:IRR #:ISK #:JMD #:JOD #:JPY #:KES #:KGS
   #:KHR #:KMF #:KPW #:KRW #:KWD #:KYD #:KZT #:LAK #:LBP #:LKR #:LRD #:LSL
   #:LTL #:LVL #:LYD #:MAD #:MDL #:MGA #:MKD #:MMK #:MNT #:MOP #:MRO #:MUR
   #:MVR #:MWK #:MXN #:MYR #:MZN #:NAD #:NGN #:NIO #:NOK #:NPR #:NZD #:OMR
   #:PAB #:PEN #:PGK #:PHP #:PKR #:PLN #:PYG #:QAR #:RON #:RSD #:RUB #:RWF
   #:SAR #:SBD #:SCR #:SDG #:SEK #:SGD #:SHP #:SLL #:SOS #:SRD #:SSP #:STD
   #:SYP #:SZL #:THB #:TJS #:TMT #:TND #:TOP #:TRY #:TTD #:TWD #:TZS #:UAH
   #:UGX #:USD #:UYU #:UZS #:VEF #:VND #:VUV #:WST #:XAF #:XCD #:XOF #:XPF
   #:YER #:ZAR #:ZMW))

(in-package :iso4217)

(defclass currency ()
  ((alpha3 :type symbol
	   :initarg :alpha3
	   :reader currency-alpha3)
   (name :type string
	 :initarg :name
	 :reader currency-name)
   (numeric :type (integer 1 999)
	    :initarg :numeric
	    :reader currency-numeric)
   (exp :type (integer 0 9)
	:initarg :exp
	:reader currency-exp)
   (countries :type list
	      :initarg :countries
	      :reader currency-countries)))

(defmethod print-object ((c currency) stream)
  (if *print-readably*
      (print-unreadable-object (c stream :type t :identity t)
	(format stream "~A ~3,'0D ~D ~S"
		(currency-alpha3 c)
		(currency-numeric c)
		(currency-exp c)
		(currency-name c)))
      (prin1 (currency-alpha3 c) stream)))

(defmacro define-currency (alpha3 numeric exp name countries)
  (assert (= 3 (length (symbol-name alpha3))))
  (assert (eq (find-package :iso4217)
	      (symbol-package alpha3)))
  `(let ((c (make-instance 'currency
			   :alpha3 ',alpha3
			   :numeric ,numeric
			   :exp ,exp
			   :name ,name
			   :countries (list ,@countries))))
     (unless (boundp ',alpha3)
       (defconstant ,alpha3 c))
     c))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *rt-stack* nil)
  (push *readtable* *rt-stack*)
  (set-macro-character
   #\[
   (lambda (stream char)
     (declare (ignore char))
     `(list
       ,@(loop until (cl:and (char= #\] (peek-char t stream))
			     (read-char stream))
	    for alpha3 = (read stream)
	    for numeric = (read stream)
	    for exp = (read stream)
	    for name = (read stream)
	    for countries = (read stream)
	    collect `(define-currency ,alpha3 ,numeric ,exp ,name
				      ,countries))))))

(defvar *currencies* [
  AFN 971 2 "Afghani" (ISO3166:AF)
  DZD 012 2 "Algerian Dinar" (ISO3166:DZ)
  ARS 032 2 "Argentine Peso" (ISO3166:AR)
  AMD 051 2 "Armenian Dram" (ISO3166:AM)
  AWG 533 2 "Aruban Florin" (ISO3166:AW)
  AUD 036 2 "Australian Dollar" (ISO3166:AU
				 ISO3166:CX
				 ISO3166:CC
				 ISO3166:HM
				 ISO3166:KI
				 ISO3166:NR
				 ISO3166:NF
				 ISO3166:TV)
  AZN 944 2 "Azerbaijanian Manat" (ISO3166:AZ)
  BSD 044 2 "Bahamian Dollar" (ISO3166:BS)
  BHD 048 3 "Bahraini Dinar" (ISO3166:BH)
  THB 764 2 "Baht" (ISO3166:TH)
  PAB 590 2 "Balboa" (ISO3166:PA)
  BBD 052 2 "Barbados Dollar" (ISO3166:BB)
  BYR 974 0 "Belarussian Ruble" (ISO3166:BY)
  BZD 084 2 "Belize Dollar" (ISO3166:BZ)
  BMD 060 2 "Bermudian Dollar" (ISO3166:BM)
  VEF 937 2 "Bolivar" (ISO3166:VE)
  BOB 068 2 "Boliviano" (ISO3166:BO)
  BRL 986 2 "Brazilian Real" (ISO3166:BR)
  BND 096 2 "Brunei Dollar" (ISO3166:BN)
  BGN 975 2 "Bulgarian Lev" (ISO3166:BG)
  BIF 108 0 "Burundi Franc" (ISO3166:BI)
  CAD 124 2 "Canadian Dollar" (ISO3166:CA)
  CVE 132 2 "Cape Verde Escudo" (ISO3166:CV)
  KYD 136 2 "Cayman Islands Dollar" (ISO3166:KY)
  XOF 952 0 "CFA Franc BCEAO" (ISO3166:NE
			       ISO3166:SN)
  XAF 950 0 "CFA Franc BEAC" (ISO3166:BJ
			      ISO3166:BF
			      ISO3166:CM
			      ISO3166:CF
			      ISO3166:TD
			      ISO3166:CG
			      ISO3166:CI
			      ISO3166:GQ
			      ISO3166:GA
			      ISO3166:ML
			      ISO3166:TG)
  XPF 953 0 "CFP Franc" (ISO3166:PF
			 ISO3166:NC
			 ISO3166:WF)
  CLP 152 0 "Chilean Peso" (ISO3166:CL)
  COP 170 2 "Colombian Peso" (ISO3166:CO)
  KMF 174 0 "Comoro Franc" (ISO3166:KM)
  CDF 976 2 "Congolese Franc" (ISO3166:CD)
  BAM 977 2 "Convertible Mark" (ISO3166:BA)
  CRC 188 2 "Costa Rican Colon" (ISO3166:CR)
  HRK 191 2 "Croatian Kuna" (ISO3166:HR)
  CUP 192 2 "Cuban Peso" (ISO3166:CU)
  CZK 203 2 "Czech Koruna" (ISO3166:CZ)
  NIO 558 2 "Córdoba" (ISO3166:NI)
  GMD 270 2 "Dalasi" (ISO3166:GM)
  DKK 208 2 "Danish Krone" (ISO3166:DK
			    ISO3166:FO
			    ISO3166:GL)
  MKD 807 2 "Denar" (ISO3166:MK)
  DJF 262 0 "Djibouti Franc" (ISO3166:DJ)
  STD 678 2 "Dobra" (ISO3166:ST)
  DOP 214 2 "Dominican Peso" (ISO3166:DO)
  VND 704 0 "Dong" (ISO3166:VN)
  XCD 951 2 "East Caribbean Dollar" (ISO3166:AI
				     ISO3166:AG
				     ISO3166:DM
				     ISO3166:GD
				     ISO3166:MS
				     ISO3166:KN
				     ISO3166:LC
				     ISO3166:VC)
  EGP 818 2 "Egyptian Pound" (ISO3166:EG)
  ETB 230 2 "Ethiopian Birr" (ISO3166:ET)
  EUR 978 2 "Euro" (ISO3166:AX
		    ISO3166:AD
		    ISO3166:AT
		    ISO3166:BE
		    ISO3166:CY
		    ISO3166:FI
		    ISO3166:FR
		    ISO3166:GF
		    ISO3166:TF
		    ISO3166:DE
		    ISO3166:GR
		    ISO3166:GP
		    ISO3166:VA
		    ISO3166:IE
		    ISO3166:IT
		    ISO3166:LU
		    ISO3166:MT
		    ISO3166:MQ
		    ISO3166:YT
		    ISO3166:MC
		    ISO3166:ME
		    ISO3166:NL
		    ISO3166:PT
		    ISO3166:RE
		    ISO3166:BL
		    ISO3166:MF
		    ISO3166:PM
		    ISO3166:SM
		    ISO3166:SK
		    ISO3166:SI
		    ISO3166:ES
		    ISO3166:ZW)
  FKP 238 2 "Falkland Islands Pound" (ISO3166:FK)
  FJD 242 2 "Fiji Dollar" (ISO3166:FJ)
  HUF 348 2 "Forint" (ISO3166:HU)
  GHS 936 2 "Ghana Cedi" (ISO3166:GH)
  GIP 292 2 "Gibraltar Pound" (ISO3166:GI)
  HTG 332 2 "Gourde" (ISO3166:HT)
  PYG 600 0 "Guarani" (ISO3166:PY)
  GNF 324 0 "Guinea Franc" (ISO3166:GN)
  GYD 328 2 "Guyana Dollar" (ISO3166:GY)
  HKD 344 2 "Hong Kong Dollar" (ISO3166:HK)
  UAH 980 2 "Hryvnia" (ISO3166:UA)
  ISK 352 0 "Iceland Krona" (ISO3166:IS)
  INR 356 2 "Indian Rupee" (ISO3166:IN)
  IRR 364 2 "Iranian Rial" (ISO3166:IR)
  IQD 368 3 "Iraqi Dinar" (ISO3166:IQ)
  JMD 388 2 "Jamaican Dollar" (ISO3166:JM)
  JOD 400 3 "Jordanian Dinar" (ISO3166:JO)
  KES 404 2 "Kenyan Shilling" (ISO3166:KE)
  PGK 598 2 "Kina" (ISO3166:PG)
  LAK 418 2 "Kip" (ISO3166:LA)
  KWD 414 3 "Kuwaiti Dinar" (ISO3166:KW)
  MWK 454 2 "Kwacha" (ISO3166:MW)
  AOA 973 2 "Kwanza" (ISO3166:AO)
  MMK 104 2 "Kyat" (ISO3166:MM)
  GEL 981 2 "Lari" (ISO3166:GE)
  LVL 428 2 "Latvian Lats" (ISO3166:LV)
  LBP 422 2 "Lebanese Pound" (ISO3166:LB)
  ALL 008 2 "Lek" (ISO3166:AL)
  HNL 340 2 "Lempira" (ISO3166:HN)
  SLL 694 2 "Leone" (ISO3166:SL)
  LRD 430 2 "Liberian Dollar" (ISO3166:LR)
  LYD 434 3 "Libyan Dinar" (ISO3166:LY)
  SZL 748 2 "Lilangeni" (ISO3166:SZ)
  LTL 440 2 "Lithuanian Litas" (ISO3166:LT)
  LSL 426 2 "Loti" (ISO3166:LS)
  MGA 969 2 "Malagasy Ariary" (ISO3166:MG)
  MYR 458 2 "Malaysian Ringgit" (ISO3166:MY)
  TMT 934 2 "Manat" (ISO3166:TM)
  MUR 480 2 "Mauritius Rupee" (ISO3166:MU)
  MXN 484 2 "Mexican Peso" (ISO3166:MX)
  MDL 498 2 "Moldovan Leu" (ISO3166:MD)
  MAD 504 2 "Moroccan Dirham" (ISO3166:MA
			       ISO3166:EH)
  MZN 943 2 "Mozambique Metical" (ISO3166:MZ)
  NGN 566 2 "Naira" (ISO3166:NG)
  ERN 232 2 "Nakfa" (ISO3166:ER)
  NAD 516 2 "Namibia Dollar" (ISO3166:NA)
  NPR 524 2 "Nepalese Rupee" (ISO3166:NP)
  ANG 532 2 "Netherlands Antillean Guilder" (ISO3166:CW
					     ISO3166:SX)
  ILS 376 2 "New Israeli Sheqel" (ISO3166:IL
				  ISO3166:PS)
  RON 946 2 "New Romanian Leu" (ISO3166:RO)
  TWD 901 2 "New Taiwan Dollar" (ISO3166:TW)
  NZD 554 2 "New Zealand Dollar" (ISO3166:CK
				  ISO3166:NZ
				  ISO3166:NU
				  ISO3166:PN
				  ISO3166:TK)
  BTN 064 2 "Ngultrum" (ISO3166:BT)
  KPW 408 2 "North Korean Won" (ISO3166:KP)
  NOK 578 2 "Norwegian Krone" (ISO3166:AQ
			       ISO3166:BV
			       ISO3166:NO
			       ISO3166:SJ)
  PEN 604 2 "Nuevo Sol" (ISO3166:PE)
  MRO 478 2 "Ouguiya" (ISO3166:MR)
  PKR 586 2 "Pakistan Rupee" (ISO3166:PK)
  MOP 446 2 "Pataca" (ISO3166:MO)
  TOP 776 2 "Pa’anga" (ISO3166:TO)
  CUC 931 2 "Peso Convertible" (ISO3166:CU)
  UYU 858 2 "Peso Uruguayo" (ISO3166:UY)
  PHP 608 2 "Philippine Peso" (ISO3166:PH)
  GBP 826 2 "Pound Sterling" (ISO3166:IO
			      ISO3166:GG
			      ISO3166:IM
			      ISO3166:JE
			      ISO3166:GS
			      ISO3166:GB
			      ISO3166:ZW)
  BWP 072 2 "Pula" (ISO3166:BW
		    ISO3166:ZW)
  QAR 634 2 "Qatari Rial" (ISO3166:QA)
  GTQ 320 2 "Quetzal" (ISO3166:GT)
  ZAR 710 2 "Rand" (ISO3166:ZA
		    ISO3166:ZW)
  OMR 512 3 "Rial Omani" (ISO3166:OM)
  KHR 116 2 "Riel" (ISO3166:KH)
  MVR 462 2 "Rufiyaa" (ISO3166:MV)
  IDR 360 2 "Rupiah" (ISO3166:ID)
  RUB 643 2 "Russian Ruble" (ISO3166:RU)
  RWF 646 0 "Rwanda Franc" (ISO3166:RW)
  SHP 654 2 "Saint Helena Pound" (ISO3166:SH)
  SAR 682 2 "Saudi Riyal" (ISO3166:SA)
  RSD 941 2 "Serbian Dinar" (ISO3166:RS)
  SCR 690 2 "Seychelles Rupee" (ISO3166:SC)
  SGD 702 2 "Singapore Dollar" (ISO3166:SG)
  SBD 090 2 "Solomon Islands Dollar" (ISO3166:SB)
  KGS 417 2 "Som" (ISO3166:KG)
  SOS 706 2 "Somali Shilling" (ISO3166:SO)
  TJS 972 2 "Somoni" (ISO3166:TJ)
  SSP 728 2 "South Sudanese Pound" (ISO3166:SS)
  LKR 144 2 "Sri Lanka Rupee" (ISO3166:LK)
  SDG 938 2 "Sudanese Pound" (ISO3166:SD)
  SRD 968 2 "Surinam Dollar" (ISO3166:SR)
  SEK 752 2 "Swedish Krona" (ISO3166:SE)
  CHF 756 2 "Swiss Franc" (ISO3166:LI
			   ISO3166:CH)
  SYP 760 2 "Syrian Pound" (ISO3166:SY)
  BDT 050 2 "Taka" (ISO3166:BD)
  WST 882 2 "Tala" (ISO3166:WS)
  TZS 834 2 "Tanzanian Shilling" (ISO3166:TZ)
  KZT 398 2 "Tenge" (ISO3166:KZ)
  TTD 780 2 "Trinidad and Tobago Dollar" (ISO3166:TT)
  MNT 496 2 "Tugrik" (ISO3166:MN)
  TND 788 3 "Tunisian Dinar" (ISO3166:TN)
  TRY 949 2 "Turkish Lira" (ISO3166:TR)
  AED 784 2 "UAE Dirham" (ISO3166:AE)
  UGX 800 0 "Uganda Shilling" (ISO3166:UG)
  USD 840 2 "US Dollar" (ISO3166:AS
			 ISO3166:BQ
			 ISO3166:EC
			 ISO3166:SV
			 ISO3166:GU
			 ISO3166:MH
			 ISO3166:FM
			 ISO3166:MP
			 ISO3166:PW
			 ISO3166:PR
			 ISO3166:MF
			 ISO3166:TL
			 ISO3166:TC
			 ISO3166:US
			 ISO3166:UM
			 ISO3166:VG
			 ISO3166:VI
			 ISO3166:ZW)
  UZS 860 2 "Uzbekistan Sum" (ISO3166:UZ)
  VUV 548 0 "Vatu" (ISO3166:VU)
  KRW 410 0 "Won" (ISO3166:KR)
  YER 886 2 "Yemeni Rial" (ISO3166:YE)
  JPY 392 0 "Yen" (ISO3166:JP)
  CNY 156 2 "Yuan Renminbi" (ISO3166:CN)
  ZMW 967 2 "Zambian Kwacha" (ISO3166:ZM)
  PLN 985 2 "Zloty" (ISO3166:PL)
  ])

(eval-when (:compile-toplevel :load-toplevel :execute)
  (loop while *rt-stack*
     do (setq *readtable* (pop *rt-stack*))))

(defun country-currencies (country)
  (sort
   (loop for cur in *currencies*
         if (find country (currency-countries cur))
         collect cur)
   #'string< :key #'currency-alpha3))
