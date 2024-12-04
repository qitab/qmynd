;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                  ;;;
;;; Free Software published under an MIT-like license. See LICENSE   ;;;
;;;                                                                  ;;;
;;; Copyright (c) 2012-2013 Google, Inc.  All rights reserved.       ;;;
;;;                                                                  ;;;
;;; Original author: Alejandro Sedeño                                ;;;
;;;                                                                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :qmynd-impl)

(eval-when (:compile-toplevel :load-toplevel :execute)

;; Character Sets (partially taken from +MYSQL+/sql/share/charsets/Index.xml)
;;
;; See also http://bugs.mysql.com/bug.php?id=22456
;;
;; and
;; http://bazaar.launchpad.net/~mysql/connectorj/5.1/view/head:/src/com/mysql/jdbc/CharsetMapping.java

(defconstant +mysql-cs-coll-big5-chinese-ci+           1)
(defconstant +mysql-cs-coll-latin2-czech-cs+           2)
(defconstant +mysql-cs-coll-dec8-swedish-ci+           3)
(defconstant +mysql-cs-coll-cp850-general-ci+          4)
(defconstant +mysql-cs-coll-latin1-german1-ci+         5)
(defconstant +mysql-cs-coll-hp8-english-ci+            6)
(defconstant +mysql-cs-coll-koi8r-general-ci+          7)
(defconstant +mysql-cs-coll-latin1-swedish-ci+         8)
(defconstant +mysql-cs-coll-latin2-general-ci+         9)
(defconstant +mysql-cs-coll-swe7-swedish-ci+           10)
(defconstant +mysql-cs-coll-ascii-general-ci+          11)
(defconstant +mysql-cs-coll-ujis-japanese-ci+          12)
(defconstant +mysql-cs-coll-sjis-japanese-ci+          13)
(defconstant +mysql-cs-coll-cp1251-bulgarian-ci+       14)
(defconstant +mysql-cs-coll-latin1-danish-ci+          15)
(defconstant +mysql-cs-coll-hebrew-general-ci+         16)
(defconstant +mysql-cs-coll-win1251+                   17) ; removed since 4.1
(defconstant +mysql-cs-coll-tis620-thai-ci+            18)
(defconstant +mysql-cs-coll-euckr-korean-ci+           19)
(defconstant +mysql-cs-coll-latin7-estonian-cs+        20)
(defconstant +mysql-cs-coll-latin2-hungarian-ci+       21)
(defconstant +mysql-cs-coll-koi8u-general-ci+          22)
(defconstant +mysql-cs-coll-cp1251-ukrainian-ci+       23)
(defconstant +mysql-cs-coll-gb2312-chinese-ci+         24)
(defconstant +mysql-cs-coll-greek-general-ci+          25)
(defconstant +mysql-cs-coll-cp1250-general-ci+         26)
(defconstant +mysql-cs-coll-latin2-croatian-ci+        27)
(defconstant +mysql-cs-coll-gbk-chinese-ci+            28)
(defconstant +mysql-cs-coll-cp1257-lithuanian-ci+      29)
(defconstant +mysql-cs-coll-latin5-turkish-ci+         30)
(defconstant +mysql-cs-coll-latin1-german2-ci+         31)
(defconstant +mysql-cs-coll-armscii8-general-ci+       32)
(defconstant +mysql-cs-coll-utf8mb3-general-ci+        33)
(defconstant +mysql-cs-coll-cp1250-czech-cs+           34)
(defconstant +mysql-cs-coll-ucs2-general-ci+           35)
(defconstant +mysql-cs-coll-cp866-general-ci+          36)
(defconstant +mysql-cs-coll-keybcs2-general-ci+        37)
(defconstant +mysql-cs-coll-macce-general-ci+          38)
(defconstant +mysql-cs-coll-macroman-general-ci+       39)
(defconstant +mysql-cs-coll-cp852-general-ci+          40)
(defconstant +mysql-cs-coll-latin7-general-ci+         41)
(defconstant +mysql-cs-coll-latin7-general-cs+         42)
(defconstant +mysql-cs-coll-macce-binary+              43)
(defconstant +mysql-cs-coll-cp1250-croatian-ci+        44)
(defconstant +mysql-cs-coll-utf8mb4-general-ci+        45)
(defconstant +mysql-cs-coll-utf8mb4-binary+            46)
(defconstant +mysql-cs-coll-latin1-binary+             47)
(defconstant +mysql-cs-coll-latin1-general-ci+         48)
(defconstant +mysql-cs-coll-latin1-general-cs+         49)
(defconstant +mysql-cs-coll-cp1251-binary+             50)
(defconstant +mysql-cs-coll-cp1251-general-ci+         51)
(defconstant +mysql-cs-coll-cp1251-general-cs+         52)
(defconstant +mysql-cs-coll-macroman-binary+           53)
(defconstant +mysql-cs-coll-utf16-general-ci+          54)
(defconstant +mysql-cs-coll-utf16-binary+              55)
(defconstant +mysql-cs-coll-utf16le-general-ci+        56)
(defconstant +mysql-cs-coll-cp1256-general-ci+         57)
(defconstant +mysql-cs-coll-cp1257-binary+             58)
(defconstant +mysql-cs-coll-cp1257-general-ci+         59)
(defconstant +mysql-cs-coll-utf32-general-ci+          60)
(defconstant +mysql-cs-coll-utf32-binary+              61)
(defconstant +mysql-cs-coll-utf16le-binary+            62)
(defconstant +mysql-cs-coll-binary+                    63)
(defconstant +mysql-cs-coll-armscii8-binary+           64)
(defconstant +mysql-cs-coll-ascii-binary+              65)
(defconstant +mysql-cs-coll-cp1250-binary+             66)
(defconstant +mysql-cs-coll-cp1256-binary+             67)
(defconstant +mysql-cs-coll-cp866-binary+              68)
(defconstant +mysql-cs-coll-dec8-binary+               69)
(defconstant +mysql-cs-coll-greek-binary+              70)
(defconstant +mysql-cs-coll-hebrew-binary+             71)
(defconstant +mysql-cs-coll-hp8-binary+                72)
(defconstant +mysql-cs-coll-keybcs2-binary+            73)
(defconstant +mysql-cs-coll-koi8r-binary+              74)
(defconstant +mysql-cs-coll-koi8u-binary+              75)
(defconstant +mysql-cs-coll-utf8mb3-tolower-ci+        76)
(defconstant +mysql-cs-coll-latin2-binary+             77)
(defconstant +mysql-cs-coll-latin5-binary+             78)
(defconstant +mysql-cs-coll-latin7-binary+             79)
(defconstant +mysql-cs-coll-cp850-binary+              80)
(defconstant +mysql-cs-coll-cp852-binary+              81)
(defconstant +mysql-cs-coll-swe7-binary+               82)
(defconstant +mysql-cs-coll-utf8mb3-binary+            83)
(defconstant +mysql-cs-coll-big5-binary+               84)
(defconstant +mysql-cs-coll-euckr-binary+              85)
(defconstant +mysql-cs-coll-gb2312-binary+             86)
(defconstant +mysql-cs-coll-gbk-binary+                87)
(defconstant +mysql-cs-coll-sjis-binary+               88)
(defconstant +mysql-cs-coll-tis620-binary+             89)
(defconstant +mysql-cs-coll-ucs2-binary+               90)
(defconstant +mysql-cs-coll-ujis-binary+               91)
(defconstant +mysql-cs-coll-geostd8-general-ci+        92)
(defconstant +mysql-cs-coll-geostd8-binary+            93)
(defconstant +mysql-cs-coll-latin1-spanish-ci+         94)
(defconstant +mysql-cs-coll-cp932-japanese-ci+         95)
(defconstant +mysql-cs-coll-cp932-binary+              96)
(defconstant +mysql-cs-coll-eucjpms-japanese-ci+       97)
(defconstant +mysql-cs-coll-eucjpms-binary+            98)
(defconstant +mysql-cs-coll-cp1250-polish-ci+          99)
;;; 100 is NOT USED
(defconstant +mysql-cs-coll-utf16-unicode-ci+          101)
(defconstant +mysql-cs-coll-utf16-icelandic-ci+        102)
(defconstant +mysql-cs-coll-utf16-latvian-ci+          103)
(defconstant +mysql-cs-coll-utf16-romanian-ci+         104)
(defconstant +mysql-cs-coll-utf16-slovenian-ci+        105)
(defconstant +mysql-cs-coll-utf16-polish-ci+           106)
(defconstant +mysql-cs-coll-utf16-estonian-ci+         107)
(defconstant +mysql-cs-coll-utf16-spanish-ci+          108)
(defconstant +mysql-cs-coll-utf16-swedish-ci+          109)
(defconstant +mysql-cs-coll-utf16-turkish-ci+          110)
(defconstant +mysql-cs-coll-utf16-czech-ci+            111)
(defconstant +mysql-cs-coll-utf16-danish-ci+           112)
(defconstant +mysql-cs-coll-utf16-lithuanian-ci+       113)
(defconstant +mysql-cs-coll-utf16-slovak-ci+           114)
(defconstant +mysql-cs-coll-utf16-spanish2-ci+         115)
(defconstant +mysql-cs-coll-utf16-roman-ci+            116)
(defconstant +mysql-cs-coll-utf16-persian-ci+          117)
(defconstant +mysql-cs-coll-utf16-esperanto-ci+        118)
(defconstant +mysql-cs-coll-utf16-hungarian-ci+        119)
(defconstant +mysql-cs-coll-utf16-sinhala-ci+          120)
(defconstant +mysql-cs-coll-utf16-german2-ci+          121)
(defconstant +mysql-cs-coll-utf16-croatian-ci+         122)
(defconstant +mysql-cs-coll-utf16-unicode-520-ci+      123)
(defconstant +mysql-cs-coll-utf16-vietnamese-ci+       124)
;;; 125 is NOT USED
;;; 126 is NOT USED
;;; 127 is NOT USED
(defconstant +mysql-cs-coll-ucs2-unicode-ci+           128)
(defconstant +mysql-cs-coll-ucs2-icelandic-ci+         129)
(defconstant +mysql-cs-coll-ucs2-latvian-ci+           130)
(defconstant +mysql-cs-coll-ucs2-romanian-ci+          131)
(defconstant +mysql-cs-coll-ucs2-slovenian-ci+         132)
(defconstant +mysql-cs-coll-ucs2-polish-ci+            133)
(defconstant +mysql-cs-coll-ucs2-estonian-ci+          134)
(defconstant +mysql-cs-coll-ucs2-spanish-ci+           135)
(defconstant +mysql-cs-coll-ucs2-swedish-ci+           136)
(defconstant +mysql-cs-coll-ucs2-turkish-ci+           137)
(defconstant +mysql-cs-coll-ucs2-czech-ci+             138)
(defconstant +mysql-cs-coll-ucs2-danish-ci+            139)
(defconstant +mysql-cs-coll-ucs2-lithuanian-ci+        140)
(defconstant +mysql-cs-coll-ucs2-slovak-ci+            141)
(defconstant +mysql-cs-coll-ucs2-spanish2-ci+          142)
(defconstant +mysql-cs-coll-ucs2-roman-ci+             143)
(defconstant +mysql-cs-coll-ucs2-persian-ci+           144)
(defconstant +mysql-cs-coll-ucs2-esperanto-ci+         145)
(defconstant +mysql-cs-coll-ucs2-hungarian-ci+         146)
(defconstant +mysql-cs-coll-ucs2-sinhala-ci+           147)
(defconstant +mysql-cs-coll-ucs2-german2-ci+           148)
(defconstant +mysql-cs-coll-ucs2-croatian-ci+          149)
(defconstant +mysql-cs-coll-ucs2-unicode-520-ci+       150)
(defconstant +mysql-cs-coll-ucs2-vietnamese-ci+        151)
;;; 152 is NOT USED
;;; 153 is NOT USED
;;; 154 is NOT USED
;;; 155 is NOT USED
;;; 156 is NOT USED
;;; 157 is NOT USED
;;; 158 is NOT USED
(defconstant +mysql-cs-coll-ucs2-general-mysql500-ci+  159)
(defconstant +mysql-cs-coll-utf32-unicode-ci+          160)
(defconstant +mysql-cs-coll-utf32-icelandic-ci+        161)
(defconstant +mysql-cs-coll-utf32-latvian-ci+          162)
(defconstant +mysql-cs-coll-utf32-romanian-ci+         163)
(defconstant +mysql-cs-coll-utf32-slovenian-ci+        164)
(defconstant +mysql-cs-coll-utf32-polish-ci+           165)
(defconstant +mysql-cs-coll-utf32-estonian-ci+         166)
(defconstant +mysql-cs-coll-utf32-spanish-ci+          167)
(defconstant +mysql-cs-coll-utf32-swedish-ci+          168)
(defconstant +mysql-cs-coll-utf32-turkish-ci+          169)
(defconstant +mysql-cs-coll-utf32-czech-ci+            170)
(defconstant +mysql-cs-coll-utf32-danish-ci+           171)
(defconstant +mysql-cs-coll-utf32-lithuanian-ci+       172)
(defconstant +mysql-cs-coll-utf32-slovak-ci+           173)
(defconstant +mysql-cs-coll-utf32-spanish2-ci+         174)
(defconstant +mysql-cs-coll-utf32-roman-ci+            175)
(defconstant +mysql-cs-coll-utf32-persian-ci+          176)
(defconstant +mysql-cs-coll-utf32-esperanto-ci+        177)
(defconstant +mysql-cs-coll-utf32-hungarian-ci+        178)
(defconstant +mysql-cs-coll-utf32-sinhala-ci+          179)
(defconstant +mysql-cs-coll-utf32-german2-ci+          180)
(defconstant +mysql-cs-coll-utf32-croatian-ci+         181)
(defconstant +mysql-cs-coll-utf32-unicode-520-ci+      182)
(defconstant +mysql-cs-coll-utf32-vietnamese-ci+       183)
;;; 184 is NOT USED
;;; 185 is NOT USED
;;; 186 is NOT USED
;;; 187 is NOT USED
;;; 188 is NOT USED
;;; 189 is NOT USED
;;; 190 is NOT USED
;;; 191 is NOT USED
(defconstant +mysql-cs-coll-utf8mb3-unicode-ci+           192)
(defconstant +mysql-cs-coll-utf8mb3-icelandic-ci+         193)
(defconstant +mysql-cs-coll-utf8mb3-latvian-ci+           194)
(defconstant +mysql-cs-coll-utf8mb3-romanian-ci+          195)
(defconstant +mysql-cs-coll-utf8mb3-slovenian-ci+         196)
(defconstant +mysql-cs-coll-utf8mb3-polish-ci+            197)
(defconstant +mysql-cs-coll-utf8mb3-estonian-ci+          198)
(defconstant +mysql-cs-coll-utf8mb3-spanish-ci+           199)
(defconstant +mysql-cs-coll-utf8mb3-swedish-ci+           200)
(defconstant +mysql-cs-coll-utf8mb3-turkish-ci+           201)
(defconstant +mysql-cs-coll-utf8mb3-czech-ci+             202)
(defconstant +mysql-cs-coll-utf8mb3-danish-ci+            203)
(defconstant +mysql-cs-coll-utf8mb3-lithuanian-ci+        204)
(defconstant +mysql-cs-coll-utf8mb3-slovak-ci+            205)
(defconstant +mysql-cs-coll-utf8mb3-spanish2-ci+          206)
(defconstant +mysql-cs-coll-utf8mb3-roman-ci+             207)
(defconstant +mysql-cs-coll-utf8mb3-persian-ci+           208)
(defconstant +mysql-cs-coll-utf8mb3-esperanto-ci+         209)
(defconstant +mysql-cs-coll-utf8mb3-hungarian-ci+         210)
(defconstant +mysql-cs-coll-utf8mb3-sinhala-ci+           211)
(defconstant +mysql-cs-coll-utf8mb3-german2-ci+           212)
(defconstant +mysql-cs-coll-utf8mb3-croatian-ci+          213)
(defconstant +mysql-cs-coll-utf8mb3-unicode-520-ci+       214)
(defconstant +mysql-cs-coll-utf8mb3-vietnamese-ci+        215)
;;; 216 is NOT USED
;;; 217 is NOT USED
;;; 218 is NOT USED
;;; 219 is NOT USED
;;; 220 is NOT USED
;;; 221 is NOT USED
;;; 222 is NOT USED
(defconstant +mysql-cs-coll-utf8mb3-general-mysql500-ci+  223)
(defconstant +mysql-cs-coll-utf8mb4-unicode-ci+        224)
(defconstant +mysql-cs-coll-utf8mb4-icelandic-ci+      225)
(defconstant +mysql-cs-coll-utf8mb4-latvian-ci+        226)
(defconstant +mysql-cs-coll-utf8mb4-romanian-ci+       227)
(defconstant +mysql-cs-coll-utf8mb4-slovenian-ci+      228)
(defconstant +mysql-cs-coll-utf8mb4-polish-ci+         229)
(defconstant +mysql-cs-coll-utf8mb4-estonian-ci+       230)
(defconstant +mysql-cs-coll-utf8mb4-spanish-ci+        231)
(defconstant +mysql-cs-coll-utf8mb4-swedish-ci+        232)
(defconstant +mysql-cs-coll-utf8mb4-turkish-ci+        233)
(defconstant +mysql-cs-coll-utf8mb4-czech-ci+          234)
(defconstant +mysql-cs-coll-utf8mb4-danish-ci+         235)
(defconstant +mysql-cs-coll-utf8mb4-lithuanian-ci+     236)
(defconstant +mysql-cs-coll-utf8mb4-slovak-ci+         237)
(defconstant +mysql-cs-coll-utf8mb4-spanish2-ci+       238)
(defconstant +mysql-cs-coll-utf8mb4-roman-ci+          239)
(defconstant +mysql-cs-coll-utf8mb4-persian-ci+        240)
(defconstant +mysql-cs-coll-utf8mb4-esperanto-ci+      241)
(defconstant +mysql-cs-coll-utf8mb4-hungarian-ci+      242)
(defconstant +mysql-cs-coll-utf8mb4-sinhala-ci+        243)
(defconstant +mysql-cs-coll-utf8mb4-german2-ci+        244)
(defconstant +mysql-cs-coll-utf8mb4-croatian-ci+       245)
(defconstant +mysql-cs-coll-utf8mb4-unicode-520-ci+    246)
(defconstant +mysql-cs-coll-utf8mb4-vietnamese-ci+     247)
(defconstant +mysql-cs-coll-gb18030-chinese-ci+        248)
(defconstant +mysql-cs-coll-gb18030-bin+               249)
(defconstant +mysql-cs-coll-gb18030-unicode-520-ci+    250)
;;; 251 is NOT USED
;;; 252 is NOT USED
;;; 253 is NOT USED
(defconstant +mysql-cs-coll-utf8mb3-general-cs+         254) ; removed since 5.8, was experimental
(defconstant +mysql-cs-coll-utf8mb4-0900-ai-ci+         255)
(defconstant +mysql-cs-coll-utf8mb4-de-pb-0900-ai-ci+   256)
(defconstant +mysql-cs-coll-utf8mb4-is-0900-ai-ci+      257)
(defconstant +mysql-cs-coll-utf8mb4-lv-0900-ai-ci+      258)
(defconstant +mysql-cs-coll-utf8mb4-ro-0900-ai-ci+      259)
(defconstant +mysql-cs-coll-utf8mb4-sl-0900-ai-ci+      260)
(defconstant +mysql-cs-coll-utf8mb4-pl-0900-ai-ci+      261)
(defconstant +mysql-cs-coll-utf8mb4-et-0900-ai-ci+      262)
(defconstant +mysql-cs-coll-utf8mb4-es-0900-ai-ci+      263)
(defconstant +mysql-cs-coll-utf8mb4-sv-0900-ai-ci+      264)
(defconstant +mysql-cs-coll-utf8mb4-tr-0900-ai-ci+      265)
(defconstant +mysql-cs-coll-utf8mb4-cs-0900-ai-ci+      266)
(defconstant +mysql-cs-coll-utf8mb4-da-0900-ai-ci+      267)
(defconstant +mysql-cs-coll-utf8mb4-lt-0900-ai-ci+      268)
(defconstant +mysql-cs-coll-utf8mb4-sk-0900-ai-ci+      269)
(defconstant +mysql-cs-coll-utf8mb4-es-trad-0900-ai-ci+ 270)
(defconstant +mysql-cs-coll-utf8mb4-la-0900-ai-ci+      271)
;;; 272 is NOT USED
(defconstant +mysql-cs-coll-utf8mb4-eo-0900-ai-ci+      273)
(defconstant +mysql-cs-coll-utf8mb4-hu-0900-ai-ci+      274)
(defconstant +mysql-cs-coll-utf8mb4-hr-0900-ai-ci+      275)
;;; 276 is NOT USED
(defconstant +mysql-cs-coll-utf8mb4-vi-0900-ai-ci+      277)
(defconstant +mysql-cs-coll-utf8mb4-0900-as-cs+         278)
(defconstant +mysql-cs-coll-utf8mb4-de-pb-0900-as-cs+   279)
(defconstant +mysql-cs-coll-utf8mb4-is-0900-as-cs+      280)
(defconstant +mysql-cs-coll-utf8mb4-lv-0900-as-cs+      281)
(defconstant +mysql-cs-coll-utf8mb4-ro-0900-as-cs+      282)
(defconstant +mysql-cs-coll-utf8mb4-sl-0900-as-cs+      283)
(defconstant +mysql-cs-coll-utf8mb4-pl-0900-as-cs+      284)
(defconstant +mysql-cs-coll-utf8mb4-et-0900-as-cs+      285)
(defconstant +mysql-cs-coll-utf8mb4-es-0900-as-cs+      286)
(defconstant +mysql-cs-coll-utf8mb4-sv-0900-as-cs+      287)
(defconstant +mysql-cs-coll-utf8mb4-tr-0900-as-cs+      288)
(defconstant +mysql-cs-coll-utf8mb4-cs-0900-as-cs+      289)
(defconstant +mysql-cs-coll-utf8mb4-da-0900-as-cs+      290)
(defconstant +mysql-cs-coll-utf8mb4-lt-0900-as-cs+      291)
(defconstant +mysql-cs-coll-utf8mb4-sk-0900-as-cs+      292)
(defconstant +mysql-cs-coll-utf8mb4-es-trad-0900-as-cs+ 293)
(defconstant +mysql-cs-coll-utf8mb4-la-0900-as-cs+      294)
;;; 295 is NOT USED
(defconstant +mysql-cs-coll-utf8mb4-eo-0900-as-cs+      296)
(defconstant +mysql-cs-coll-utf8mb4-hu-0900-as-cs+      297)
(defconstant +mysql-cs-coll-utf8mb4-hr-0900-as-cs+      298)
;;; 299 is NOT USED
(defconstant +mysql-cs-coll-utf8mb4-vi-0900-as-cs+      300)
;;; 301 is NOT USED
;;; 302 is NOT USED
(defconstant +mysql-cs-coll-utf8mb4-ja-0900-as-cs+      303)
(defconstant +mysql-cs-coll-utf8mb4-ja-0900-as-cs-ks+   304)
(defconstant +mysql-cs-coll-utf8mb4-0900-as-ci+         305)
(defconstant +mysql-cs-coll-utf8mb4-ru-0900-ai-ci+      306)
(defconstant +mysql-cs-coll-utf8mb4-ru-0900-as-cs+      307)
(defconstant +mysql-cs-coll-utf8mb4-zh-0900-as-cs+      308)
(defconstant +mysql-cs-coll-utf8mb4-0900-bin+           309)
(defconstant +mysql-cs-coll-utf8mb4-nb-0900-ai-ci+      310)
(defconstant +mysql-cs-coll-utf8mb4-nb-0900-as-cs+      311)
(defconstant +mysql-cs-coll-utf8mb4-nn-0900-ai-ci+      312)
(defconstant +mysql-cs-coll-utf8mb4-nn-0900-as-cs+      313)
(defconstant +mysql-cs-coll-utf8mb4-sr-latn-0900-ai-ci+ 314)
(defconstant +mysql-cs-coll-utf8mb4-sr-latn-0900-as-cs+ 315)
(defconstant +mysql-cs-coll-utf8mb4-bs-0900-ai-ci+      316)
(defconstant +mysql-cs-coll-utf8mb4-bs-0900-as-cs+      317)
(defconstant +mysql-cs-coll-utf8mb4-bg-0900-ai-ci+      318)
(defconstant +mysql-cs-coll-utf8mb4-bg-0900-as-cs+      319)
(defconstant +mysql-cs-coll-utf8mb4-gl-0900-ai-ci+      320)
(defconstant +mysql-cs-coll-utf8mb4-gl-0900-as-cs+      321)
(defconstant +mysql-cs-coll-utf8mb4-mn-cyrl-0900-ai-ci+ 322)
(defconstant +mysql-cs-coll-utf8mb4-mn-cyrl-0900-as-cs+ 323)
)            ;eval-when
