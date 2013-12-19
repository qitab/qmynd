;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                  ;;;
;;; Free Software published under an MIT-like license. See LICENSE   ;;;
;;;                                                                  ;;;
;;; Copyright (c) 2012 Google, Inc.  All rights reserved.            ;;;
;;;                                                                  ;;;
;;; Original author: Alejandro Sedeño                                ;;;
;;;                                                                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :qmynd-impl)

;;; Stuff in here should find a more permanent home as the library
;;; evolves.

(defun mysql-cs-coll-to-character-encoding (cs-coll)
  "Maps a MySQL Character Set / Collation identifier to an encoding."
  (ecase cs-coll
    ;; (#. +mysql-cs-coll-big5-chinese-ci+        :unknown)
    (#. +mysql-cs-coll-latin2-czech-cs+          :iso-8859-2)
    (#. +mysql-cs-coll-dec8-swedish-ci+          :iso-8859-1) ; punting
    (#. +mysql-cs-coll-cp850-general-ci+         :iso-8859-1) ; punting
    (#. +mysql-cs-coll-latin1-german1-ci+        :iso-8859-1)
    (#. +mysql-cs-coll-hp8-english-ci+           :iso-8859-1) ; punting
    ;; (#. +mysql-cs-coll-koi8r-general-ci+       :unknown)
    (#. +mysql-cs-coll-latin1-swedish-ci+        :iso-8859-1)
    (#. +mysql-cs-coll-latin2-general-ci+        :iso-8859-2)
    (#. +mysql-cs-coll-swe7-swedish-ci+          :iso-8859-2)
    (#. +mysql-cs-coll-ascii-general-ci+         :us-ascii)
    ;; (#. +mysql-cs-coll-ujis-japanese-ci+      :unknown)
    ;; (#. +mysql-cs-coll-sjis-japanese-ci+      :unknown)
    (#. +mysql-cs-coll-cp1251-bulgarian-ci+      :cp1251)
    (#. +mysql-cs-coll-latin1-danish-ci+         :iso-8859-1)
    ;; (#. +mysql-cs-coll-hebrew-general-ci+      :unknown)
    (#. +mysql-cs-coll-win1251+                  :cp1251)
    ;; (#. +mysql-cs-coll-tis620-thai-ci+        :unknown)
    ;; (#. +mysql-cs-coll-euckr-korean-ci+        :unknown)
    (#. +mysql-cs-coll-latin7-estonian-cs+       :iso-8859-7)
    (#. +mysql-cs-coll-latin2-hungarian-ci+      :iso-8859-2)
    ;; (#. +mysql-cs-coll-koi8u-general-ci+       :unknown)
    (#. +mysql-cs-coll-cp1251-ukrainian-ci+      :cp1251)
    ;; (#. +mysql-cs-coll-gb2312-chinese-ci+      :unknown)
    ;; (#. +mysql-cs-coll-greek-general-ci+       :unknown)
    ;; (#. +mysql-cs-coll-cp1250-general-ci+      :unknown)
    (#. +mysql-cs-coll-latin2-croatian-ci+       :iso-8859-2)
    (#. +mysql-cs-coll-gbk-chinese-ci+           :gbk)
    ;; (#. +mysql-cs-coll-cp1257-lithuanian-ci+   :unknown)
    (#. +mysql-cs-coll-latin5-turkish-ci+        :iso-8859-5)
    (#. +mysql-cs-coll-latin1-german2-ci+        :iso-8859-2)
    (#. +mysql-cs-coll-armscii8-general-ci+      :iso-8859-1)
    (#. +mysql-cs-coll-utf8-general-ci+          :utf-8)
    ;; (#. +mysql-cs-coll-cp1250-general-ci+      :unknown)
    (#. +mysql-cs-coll-ucs2-general-ci+          :ucs-2)
    ;; (#. +mysql-cs-coll-cp866-general-ci+       :unknown)
    ;; (#. +mysql-cs-coll-keybcs2-general-ci+     :unknown)
    ;; (#. +mysql-cs-coll-macce-general-ci+       :unknown)
    ;; (#. +mysql-cs-coll-macroman-general-ci+    :unknown)
    ;; (#. +mysql-cs-coll-cp852-general-ci+       :unknown)
    (#. +mysql-cs-coll-latin7-general-ci+        :iso-8859-13) ; latvian
    (#. +mysql-cs-coll-latin7-general-cs+        :iso-8859-13) ; latvian1
    ;; (#. +mysql-cs-coll-macce-binary+           :unknown)
    ;; (#. +mysql-cs-coll-cp1250-croatian-ci+     :unknown)
    (#. +mysql-cs-coll-utf8mb4-general-ci+       :utf-8)
    (#. +mysql-cs-coll-utf8mb4-binary+           :utf-8)
    (#. +mysql-cs-coll-latin1-binary+            :iso-8859-1)
    (#. +mysql-cs-coll-latin1-general-ci+        :iso-8859-1)
    (#. +mysql-cs-coll-latin1-general-cs+        :iso-8859-1)
    (#. +mysql-cs-coll-cp1251-binary+            :cp1251)
    (#. +mysql-cs-coll-cp1251-general-ci+        :cp1251)
    (#. +mysql-cs-coll-cp1251-general-cs+        :cp1251)
    ;;; (#. +mysql-cs-coll-macroman-binary+       :unknown)
    (#. +mysql-cs-coll-utf16-general-ci+         :utf-16)
    (#. +mysql-cs-coll-utf16-binary+             :utf-16)
    (#. +mysql-cs-coll-utf16le-general-ci+       :utf-16le)
    ;; (#. +mysql-cs-coll-cp1256-general-ci+      :unknown)
    ;; (#. +mysql-cs-coll-cp1257-binary+          :unknown)
    ;; (#. +mysql-cs-coll-cp1257-general-ci+      :unknown)
    (#. +mysql-cs-coll-utf32-general-ci+         :utf-32)
    (#. +mysql-cs-coll-utf32-binary+             :utf-32)
    (#. +mysql-cs-coll-utf16le-binary+           :utf-16le)
    (#. +mysql-cs-coll-binary+ nil)
    (#. +mysql-cs-coll-armscii8-binary+          :iso-8859-2) ; punting armscii
    (#. +mysql-cs-coll-ascii-binary+             :us-ascii)
    ;; (#. +mysql-cs-coll-cp1250-binary+           :unknown)
    ;; (#. +mysql-cs-coll-cp1256-binary+           :unknown)
    ;; (#. +mysql-cs-coll-cp866-binary+            :unknown)
    (#. +mysql-cs-coll-dec8-binary+              :iso-8859-2) ; punting for dec8
    ;; (#. +mysql-cs-coll-greek-binary+            :unknown)
    ;; (#. +mysql-cs-coll-hebrew-binary+           :unknown)
    (#. +mysql-cs-coll-hp8-binary+               :us-ascii)
    ;; (#. +mysql-cs-coll-keybcs2-binary+          :unknown)
    ;; (#. +mysql-cs-coll-koi8r-binary+            :unknown)
    ;; (#. +mysql-cs-coll-koi8u-binary+            :unknown)
    (#. +mysql-cs-coll-latin2-binary+            :iso-8859-2)
    (#. +mysql-cs-coll-latin5-binary+            :iso-8859-5)
    (#. +mysql-cs-coll-latin7-binary+            :iso-8859-7)
    ;; (#. +mysql-cs-coll-cp850-binary+          :unknown)
    ;; (#. +mysql-cs-coll-cp852-binary+          :unknown)
    (#. +mysql-cs-coll-swe7-binary+              :iso-8859-1) ; punting for swe7
    (#. +mysql-cs-coll-utf8-binary+              :utf-8)
    ;; (#. +mysql-cs-coll-big5-binary+             :unknown)
    ;; (#. +mysql-cs-coll-euckr-binary+            :unknown)
    ;; (#. +mysql-cs-coll-gb2312-binary+           :unknown)
    (#. +mysql-cs-coll-gbk-binary+               :gbk)
    ;; (#. +mysql-cs-coll-sjis-binary+             :unknown)
    ;; (#. +mysql-cs-coll-tis620-binary+           :unknown)
    (#. +mysql-cs-coll-ucs2-binary+              :ucs-2)
    ;; (#. +mysql-cs-coll-ujis-binary+             :unknown)
    (#. +mysql-cs-coll-geostd8-general-ci+       :us-ascii) ; punting for geostd8
    (#. +mysql-cs-coll-geostd8-binary+           :us-ascii) ; punting for geostd8
    (#. +mysql-cs-coll-latin1-spanish-ci+        :iso-8859-1)
    (#. +mysql-cs-coll-cp932-japanese-ci+        :cp932)
    (#. +mysql-cs-coll-cp932-binary+             :cp932)
    (#. +mysql-cs-coll-eucjpms-japanese-ci+      :eucjp)
    (#. +mysql-cs-coll-eucjpms-binary+           :eucjp)
    ;; (#. +mysql-cs-coll-cp1250-polish-ci+        :unknown)
    (#. +mysql-cs-coll-utf16-unicode-ci+         :utf-16)
    (#. +mysql-cs-coll-utf16-icelandic-ci+       :utf-16)
    (#. +mysql-cs-coll-utf16-latvian-ci+         :utf-16)
    (#. +mysql-cs-coll-utf16-romanian-ci+        :utf-16)
    (#. +mysql-cs-coll-utf16-slovenian-ci+       :utf-16)
    (#. +mysql-cs-coll-utf16-polish-ci+          :utf-16)
    (#. +mysql-cs-coll-utf16-estonian-ci+        :utf-16)
    (#. +mysql-cs-coll-utf16-spanish-ci+         :utf-16)
    (#. +mysql-cs-coll-utf16-swedish-ci+         :utf-16)
    (#. +mysql-cs-coll-utf16-turkish-ci+         :utf-16)
    (#. +mysql-cs-coll-utf16-czech-ci+           :utf-16)
    (#. +mysql-cs-coll-utf16-danish-ci+          :utf-16)
    (#. +mysql-cs-coll-utf16-lithuanian-ci+      :utf-16)
    (#. +mysql-cs-coll-utf16-slovak-ci+          :utf-16)
    (#. +mysql-cs-coll-utf16-spanish2-ci+        :utf-16)
    (#. +mysql-cs-coll-utf16-roman-ci+           :utf-16)
    (#. +mysql-cs-coll-utf16-persian-ci+         :utf-16)
    (#. +mysql-cs-coll-utf16-esperanto-ci+       :utf-16)
    (#. +mysql-cs-coll-utf16-hungarian-ci+       :utf-16)
    (#. +mysql-cs-coll-utf16-sinhala-ci+         :utf-16)
    (#. +mysql-cs-coll-utf16-german2-ci+         :utf-16)
    (#. +mysql-cs-coll-utf16-croatian-ci+        :utf-16)
    (#. +mysql-cs-coll-utf16-unicode-520-ci+     :utf-16)
    (#. +mysql-cs-coll-utf16-vietnamese-ci+      :utf-16)
    (#. +mysql-cs-coll-ucs2-unicode-ci+          :ucs-2)
    (#. +mysql-cs-coll-ucs2-icelandic-ci+        :ucs-2)
    (#. +mysql-cs-coll-ucs2-latvian-ci+          :ucs-2)
    (#. +mysql-cs-coll-ucs2-romanian-ci+         :ucs-2)
    (#. +mysql-cs-coll-ucs2-slovenian-ci+        :ucs-2)
    (#. +mysql-cs-coll-ucs2-polish-ci+           :ucs-2)
    (#. +mysql-cs-coll-ucs2-estonian-ci+         :ucs-2)
    (#. +mysql-cs-coll-ucs2-spanish-ci+          :ucs-2)
    (#. +mysql-cs-coll-ucs2-swedish-ci+          :ucs-2)
    (#. +mysql-cs-coll-ucs2-turkish-ci+          :ucs-2)
    (#. +mysql-cs-coll-ucs2-czech-ci+            :ucs-2)
    (#. +mysql-cs-coll-ucs2-danish-ci+           :ucs-2)
    (#. +mysql-cs-coll-ucs2-lithuanian-ci+       :ucs-2)
    (#. +mysql-cs-coll-ucs2-slovak-ci+           :ucs-2)
    (#. +mysql-cs-coll-ucs2-spanish2-ci+         :ucs-2)
    (#. +mysql-cs-coll-ucs2-roman-ci+            :ucs-2)
    (#. +mysql-cs-coll-ucs2-persian-ci+          :ucs-2)
    (#. +mysql-cs-coll-ucs2-esperanto-ci+        :ucs-2)
    (#. +mysql-cs-coll-ucs2-hungarian-ci+        :ucs-2)
    (#. +mysql-cs-coll-ucs2-sinhala-ci+          :ucs-2)
    (#. +mysql-cs-coll-ucs2-german2-ci+          :ucs-2)
    (#. +mysql-cs-coll-ucs2-croatian-ci+         :ucs-2)
    (#. +mysql-cs-coll-ucs2-unicode-520-ci+      :ucs-2)
    (#. +mysql-cs-coll-ucs2-vietnamese-ci+       :ucs-2)
    (#. +mysql-cs-coll-ucs2-general-mysql500-ci+ :ucs-2)
    (#. +mysql-cs-coll-utf32-unicode-ci+         :utf-32)
    (#. +mysql-cs-coll-utf32-icelandic-ci+       :utf-32)
    (#. +mysql-cs-coll-utf32-latvian-ci+         :utf-32)
    (#. +mysql-cs-coll-utf32-romanian-ci+        :utf-32)
    (#. +mysql-cs-coll-utf32-slovenian-ci+       :utf-32)
    (#. +mysql-cs-coll-utf32-polish-ci+          :utf-32)
    (#. +mysql-cs-coll-utf32-estonian-ci+        :utf-32)
    (#. +mysql-cs-coll-utf32-spanish-ci+         :utf-32)
    (#. +mysql-cs-coll-utf32-swedish-ci+         :utf-32)
    (#. +mysql-cs-coll-utf32-turkish-ci+         :utf-32)
    (#. +mysql-cs-coll-utf32-czech-ci+           :utf-32)
    (#. +mysql-cs-coll-utf32-danish-ci+          :utf-32)
    (#. +mysql-cs-coll-utf32-lithuanian-ci+      :utf-32)
    (#. +mysql-cs-coll-utf32-slovak-ci+          :utf-32)
    (#. +mysql-cs-coll-utf32-spanish2-ci+        :utf-32)
    (#. +mysql-cs-coll-utf32-roman-ci+           :utf-32)
    (#. +mysql-cs-coll-utf32-persian-ci+         :utf-32)
    (#. +mysql-cs-coll-utf32-esperanto-ci+       :utf-32)
    (#. +mysql-cs-coll-utf32-hungarian-ci+       :utf-32)
    (#. +mysql-cs-coll-utf32-sinhala-ci+         :utf-32)
    (#. +mysql-cs-coll-utf32-german2-ci+         :utf-32)
    (#. +mysql-cs-coll-utf32-croatian-ci+        :utf-32)
    (#. +mysql-cs-coll-utf32-unicode-520-ci+     :utf-32)
    (#. +mysql-cs-coll-utf32-vietnamese-ci+      :utf-32)
    (#. +mysql-cs-coll-utf8-unicode-ci+          :utf-8)
    (#. +mysql-cs-coll-utf8-icelandic-ci+        :utf-8)
    (#. +mysql-cs-coll-utf8-latvian-ci+          :utf-8)
    (#. +mysql-cs-coll-utf8-romanian-ci+         :utf-8)
    (#. +mysql-cs-coll-utf8-slovenian-ci+        :utf-8)
    (#. +mysql-cs-coll-utf8-polish-ci+           :utf-8)
    (#. +mysql-cs-coll-utf8-estonian-ci+         :utf-8)
    (#. +mysql-cs-coll-utf8-spanish-ci+          :utf-8)
    (#. +mysql-cs-coll-utf8-swedish-ci+          :utf-8)
    (#. +mysql-cs-coll-utf8-turkish-ci+          :utf-8)
    (#. +mysql-cs-coll-utf8-czech-ci+            :utf-8)
    (#. +mysql-cs-coll-utf8-danish-ci+           :utf-8)
    (#. +mysql-cs-coll-utf8-lithuanian-ci+       :utf-8)
    (#. +mysql-cs-coll-utf8-slovak-ci+           :utf-8)
    (#. +mysql-cs-coll-utf8-spanish2-ci+         :utf-8)
    (#. +mysql-cs-coll-utf8-roman-ci+            :utf-8)
    (#. +mysql-cs-coll-utf8-persian-ci+          :utf-8)
    (#. +mysql-cs-coll-utf8-esperanto-ci+        :utf-8)
    (#. +mysql-cs-coll-utf8-hungarian-ci+        :utf-8)
    (#. +mysql-cs-coll-utf8-sinhala-ci+          :utf-8)
    (#. +mysql-cs-coll-utf8-german2-ci+          :utf-8)
    (#. +mysql-cs-coll-utf8-croatian-ci+         :utf-8)
    (#. +mysql-cs-coll-utf8-unicode-520-ci+      :utf-8)
    (#. +mysql-cs-coll-utf8-vietnamese-ci+       :utf-8)
    (#. +mysql-cs-coll-utf8-general-mysql500-ci+ :utf-8)
    (#. +mysql-cs-coll-utf8mb4-unicode-ci+       :utf-8) ; utf8mb4
    (#. +mysql-cs-coll-utf8mb4-icelandic-ci+     :utf-8) ; utf8mb4
    (#. +mysql-cs-coll-utf8mb4-latvian-ci+       :utf-8) ; utf8mb4
    (#. +mysql-cs-coll-utf8mb4-romanian-ci+      :utf-8) ; utf8mb4
    (#. +mysql-cs-coll-utf8mb4-slovenian-ci+     :utf-8) ; utf8mb4
    (#. +mysql-cs-coll-utf8mb4-polish-ci+        :utf-8) ; utf8mb4
    (#. +mysql-cs-coll-utf8mb4-estonian-ci+      :utf-8) ; utf8mb4
    (#. +mysql-cs-coll-utf8mb4-spanish-ci+       :utf-8) ; utf8mb4
    (#. +mysql-cs-coll-utf8mb4-swedish-ci+       :utf-8) ; utf8mb4
    (#. +mysql-cs-coll-utf8mb4-turkish-ci+       :utf-8) ; utf8mb4
    (#. +mysql-cs-coll-utf8mb4-czech-ci+         :utf-8) ; utf8mb4
    (#. +mysql-cs-coll-utf8mb4-danish-ci+        :utf-8) ; utf8mb4
    (#. +mysql-cs-coll-utf8mb4-lithuanian-ci+    :utf-8) ; utf8mb4
    (#. +mysql-cs-coll-utf8mb4-slovak-ci+        :utf-8) ; utf8mb4
    (#. +mysql-cs-coll-utf8mb4-spanish2-ci+      :utf-8) ; utf8mb4
    (#. +mysql-cs-coll-utf8mb4-roman-ci+         :utf-8) ; utf8mb4
    (#. +mysql-cs-coll-utf8mb4-persian-ci+       :utf-8) ; utf8mb4
    (#. +mysql-cs-coll-utf8mb4-esperanto-ci+     :utf-8) ; utf8mb4
    (#. +mysql-cs-coll-utf8mb4-hungarian-ci+     :utf-8) ; utf8mb4
    (#. +mysql-cs-coll-utf8mb4-sinhala-ci+       :utf-8) ; utf8mb4
    (#. +mysql-cs-coll-utf8mb4-german2-ci+       :utf-8) ; utf8mb4
    (#. +mysql-cs-coll-utf8mb4-croatian-ci+      :utf-8) ; utf8mb4
    (#. +mysql-cs-coll-utf8mb4-unicode-520-ci+   :utf-8) ; utf8mb4
    (#. +mysql-cs-coll-utf8mb4-vietnamese-ci+    :utf-8) ; utf8mb4
    (#. +mysql-cs-coll-utf8mb3-general-cs+       :utf8)
    ))
